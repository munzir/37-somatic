/**
 * @file somatic_dump.cpp
 * @author Neil Dantam, Can Erdogan
 * @date June 28, 2013
 * @brief This executable prints out the data in the given channel with the proper format.
 */

#include <amino.h>
#include <argp.h>
#include "somatic.h"
#include <imud.h>
#include <unistd.h>

/* ******************************************************************************************** */
// Channel vairables
ach_channel_t sd_chan;
uint8_t *sd_achbuf;
size_t sd_n_achbuf = 1024;
size_t sd_frame_size;
size_t sd_indent = 0;

/* ******************************************************************************************** */
// Option Vars
const char *opt_chan_name = NULL;	///< The ach channel to get data from
const char *opt_msg_type = NULL; 	///< Message type to be used if the message does not have a type

/* ******************************************************************************************** */
static struct argp_option options[] = {
	{ "verbose", 'v', NULL, 0, "Causes verbose output" },
	{ "chan", 'c', "ach channel", 0, "ach channel to send data to" },
	{ "protobuf", 'p', "protobuf-msg", 0, "name of protobuf message [imu]" }, 
	{ NULL, 0, NULL, 0, NULL }
};

/* ******************************************************************************************** */
static int parse_opt( int key, char *arg, struct argp_state *state) {
	(void) state; // ignore unused parameter
	switch(key) {
		case 'v': somatic_opt_verbosity++; break;
		case 'c': opt_chan_name = strdup( arg ); break;
		case 'p': opt_msg_type = strdup( arg ); break;
		case 0: break;
	}
	return 0;
}

/// argp program version
const char *argp_program_version = "somatic_dump 0.0";

/// argp program doc line
static char doc[] = "dump somatic msg to stdout";

/// argp object
static struct argp argp = {options, parse_opt, NULL, doc, NULL, NULL, NULL };

/* ******************************************************************************************** */
/// Reads the data from the ach channel
void read_ach() {
	ach_status_t r = ach_get( &sd_chan, sd_achbuf, sd_n_achbuf, &sd_frame_size, NULL, ACH_O_WAIT);
	if( ACH_OVERFLOW == r ) {
		sd_n_achbuf = AA_MAX( sd_frame_size, 2*sd_n_achbuf );
		free( sd_achbuf );
		sd_achbuf = AA_NEW_AR(uint8_t,  sd_n_achbuf );
		read_ach();
		return;
	}
	aa_hard_assert( ACH_OK == r || ACH_MISSED_FRAME == r, "Error reading frame: %s\n",
		ach_result_to_string( r ) );
}

/* ******************************************************************************************** */
/// Initializes the buffer to read messages into, creates the ach channel and sets the interrupt
/// handler
void init() {

	// Set up the buffer
	sd_achbuf = AA_NEW_AR(uint8_t,  sd_n_achbuf );

	// Open the given channel 
	int r = ach_open( &sd_chan, opt_chan_name, NULL );
	aa_hard_assert( ACH_OK == r, "Couldn't open channel %s\n", opt_chan_name );
	r = ach_flush( &sd_chan );
	aa_hard_assert( ACH_OK == r, "Couldn't flush channel\n");

	// Set the interrupt handler
	somatic_sighandler_simple_install();
}

/* ******************************************************************************************** */
void indent() {
	char buf[2*sd_indent+1];
	memset( buf, ' ', 2*sd_indent );
	buf[2*sd_indent] = '\0';
	printf("%s", buf);

}

/* ******************************************************************************************** */
/// Prints a double vector
void dump_vector (Somatic__Vector *pb, const char *fmt) {
	for( size_t i = 0; pb && pb->data && i < pb->n_data; i++ ) {
		printf(fmt, pb->data[i]);
	}
}

/* ******************************************************************************************** */
/// Prints an integer vector
void dump_ivector (Somatic__Ivector *pb, const char *fmt) {
	for( size_t i = 0; i < pb->n_data; i++ ) {
		printf(fmt, pb->data[i]);
	}
}

/* ******************************************************************************************** */
/// Prints a transform
void dump_transform( Somatic__Transform *pb ) {

	// Print the translation
	indent();
	printf("[Transform] ");
	if( pb->translation ) {
		printf("\t%6.3f\t%6.3f\t%6.3f", pb->translation->data[0], pb->translation->data[1],
			pb->translation->data[2]);
	} 
	else printf("\t\t\t");

	// Print the rotation
	printf("\t|");
	if( pb->rotation ) {
		printf("\t%6.3f\t%6.3f\t%6.3f\t%6.3f",
			   pb->rotation->data[0],
			   pb->rotation->data[1],
			   pb->rotation->data[2],
			   pb->rotation->data[3] );
	}
	printf("\n");
}

/* ******************************************************************************************** */
/// Prints the time specification
void dump_timespec( Somatic__Timespec *pb, const char *name ) {
	indent();
	printf("[%s : Timespec]\t%lus\t%09dns\n",
		   name, pb->sec, pb->has_nsec ? pb->nsec : 0 );
}

/* ******************************************************************************************** */
/// Prints the metadata
void dump_metadata( Somatic__Metadata *pb ) {

	// Print the time and the duration 
	indent();
	printf("[Metadata] \n");
	sd_indent++;
	if(pb->time) dump_timespec(pb->time, "time");
	if(pb->until) dump_timespec(pb->until, "until");

	// Print the type if one exists
	if(pb->has_type) {
		indent();
		const char *c = "unknown";
		switch(pb->type) {
			case SOMATIC__MSG_TYPE__FORCE_MOMENT: c = "ForceMoment"; break;
			case SOMATIC__MSG_TYPE__MOTOR_CMD: c = "MotorCmd"; break;
			case SOMATIC__MSG_TYPE__MOTOR_STATE: c = "MotorState"; break;
			case SOMATIC__MSG_TYPE__TRANSFORM: c = "Transform"; break;
			case SOMATIC__MSG_TYPE__MULTI_TRANSFORM: c = "MultiTransform"; break;
			case SOMATIC__MSG_TYPE__POINT_CLOUD: c = "PointCloud"; break;
			case SOMATIC__MSG_TYPE__JOYSTICK: c = "Joystick"; break;
			case SOMATIC__MSG_TYPE__TOUCH: c = "Touch"; break;
			case SOMATIC__MSG_TYPE__MICROPHONE: c = "Microphone"; break;
			case SOMATIC__MSG_TYPE__BATTERY: c = "Battery"; break;
		}
		printf("[type] %s\n", c);
	}
	sd_indent--;
}

/* ******************************************************************************************** */
/// Prints multiple transforms
void dump_multi_transform( Somatic__MultiTransform *pb ) {

	// Print each transform
	indent();
	printf("[MultiTransform]\n");
	sd_indent++;
	if( pb->tf ) {
		for( size_t i = 0; i < pb->n_tf; i++ ) dump_transform(pb->tf[i]);
	}

	// Print the metadata
	if( pb->meta ) dump_metadata( pb->meta );
	sd_indent--;
}

/* ******************************************************************************************** */
/// Prints the force/torque data
void dump_force_moment( Somatic__ForceMoment *pb ) {

	// Print the title and the force values
	indent();
	printf("[ForceMoment]\n");
	sd_indent++;
	indent();
	printf("[force]");
	if(pb->force) dump_vector(pb->force, "\t%6.3f");

	// Print the moments 
	printf("\n");
	indent();
	printf("[moment]");
	if(pb->moment) dump_vector(pb->moment, "\t%6.3f");
	printf("\n");

	// Print the metadata
	if(pb->meta) dump_metadata( pb->meta );
	sd_indent--;

}

/* ******************************************************************************************** */
/// Prints the joystick data
void dump_joystick( Somatic__Joystick *pb ) {

	// Print the title and the axes values
	indent();
	printf("[Joystick]\n");
	sd_indent++;
	indent();
	printf("[axes]");
	dump_vector(pb->axes, "\t%6.3f");

	// Print the buttons
	printf("\n");
	indent();
	printf("[buttons]\t");
	dump_ivector(pb->buttons, "%d:");
	printf("\n");

	// Print the metadata
	if(pb->meta) dump_metadata( pb->meta );
	sd_indent--;
}

/* ******************************************************************************************** */
/// Print battery data
void dump_battery( Somatic__Battery *pb ) {

	// Print the title and the voltage values
	indent();
	printf("[Battery]\n");
	sd_indent++;
	indent();
	printf("[volt]");
	dump_vector(pb->voltage, "\t%5.3f");

	// Print the temperatures
	printf("\n");
	indent();
	printf("[temp]");
	dump_vector(pb->temp, "\t%5.2f");
	printf("\n");

	// Print the metadata
	if(pb->meta) dump_metadata( pb->meta );
	sd_indent--;
}

/* ******************************************************************************************** */
/// Print the motor state with position, velocity and current values
void dump_motor_state( Somatic__MotorState *pb ) {

	// Print the title and the position values
	indent();
	printf("[MotorState]\n");
	sd_indent++;
	if( pb->position ) {
		indent();
		printf("[position]");
		dump_vector(pb->position, "\t%6.3f");
		printf("\n");
	}

	// Print the velocities
	if( pb->velocity ) {
		indent();
		printf("[velocity]");
		dump_vector(pb->velocity, "\t%6.3f");
		printf("\n");
	}

	// Print the currents
	if( pb->current ) {
		indent();
		printf("[current]");
		dump_vector(pb->current, "\t%6.3f");
		printf("\n");
	}

	// Print the metadata
	if( pb->meta ) dump_metadata( pb->meta );
	sd_indent--;
}
 
/* ******************************************************************************************** */
/// Print the motor command
void dump_motor_cmd( Somatic__MotorCmd *pb ) {

	// Print the title and the parameter name
	indent();
	printf("[MotorCmd]\n");
	sd_indent++;
	indent();
	switch ( pb->param ) {
		case SOMATIC__MOTOR_PARAM__MOTOR_CURRENT: printf("[param]\tCURRENT\n"); break;
		case SOMATIC__MOTOR_PARAM__MOTOR_VELOCITY: printf("[param]\tVELOCITY\n"); break;
		case SOMATIC__MOTOR_PARAM__MOTOR_POSITION: printf("[param]\tPOSITION\n"); break;
		case SOMATIC__MOTOR_PARAM__MOTOR_HALT: printf("[param]\tHALT\n"); break;
		case SOMATIC__MOTOR_PARAM__MOTOR_RESET: printf("[param]\tRESET\n"); break;
		case SOMATIC__MOTOR_PARAM__MOTOR_DIGITAL_OUT: printf("[param]\tDIGITAL OUT\n"); break;
	}

	// Print the given values
	if ( pb->values ) {
		indent();
		printf("[values]");
		dump_vector(pb->values, "\t%6.3f");
		printf("\n");
	}
	sd_indent--;
}

/* ******************************************************************************************** */
/// Dumps the imu value after processing it with ssdmu library
void dump_imu (Somatic__Vector* vec) {

	// Compute the pitch from the value (as done in ssdmu_pitch function)
	static const double csr = -.7853981634;
	double newX = vec->data[0]*cos(csr) - vec->data[1]*sin(csr);
	double imu = atan2(newX, vec->data[2]);

	// Print it
	indent();
	printf("[Imu]\n");
	sd_indent++;
	printf("\t%6.3lf (rad) \t %6.3lf (deg)\n", imu, (imu / M_PI) * 180.0);
	sd_indent--;
}

/* ******************************************************************************************** */
/// Calls the dump function for a given message type
#define UNPACK_DUMP( type, alloc, buf, size ) \
	dump_ ## type( somatic__ ## type ## __unpack( alloc, size, buf ) );

/* ******************************************************************************************** */
/// The main thread that gets a message, casts it to the correct type and prints it
void run() {

	somatic_pbregalloc_t alloc;
	somatic_pbregalloc_init(&alloc, 4096);

	while( !somatic_sig_received ) {

		// Read the message into a buffer
		read_ach();

		// Get the base message from the buffer
		Somatic__BaseMsg *base =
			somatic__base_msg__unpack( &alloc, sd_frame_size, sd_achbuf);

		// Convert the message to the proper type if it has one
		if( base->meta && base->meta->has_type ) {
			switch ( base->meta->type ) {
				case SOMATIC__MSG_TYPE__FORCE_MOMENT:
					UNPACK_DUMP( force_moment, &alloc, sd_achbuf, sd_frame_size ); break;
				case SOMATIC__MSG_TYPE__MULTI_TRANSFORM:
					UNPACK_DUMP( multi_transform, &alloc, sd_achbuf, sd_frame_size ); break;
				case SOMATIC__MSG_TYPE__JOYSTICK:
					UNPACK_DUMP( joystick, &alloc, sd_achbuf, sd_frame_size ); break;
				case SOMATIC__MSG_TYPE__MOTOR_CMD:
					UNPACK_DUMP( motor_cmd, &alloc, sd_achbuf, sd_frame_size ); break;
				case SOMATIC__MSG_TYPE__MOTOR_STATE:
					UNPACK_DUMP( motor_state, &alloc, sd_achbuf, sd_frame_size ); break;
				case SOMATIC__MSG_TYPE__BATTERY:
					UNPACK_DUMP( battery, &alloc, sd_achbuf, sd_frame_size ); break;
				default: printf("Unknown Message: %d\n",base->meta->type);
			}
		} 

		// If not, if a type is provided use it (such as vector for imu values)
		else if(opt_msg_type != NULL) {

			// If it is a vector, dump it as a vector
			if(strcmp(opt_msg_type, "imu") == 0.0) {
				dump_imu(somatic__vector__unpack(&alloc, sd_frame_size, sd_achbuf));
			}
		
		}

		// If no information is available, just give up
		else 
			printf("Unknown Message, no type info\n");
		
		// Free the message memory
		assert( 0 == sd_indent );
		somatic_pbregalloc_release(&alloc);

		// Sleep a bit
		usleep(1e4);
	}

	// Free the daemon memory (?)
	somatic_pbregalloc_destroy(&alloc);
}

/* ******************************************************************************************** */
/// Closes the ach channel
void destroy() {
	ach_close(&sd_chan);
}

/* ******************************************************************************************** */
/// The main function
int main( int argc, char **argv ) {

	// Parse options
	argp_parse (&argp, argc, argv, 0, NULL, NULL);
	somatic_verbprintf_prefix="somatic_dump";
	aa_hard_assert( NULL != opt_chan_name, "Must set channel name\n");

	// Initialize the ach channel and the buffer
	init();

	// Print 
	somatic_verbprintf( 1, "Channel %s\n", opt_chan_name );
	somatic_verbprintf( 1, "Protobuf %s\n", opt_msg_type );

	// Continuously check for new messages
	run();

	// Clean up
	destroy();
	return 0;
}
/* ******************************************************************************************** */
