/** \file motor.c
*
*  Library to manage motor command and state messages
*
*  \author Jon Scholz
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <somatic.h>
#include <ach.h>

#include <somatic/util.h>
#include <somatic.pb-c.h>

#include "somatic/msg/base.h"
#include "somatic/msg/motor.h"

/*
 * Allocate a Somatic__MotorCmd message
 */
Somatic__MotorCmd *somatic_motorcmd_alloc(size_t n_modules)
{
	Somatic__MotorCmd *msg = SOMATIC_NEW(Somatic__MotorCmd);
	somatic__motor_cmd__init(msg);

	msg->values = somatic_vector_alloc(n_modules);

	return (msg);
}

/*
 * Allocate a Somatic__MotorState message
 */
Somatic__MotorState *somatic_motorstate_alloc(size_t n_modules)
{
	Somatic__MotorState *msg = SOMATIC_NEW(Somatic__MotorState);
	somatic__motor_state__init(msg);

	msg->position = somatic_vector_alloc(n_modules);
	msg->velocity = somatic_vector_alloc(n_modules);

	return (msg);
}

/*
 * Free dynamically allocated memory from MotorCmd message
 */
int somatic_motorcmd_free(Somatic__MotorCmd *msg)
{
	somatic_vector_free(msg->values);
	free(msg);

	return(0);
}

/*
 * Free dynamically allocated memory from MotorState message
 */
int somatic_motorstate_free(Somatic__MotorState *msg)
{
	somatic_vector_free(msg->position);
	somatic_vector_free(msg->velocity);
	free(msg);

	return(0);
}

// Generate a motor command and publish on specified channel
int somatic_generate_motorcmd(ach_channel_t *chan, double *values, size_t n_modules, Somatic__MotorParam cmd_type)
{
	Somatic__MotorCmd msg;
	somatic__motor_cmd__init(&msg);

	msg.has_param = 1;
	msg.param = cmd_type;
	msg.values = SOMATIC_NEW(Somatic__Vector);
	somatic__vector__init(msg.values);
	msg.values->data = values;
	msg.values->n_data = n_modules;
	//					size_t size = somatic__motorcmd__get_packed_size(js_msg);
	return somatic_motorcmd_publish(&msg, chan);
}

// Issues a motor state query request
//void somatic_request_motor_state_update(ach_channel_t *cmd_chan, size_t n_modules)
//{
/*
 * TODO: implement this (requires an addition to the motor command message to
 * include whatever CAN message type that is an ack only.
 */
//}

// Publish command message on specified channel
int somatic_motorcmd_publish(Somatic__MotorCmd *msg, ach_channel_t *chan)
{
	/* The type parameter (2) is the PREFIX for the appropriate libsomatic
	 * protobuf functions, not the type itself
	 */
	int r = SOMATIC_PACK_SEND( chan, somatic__motor_cmd, msg );
	somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n", ach_result_to_string( r ) );

	return(r);
}

// Publish MotorState message on specified channel
int somatic_motorstate_publish(Somatic__MotorState *msg, ach_channel_t *chan)
{
	/* The type parameter (2) is the PREFIX for the appropriate libsomatic
	 * protobuf functions, not the type itself
	 */
	int r = SOMATIC_PACK_SEND( chan, somatic__motor_state, msg );
	somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n", ach_result_to_string( r ) );

	return(r);
}

/**
 * Define a receive function for motor command message type
 */
SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_motorcmd_receive,
							somatic__motor_cmd,
							Somatic__MotorCmd);

/**
 * Define a receive function for motor statemessage type
 */
SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_motorstate_receive,
							somatic__motor_state,
							Somatic__MotorState);

// Print the contents of a Somatic__MotorCmd message
void somatic_motorcmd_print(Somatic__MotorCmd *msg)
{
	fprintf(stdout, "Motor command ");
	if (msg->param == SOMATIC__MOTOR_PARAM__MOTOR_CURRENT)
		fprintf(stdout, "current mode \n");
	else if (msg->param == SOMATIC__MOTOR_PARAM__MOTOR_VELOCITY)
		fprintf(stdout, "velocity mode\n");
	else if (msg->param == SOMATIC__MOTOR_PARAM__MOTOR_POSITION)
		fprintf(stdout, "position mode\n");
	else
		fprintf(stdout, "unrecognized parameter \n");

	size_t i;
	fprintf(stdout, "Values: ");
	for (i=0; i < msg->values->n_data; ++i)
		if (i < msg->values->n_data - 1)
			fprintf(stdout, "% 1.2lf::", msg->values->data[i]);
		else
			fprintf(stdout, "% 1.2lf\n", msg->values->data[i]);
	fprintf(stdout, "\n");
}

// Print the contents of a Somatic__MotorState message
void somatic_motorstate_print(Somatic__MotorState *msg)
{
	fprintf(stdout, "Motor state ");
	size_t i;

	if (msg->position!= NULL) {
		fprintf(stdout, "Position: ");
		for (i = 0; i < msg->position->n_data; ++i)
			if (i < msg->position->n_data - 1)
				fprintf(stdout, "% 1.2lf::", msg->position->data[i]);
			else
				fprintf(stdout, "% 1.2lf\n", msg->position->data[i]);
	}

	if (msg->velocity != NULL) {
		fprintf(stdout, "Velocity: ");
		for (i=0; i<msg->velocity->n_data; ++i)
			if (i < msg->velocity->n_data - 1)
				fprintf(stdout, "% 1.2lf::", msg->velocity->data[i]);
			else
				fprintf(stdout, "% 1.2lf\n", msg->velocity->data[i]);
	}

	/*
	 * TODO: fix this motor status to be an array of enums
	 * it's dumb to have a single status for the entire pcio group
	 */
	if (msg->has_status) {
		if (msg->status == SOMATIC__MOTOR_STATUS__MOTOR_OK)
			fprintf(stdout, "Motor okay!\n");
		else if (msg->status == SOMATIC__MOTOR_STATUS__MOTOR_FAIL)
			fprintf(stdout, "Motor failure!\n");
	}
	fprintf(stdout, "\n");
}
