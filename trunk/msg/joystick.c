/*
 * js_smm.c
 *
 *  Created on: Mar 29, 2010
 *      Author: jscholz
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <ach.h>

#include "somatic/msg/base.h"
#include "somatic/msg/joystick.h"


/**
 * Allocate a Somatic__Joystick message
 */
// TODO: replace with version that takes only size params and returns pointer to entire malloced message?
Somatic__Joystick *somatic_joystick_alloc(size_t n_axes, size_t n_buttons)
{
	Somatic__Joystick *msg = SOMATIC_NEW(Somatic__Joystick);
	somatic__joystick__init(msg);

	msg->axes = somatic_vector_alloc(n_axes);
	msg->buttons = somatic_ivector_alloc(n_buttons);

	return (msg);
}

/**
 * Free memory allocated to Somatic__Joystick msg
 */
int somatic_joystick_free(Somatic__Joystick *msg)
{
	somatic_vector_free(msg->axes);
	somatic_ivector_free(msg->buttons);
	free(msg);

	return(0);
}

/**
 * Publish a Somatic__Joystick message on specified channel
 */
int somatic_joystick_publish(Somatic__Joystick *msg, ach_channel_t *chan)
{
	int r = SOMATIC_PACK_SEND( chan, somatic__joystick, msg );
	somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n", ach_result_to_string( r ) );

	return(r);
}

/**
 * Defines a receive function for joystick message type
 */
SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_joystick_receive,
							somatic__joystick,
							Somatic__Joystick);

/**
 * Print the contents of a Somatic__Joystick message
 */
void somatic_joystick_print(Somatic__Joystick *msg){
	size_t i;
	for (i=0; i<msg->axes->n_data; ++i)
		fprintf(stdout, "% 1.2lf::", msg->axes->data[i]);
	fprintf(stdout, "[");
	for (i=0; i<msg->buttons->n_data; ++i)
		fprintf(stdout, "%lld::", msg->buttons->data[i]);
	fprintf(stdout, "]\n");
}
