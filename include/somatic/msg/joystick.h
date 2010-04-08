/*
 * js_smm.h
 *
 * This is an smm (somatic message management) header for joystick
 * messages.
 *
 *  Created on: Mar 29, 2010
 *      Author: jscholz
 */

#ifndef JS_SMM_H_
#define JS_SMM_H_

#include "somatic.h"
#include "somatic/util.h"
#include "somatic.pb-c.h"

// Allocate a Somatic__Joystick message for spacenav data
Somatic__Joystick *somatic_joystick_alloc(size_t n_axes, size_t n_buttons);

// Free memory allocated by somatic_joystick_alloc
int somatic_joystick_free(Somatic__Joystick *msg);

// Publish message on Ach channel
int somatic_joystick_publish(Somatic__Joystick *msg, ach_channel_t *chan);


/**
 * Declare a receive function for joystick message type
 */
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_joystick_receive,
							somatic__joystick,
							Somatic__Joystick);


// Print the contents of a Somatic__Joystick message
void somatic_joystick_print(Somatic__Joystick *msg);

#endif /* JS_SMM_H_ */
