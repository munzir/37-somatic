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

//TODO put these #defines in daemons (put in headers so we can include them?)
#define JOYSTICK_CHANNEL_NAME "joystick-data"
#define JACH_NBUTTONS 10
#define JACH_NAXES 6

#define SPACENAV_CHANNEL_NAME "spacenav-data"
#define SPNAV_MOTION_MAX 512
#define SNACH_NBUTTONS 2
#define SNACH_NAXES 6


// Allocate a Somatic__Joystick message for spacenav data
int somatic_joystick_alloc(Somatic__Joystick *msg, size_t n_axes, size_t n_buttons);

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

// Receive a message from specified Ach channel
//TODO replace this with new somatic macro
//Somatic__Joystick* somatic_joystick_receive(ach_channel_t *chan);

// Print the contents of a Somatic__Joystick message
void somatic_joystick_print(Somatic__Joystick *msg);

#endif /* JS_SMM_H_ */
