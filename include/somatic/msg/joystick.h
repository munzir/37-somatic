/*
 * Copyright (c) 2009-2010, Georgia Tech Research Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *     * Redistributions of source code must retain the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *     * Neither the name of the Georgia Tech Research Corporation nor
 *       the names of its contributors may be used to endorse or
 *       promote products derived from this software without specific
 *       prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY GEORGIA TECH RESEARCH CORPORATION ''AS
 * IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL GEORGIA
 * TECH RESEARCH CORPORATION BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/**
 * \file joystick.h
 *
 *  Created on: March 29, 2010
 *  \brief functions to manage joystick messages
 *
 *  \author Jon Scholz
 */

#ifndef JOYSTICK_H_
#define JOYSTICK_H_

#include "somatic.h"
#include "somatic/util.h"
#include "somatic.pb-c.h"

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus

// Allocate a Somatic__Joystick message for spacenav data
Somatic__Joystick *somatic_joystick_alloc(size_t n_axes, size_t n_buttons) AA_DEPRECATED;

// Free memory allocated by somatic_joystick_alloc
int somatic_joystick_free(Somatic__Joystick *msg) AA_DEPRECATED;

// Publish message on Ach channel
int somatic_joystick_publish(Somatic__Joystick *msg, ach_channel_t *chan) AA_DEPRECATED;


/**
 * Declare a receive function for joystick message type
 */
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_joystick_receive,
							somatic__joystick,
							Somatic__Joystick);


// Print the contents of a Somatic__Joystick message
void somatic_joystick_print(Somatic__Joystick *msg) AA_DEPRECATED;

#ifdef __cplusplus
}
#endif //__cplusplus

#endif /* JOYSTICK_H_ */
