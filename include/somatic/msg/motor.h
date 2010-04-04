/* -*- mode: C; c-basic-offset: 2  -*- */
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

#ifndef MOTOR_H_
#define MOTOR_H_

#include "somatic.h"
#include "somatic/util.h"
#include "somatic.pb-c.h"

/** \file motor.h
 *
 *  Library to manage motor command and state messages
 *
 *  \author Jon Scholz
 */


/* Memory Management Functions */

/// Allocate a Somatic__MotorCmd message
Somatic__MotorCmd *somatic_motorcmd_alloc(size_t n_modules);

/// Allocate a Somatic__MotorState message
Somatic__MotorState *somatic_motorstate_alloc(size_t n_modules);

/// Free memory allocated by somatic_motorcmd_alloc
int somatic_motorcmd_free(Somatic__MotorCmd *msg);

/// Free memory allocated by somatic_motorstate_alloc
int somatic_motorstate_free(Somatic__MotorState *msg);


/* Main motor message commands */

/**
 * Generate a motor command and publish on specified channel
 * \note pciod will update the state channel after issuing this command
 *
 * @param chan The channel to issue motor commands
 * @param values An array of motor values to write
 * @param n_modules The number of modules, and size of the values array
 * @param cmd_type The motor command being issued (current | velocity | position)
 *
 * \pre An initialized motor channel
 * \post Values are written through the ACH channel to the modules
 */
int somatic_generate_motorcmd(ach_channel_t *chan, double *values, size_t n_modules, Somatic__MotorParam cmd_type);

/**
 *  Issues a motor state query request (not implemented yet)
 *
 *  @param cmd_chan The channel to issue motor commands
 *  @param n_modules The number of modules
 */
//void somatic_request_motor_state_update(ach_channel_t *cmd_chan, size_t n_modules);


/* Message IO */

/// Publish command message on specified channel
int somatic_motorcmd_publish(Somatic__MotorCmd *msg, ach_channel_t *chan);

/// Publish state message on specified channel
int somatic_motorstate_publish(Somatic__MotorState *msg, ach_channel_t *chan);

/**
 * Declare a receive function for motor command message type
 */
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_motorcmd_receive,
							somatic__motor_cmd,
							Somatic__MotorCmd);

/**
 * Declare a receive function for motor statemessage type
 */
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_motorstate_receive,
							somatic__motor_state,
							Somatic__MotorState);

/// Print the contents of a Somatic__MotorCmd message
void somatic_motorcmd_print(Somatic__MotorCmd *msg);

/// Print the contents of a Somatic__MotorState message
void somatic_motorstate_print(Somatic__MotorState *msg);

#endif /* MOTOR_H_ */
