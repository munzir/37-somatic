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

/*
 * sdhdsa.h
 *
 *  Created on: Apr 2, 2010
 *      Author: Hyun-Soo Yi
 */

#ifndef DSA_SMM_H_
#define DSA_SMM_H_

#include "somatic.h"
#include "somatic/util.h"
#include "somatic.pb-c.h"

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus

//==================================================
//definitions
typedef void* DSApointer;


//==================================================
//function prototypes

/*
 * Allocate a Somatic__Joystick message
 */
Somatic__Dsa *somatic_dsa_alloc(size_t n_data);

/**
 * Free memory allocated to Somatic__Joystick msg
 */
int somatic_dsa_free(Somatic__Dsa *msg);

/**
 * Publish a Somatic__Joystick message on specified channel
 */
int somatic_dsa_publish(Somatic__Dsa *msg, ach_channel_t *chan);

/*
 * Defines a receive function
 */
SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_dsa_receive,
							somatic__dsa,
							Somatic__Dsa);

/*
 * Print the contents of a Somatic__Dsa message
 */
void somatic_dsa_print(Somatic__Dsa *msg);


/*
 * Decode dsa msg to a 6 x 16 x 6 array.
 */
void somatic_dsa_decode(Somatic__Dsa *msg, int64_t output[6][14][6]);




#ifdef __cplusplus
}
#endif //__cplusplus

#endif /* SDHDSA_H_ */
