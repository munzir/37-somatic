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
 * \file base.h
 *
 *  Created on: Apr 2, 2010
 * \author jscholz
 */

#ifndef BASE_H_
#define BASE_H_

#include "somatic.h"
#include "somatic/util.h"
#include "somatic.pb-c.h"

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus

/**
 * Somatic__Vector message utils
 */
// Allocate a Somatic__Vector
Somatic__Vector *somatic_vector_alloc(size_t size);

// Free allocated memory
int somatic_vector_free(Somatic__Vector *msg);

// Publish message on Ach channel
int somatic_vector_publish(Somatic__Vector *msg, ach_channel_t *chan) AA_DEPRECATED;


// Declares a receive function
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_vector_receive,
							somatic__vector,
							Somatic__Vector);

// Declares a last receive function
SOMATIC_DEC_GET_LAST_UNPACK(somatic_vector_lastreceive,
							somatic__vector,
							Somatic__Vector);

// Print the contents of a Somatic__Vector message
void somatic_vector_print(Somatic__Vector *msg);

/**
 * Somatic__Ivector message utils
 */
// Allocate a Somatic__Ivector
Somatic__Ivector *somatic_ivector_alloc(size_t size);

// Free allocated memory
int somatic_ivector_free(Somatic__Ivector *msg);

// Publish message on Ach channel
int somatic_ivector_publish(Somatic__Ivector *msg, ach_channel_t *chan) AA_DEPRECATED;


// Declares a receive function
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_ivector_receive,
							somatic__ivector,
							Somatic__Ivector);

// Declares a last receive function
SOMATIC_DEC_GET_LAST_UNPACK(somatic_ivector_lastreceive,
							somatic__ivector,
							Somatic__Ivector);


// Print the contents of a Somatic__Ivector message
void somatic_ivector_print(Somatic__Ivector *msg) AA_DEPRECATED;

#ifdef __cplusplus
}
#endif //__cplusplus

#endif /* BASE_H_ */
