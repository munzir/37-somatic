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
 * base.c
 *
 *  Created on: Apr 3, 2010
 *      Author: jscholz
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <ach.h>

#include "somatic/msg/base.h"

/**
 * Somatic__Vector message utils
 */
// Allocate a Somatic__Vector
Somatic__Vector *somatic_vector_alloc(size_t size)
{
	Somatic__Vector *msg = SOMATIC_NEW(Somatic__Vector);
	somatic__vector__init(msg);
	msg->data = SOMATIC_NEW_AR(double, size);
	msg->n_data = size;
	return (msg);
}

// Free allocated memory
// * DON'T call this if you're aliasing stack-allocated memory!
int somatic_vector_free(Somatic__Vector *msg)
{
	if (msg->data != NULL) {
		free(msg->data);
	}
	if (msg != NULL) {
		free(msg);
	}

	return(0);
}

// Publish message on Ach channel
int somatic_vector_publish(Somatic__Vector *msg, ach_channel_t *chan)
{
	int r = SOMATIC_PACK_SEND( chan, somatic__vector, msg );
	somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n", ach_result_to_string( r ) );

	return(r);
}


// Defines a receive function
SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_vector_receive,
							somatic__vector,
							Somatic__Vector);

// Defines a last receive function
SOMATIC_DEF_GET_LAST_UNPACK(somatic_vector_lastreceive,
							somatic__vector,
							Somatic__Vector);

// Print the contents of a Somatic__Vector message
void somatic_vector_print(Somatic__Vector *msg)
{
	size_t i;
	for (i=0; i<msg->n_data; ++i) {
		if (i < msg->n_data - 1)
			fprintf(stdout, "% 1.2lf::", msg->data[i]);
		else
			fprintf(stdout, "% 1.2lf", msg->data[i]);
	}
}

/**
 * Somatic__Ivector message utils
 */

// Allocate a Somatic__Ivector
Somatic__Ivector *somatic_ivector_alloc(size_t size)
{
	Somatic__Ivector *msg = SOMATIC_NEW(Somatic__Ivector);
	somatic__ivector__init(msg);
	msg->data = SOMATIC_NEW_AR(int64_t, size);
	msg->n_data = size;
	return (msg);
}

// Free allocated memory
// * DON'T call this if you're aliasing stack-allocated memory!
int somatic_ivector_free(Somatic__Ivector *msg)
{
	if (msg->data != NULL)
		free(msg->data);
	if (msg != NULL)
		free(msg);

	return(0);
}

// Publish message on Ach channel
int somatic_ivector_publish(Somatic__Ivector *msg, ach_channel_t *chan)
{
	int r = SOMATIC_PACK_SEND( chan, somatic__ivector, msg );
	somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n", ach_result_to_string( r ) );

	return(r);
}


// Defines a receive function
SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_ivector_receive,
							somatic__ivector,
							Somatic__Ivector);

// Defines a last receive function
SOMATIC_DEF_GET_LAST_UNPACK(somatic_ivector_lastreceive,
							somatic__ivector,
							Somatic__Ivector);

// Print the contents of a Somatic__Ivector message
void somatic_ivector_print(Somatic__Ivector *msg)
{
	size_t i;
	for (i=0; i<msg->n_data; ++i) {
		if (i < msg->n_data - 1)
			fprintf(stdout, "% 1.2lld::", msg->data[i]);
		else
			fprintf(stdout, "% 1.2lld", msg->data[i]);
	}
}
