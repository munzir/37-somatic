/*
 * Copyright (c) 2010, Georgia Tech Research Corporation
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

#ifndef SOMATIC_MSG_H
#define SOMATIC_MSG_H

// transform
#ifdef __cplusplus

#include "somatic.pb-c.h"
#include <bullet/LinearMath/btTransform.h>
#include "util.h"


// naming convention
// somatic_MSGNAME_ply
// somatic_MSGNAME_opine
// somatic_MSGNAME_create
// somatic_MSGNAME_destroy

btTransform somatic_ply_transform( Somatic__Transform * );
btTransform somatic_ply_transform( const double *x, size_t n );

void somatic_opine_quaternion( double *x, size_t n, const btQuaternion &q );
void somatic_opine_vector3( double *x, size_t n, const btVector3 &v );
void somatic_opine_transform( double *x, size_t n, const btTransform &T );

#endif

static struct timespec somatic_ply_timespec( Somatic__Timespec * t) {
    return somatic_make_timespec( (time_t) t->sec, (long) t->nsec );
}

#endif //SOMATIC_MSG_H
