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
// somatic_MSGNAME_alloc
// somatic_MSGNAME_free
// somatic_MSGNAME_set_FIELD
// somatic_MSGNAME_get_FIELD

btTransform somatic_ply_transform( Somatic__Transform * );
btTransform somatic_ply_transform( const double *x, size_t n );

void somatic_opine_quaternion( double *x, size_t n, const btQuaternion &q );
void somatic_opine_vector3( double *x, size_t n, const btVector3 &v );
void somatic_opine_transform( double *x, size_t n, const btTransform &T );

#endif

static struct timespec somatic_ply_timespec( Somatic__Timespec * t) {
    return aa_tm_make( (time_t) t->sec, (long) t->nsec );
}

static void somatic_opine_timespec( Somatic__Timespec * t,
                                    struct timespec ts ) {
    t->sec = ts.tv_sec;
    t->nsec = ts.tv_nsec;
    t->has_nsec = 1;
}



//=== Timespec ===

Somatic__Timespec *somatic_timespec_alloc();
void somatic_timespec_free(Somatic__Timespec *pb);

//=== Vector ===
Somatic__Vector *somatic_vector_alloc(size_t size);
void somatic_vector_free(Somatic__Vector *pb);
void somatic_vector_set_unit(Somatic__Vector *pb, int unit);


//=== Transform ===
Somatic__Transform *somatic_transform_alloc();
void somatic_transform_free(Somatic__Transform *pb);
void somatic_transform_set_quat( Somatic__Transform *pb, const double r[4] );
void somatic_transform_set_vec( Somatic__Transform *pb, const double r[3] );
void somatic_transform_set_tf12( Somatic__Transform *pb, double r[12] );
void somatic_transform_get_quat( Somatic__Transform *pb, double r[4] );
void somatic_transform_get_vec( Somatic__Transform *pb, double r[3] );
void somatic_transform_get_tf12( Somatic__Transform *pb, double r[12] );

//=== Metadata ===
/// heap allocates metadata object
Somatic__Metadata *somatic_metadata_alloc();
/// frees heap allocated metadata object and all children
void somatic_metadata_free( Somatic__Metadata *pb );
/// strdups the label
void somatic_metadata_set_label( Somatic__Metadata *pb, const char *label );
/// heap allocates timespec for time field
void somatic_metadata_set_time( Somatic__Metadata *pb, int64_t sec, int32_t nsec );
/// heap allocates timespec for until field
void somatic_metadata_set_until( Somatic__Metadata *pb, int64_t sec, int32_t nsec );


//=== Multi Transform ===
Somatic__MultiTransform *somatic_multi_transform_alloc(size_t n);
void somatic_multi_transform_free(Somatic__MultiTransform *pb);


#endif //SOMATIC_MSG_H
