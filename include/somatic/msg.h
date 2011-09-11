/* -*- mode: C; c-basic-offset: 4 -*- */
/* ex: set shiftwidth=4 tabstop=4 expandtab: */
/*
 * Copyright (c) 2010-2011, Georgia Tech Research Corporation
 * All rights reserved.
 *
 * Author(s): Neil T. Dantam <ntd@gatech.edu>
 * Georgia Tech Humanoid Robotics Lab
 * Under Direction of Prof. Mike Stilman
 *
 *
 * This file is provided under the following "BSD-style" License:
 *
 *
 *   Redistribution and use in source and binary forms, with or
 *   without modification, are permitted provided that the following
 *   conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 *   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 *   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 *   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *   AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *   POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef SOMATIC_MSG_H
#define SOMATIC_MSG_H

// transform
#ifdef __cplusplus

#include "somatic.pb-c.h"
//#include <bullet/LinearMath/btTransform.h>
#include "util.h"


// naming convention
// somatic_MSGNAME_alloc
// somatic_MSGNAME_free
// somatic_MSGNAME_set_FIELD
// somatic_MSGNAME_get_FIELD

//btTransform somatic_ply_transform( Somatic__Transform * );
//btTransform somatic_ply_transform( const double *x, size_t n );

//void somatic_opine_quaternion( double *x, size_t n, const btQuaternion &q );
//void somatic_opine_vector3( double *x, size_t n, const btVector3 &v );
//void somatic_opine_transform( double *x, size_t n, const btTransform &T );

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


//=== Allocator ===
typedef ProtobufCAllocator somatic_pbregalloc_t;

AA_API void somatic_pbregalloc_set( somatic_pbregalloc_t *pba, aa_region_t *reg );
AA_API void somatic_pbregalloc_init( somatic_pbregalloc_t *a, size_t n );
AA_API void somatic_pbregalloc_release( somatic_pbregalloc_t *a );
AA_API void somatic_pbregalloc_destroy( somatic_pbregalloc_t *a );

//=== Timespec ===

Somatic__Timespec *somatic_timespec_alloc();
void somatic_timespec_free(Somatic__Timespec *pb);
static inline void somatic_timespec_set_s( Somatic__Timespec *pb, int64_t s ) {
    pb->sec = s;
}
static inline void somatic_timespec_set_ns( Somatic__Timespec *pb, int32_t ns ) {
    pb->nsec = ns;
    pb->has_nsec = 1;
}
static inline struct timespec somatic_timespec_get( Somatic__Timespec *pb ) {
    return aa_tm_make( (time_t) pb->sec, pb->has_nsec ? (long) pb->nsec : 0 );
}

//=== Vector ===
Somatic__Vector *somatic_vector_alloc(size_t size);
void somatic_vector_free(Somatic__Vector *pb);
void somatic_vector_set_unit(Somatic__Vector *pb, int unit);
void somatic_vector_set_data(Somatic__Vector *pb, const double *x, size_t n);

//=== IVector ===
AA_API Somatic__Ivector *somatic_ivector_alloc(size_t size);
AA_API void somatic_ivector_free(Somatic__Ivector *pb);

//=== Transform ===
AA_API Somatic__Transform *somatic_transform_alloc();
AA_API void somatic_transform_free(Somatic__Transform *pb);
AA_API void somatic_transform_set_quat( Somatic__Transform *pb, const double r[4] );
AA_API void somatic_transform_set_vec( Somatic__Transform *pb, const double r[3] );
AA_API void somatic_transform_set_tf12( Somatic__Transform *pb, double r[12] );
AA_API void somatic_transform_get_quat( Somatic__Transform *pb, double r[4] );
AA_API void somatic_transform_get_vec( Somatic__Transform *pb, double r[3] );
AA_API void somatic_transform_get_tf12( Somatic__Transform *pb, double r[12] );

//=== Metadata ===
/// heap allocates metadata object
AA_API Somatic__Metadata *somatic_metadata_alloc();
/// frees heap allocated metadata object and all children
AA_API void somatic_metadata_free( Somatic__Metadata *pb );
/// strdups the label
AA_API void somatic_metadata_set_label( Somatic__Metadata *pb, const char *label );
/// heap allocates timespec for time field
AA_API void somatic_metadata_set_time( Somatic__Metadata *pb, int64_t sec, int32_t nsec );
/// heap allocates timespec for time field and sets to current time
AA_API void somatic_metadata_set_time_now( Somatic__Metadata *pb );
/// heap allocates timespec for time field and sets to timespec
AA_API void somatic_metadata_set_time_timespec( Somatic__Metadata *pb, struct timespec ts );
/// heap allocates timespec for until field
AA_API void somatic_metadata_set_until( Somatic__Metadata *pb, int64_t sec, int32_t nsec );
/// heap allocates timespec for until field
AA_API void somatic_metadata_set_until_timespec( Somatic__Metadata *pb, struct timespec ts);
/// heap allocates timespec for until field and sets to secs offset from pb->time
AA_API void somatic_metadata_set_until_duration( Somatic__Metadata *pb,
                                          double secs );

//=== Battery ===
/// heap allocate battery message
Somatic__Battery *somatic_battery_alloc( size_t n );
/// free heap allocated battery message
void somatic_battery_free( Somatic__Battery *pb );

//=== Multi Transform ===
//AA_API Somatic__MultiTransform *somatic_multi_transform_alloc(size_t n);
//AA_API void somatic_multi_transform_free(Somatic__MultiTransform *pb);

//=== Force Moment ===
AA_API Somatic__ForceMoment *somatic_force_moment_alloc( int alloc_force, int alloc_moment );
AA_API void somatic_force_moment_free( Somatic__ForceMoment *pb );
AA_API void somatic_force_moment_set( Somatic__ForceMoment *pb, const double[6] );
AA_API void somatic_force_moment_get( const Somatic__ForceMoment *pb, double[6] );

//=== Motor Cmd ===
AA_API Somatic__MotorCmd *somatic_motor_cmd_alloc( size_t n );
AA_API void somatic_motor_cmd_free( Somatic__MotorCmd *pb );
AA_API void somatic_motor_cmd_set( Somatic__MotorCmd *pb,
                            Somatic__MotorParam param, const double *x, size_t n );

//=== Motor State ===
AA_API Somatic__MotorState *somatic_motor_state_alloc();
AA_API void somatic_motor_state_free(Somatic__MotorState *pb);
AA_API void somatic_motor_state_set_position( Somatic__MotorState *pb,
                                       const double *x, size_t n );
AA_API void somatic_motor_state_set_velocity( Somatic__MotorState *pb,
                                       const double *x, size_t n );
AA_API void somatic_motor_state_set_acceleraton( Somatic__MotorState *pb,
                                          const double *x, size_t n );
AA_API void somatic_motor_state_set_current( Somatic__MotorState *pb,
                                      const double *x, size_t n );


//=== Joystick ===
AA_API Somatic__Joystick *somatic_joystick_alloc(size_t n_axes, size_t n_buttons);
AA_API void somatic_joystick_free(Somatic__Joystick *pb);


//=== Event ===
const char *somatic_event_code2str(Somatic__Event__Codes code);
const char *somatic_event_pri2str(Somatic__Event__Priorities pri);

#endif //SOMATIC_MSG_H
