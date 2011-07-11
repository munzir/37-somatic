/* -*- mode: C; c-basic-offset: 4  -*- */
/* ex: set shiftwidth=4 expandtab: */
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
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *
 *     * Neither the name of the copyright holder(s) nor the names of
 *       its/their contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER(S) AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include <amino.h>
#include "somatic.pb-c.h"
#include "somatic/msg.h"

#define VECTOR_FIELD_INIT( PB, FIELD, SIZE )              \
    if( NULL == (PB)->FIELD ) {                           \
        (PB)->FIELD = somatic_vector_alloc( (SIZE) );     \
    }                                                     \
    assert( (PB)->FIELD->n_data == SIZE );                \


#define VECTOR_FIELD_SET( PB, FIELD, SIZE, DATA )       \
    VECTOR_FIELD_INIT( PB, FIELD, SIZE )                \
    aa_fcpy( (PB)->FIELD->data, DATA, SIZE )

//=== Timespec ===

Somatic__Timespec *somatic_timespec_alloc() {
   Somatic__Timespec *pb = AA_NEW0( Somatic__Timespec );
   somatic__timespec__init( pb );
   return pb;
}
void somatic_timespec_free(Somatic__Timespec *pb) {
    aa_free_if_valid( pb );
}

//=== Vector ===
Somatic__Vector *somatic_vector_alloc(size_t size) {
    Somatic__Vector *pb = AA_NEW0( Somatic__Vector );
    somatic__vector__init( pb );
    pb->data = AA_NEW0_AR(double, size);
    pb->n_data = size;
    return pb;
}

void somatic_vector_free(Somatic__Vector *pb) {
    if( pb ) {
        aa_free_if_valid( pb->units );
        aa_free_if_valid( pb->data );
        free( pb );
    }
}
void somatic_vector_set_unit(Somatic__Vector *pb, int unit) {
    pb->units = AA_NEW( Somatic__Unit );
    pb->units[0] = unit;
    pb->n_units = 1;
}

void somatic_vector_set_data(Somatic__Vector *pb, const double *x, size_t n) {
    assert( n <= pb->n_data );
    aa_fcpy( pb->data, x, n );
}


//=== IVector ===
Somatic__Ivector *somatic_ivector_alloc(size_t size) {
    Somatic__Ivector *pb = AA_NEW0( Somatic__Ivector );
    somatic__ivector__init( pb );
    pb->data = AA_NEW0_AR(int64_t, size);
    pb->n_data = size;
    return pb;
}

void somatic_ivector_free(Somatic__Ivector *pb) {
    if( pb ) {
        aa_free_if_valid( pb->data );
        free( pb );
    }
}

//=== Transform ===
Somatic__Transform *somatic_transform_alloc() {
    Somatic__Transform *pb = AA_NEW0(Somatic__Transform);
    somatic__transform__init(pb);
    pb->meta = somatic_metadata_alloc();
    pb->meta->type = SOMATIC__MSG_TYPE__TRANSFORM;
    return pb;

}
void somatic_transform_free(Somatic__Transform *pb) {
    if( pb ) {
        somatic_vector_free( pb->translation );
        somatic_vector_free( pb->rotation );
        somatic_metadata_free( pb->meta );
        free(pb);
    }
}

void somatic_transform_set_quat( Somatic__Transform *pb, const double r[4] ) {
    VECTOR_FIELD_SET( pb, rotation, 4, r );
}

void somatic_transform_set_vec( Somatic__Transform *pb, const double r[3] ) {
    VECTOR_FIELD_SET( pb, translation, 3, r );
}

void somatic_transform_set_tf12( Somatic__Transform *pb, double r[12] ) {
    VECTOR_FIELD_SET( pb, translation, 3, r+9 );
    VECTOR_FIELD_INIT( pb, rotation, 4 );
    aa_tf_rotmat2quat( r, pb->rotation->data );

}
void somatic_transform_get_quat( Somatic__Transform *pb, double r[4] ) {
    assert( pb->rotation && pb->rotation->data && 4 == pb->rotation->n_data );
    aa_fcpy( r, pb->rotation->data, 4 );

}
void somatic_transform_get_vec( Somatic__Transform *pb, double r[3] ) {
    assert( pb->translation && pb->translation->data && 3 == pb->translation->n_data );
    aa_fcpy( r, pb->translation->data, 3 );

}
void somatic_transform_get_tf12( Somatic__Transform *pb, double r[12] ) {
    assert( pb->rotation && pb->rotation->data && 4 == pb->rotation->n_data );
    assert( pb->translation && pb->translation->data && 3 == pb->translation->n_data );
    aa_fcpy( r+9, pb->translation->data, 3 );
    aa_tf_quat2rotmat( pb->rotation->data, r );

}

//=== Metadata ===
Somatic__Metadata *somatic_metadata_alloc() {
    Somatic__Metadata *pb = AA_NEW0(Somatic__Metadata);
    somatic__metadata__init( pb );
    return pb;
}
void somatic_metadata_free( Somatic__Metadata *pb ) {
    if( pb ) {
        somatic_timespec_free( pb->time );
        somatic_timespec_free( pb->until );
        aa_free_if_valid( pb->label );
    }
}
void somatic_metadata_set_label( Somatic__Metadata *pb, const char *label ) {
    assert( NULL == pb->label );
    pb->label = strdup(label);
}
void somatic_metadata_set_time( Somatic__Metadata *pb, int64_t sec, int32_t nsec ) {
    if( NULL == pb->time ) {
        pb->time = somatic_timespec_alloc();
    }
    pb->time->sec = sec;
    pb->time->nsec = nsec;
    pb->time->has_nsec = 1;
}

void somatic_metadata_set_time_timespec( Somatic__Metadata *pb, struct timespec ts ) {
    somatic_metadata_set_time( pb, ts.tv_sec, ts.tv_nsec );
}

void somatic_metadata_set_time_now( Somatic__Metadata *pb ) {
    somatic_metadata_set_time_timespec( pb, aa_tm_now() );
}

void somatic_metadata_set_until( Somatic__Metadata *pb, int64_t sec, int32_t nsec ) {
    if( NULL == pb->until ) {
        pb->until = somatic_timespec_alloc();
    }
    pb->until->sec = sec;
    pb->until->nsec = nsec;
    pb->until->has_nsec = 1;
}


void somatic_metadata_set_until_duration( Somatic__Metadata *pb,
                                          double duration ) {
    struct timespec now;
    now.tv_sec = pb->time->sec;
    now.tv_nsec = pb->time->has_nsec ?  pb->time->nsec : 0;
    struct timespec until = aa_tm_add(now,  aa_tm_sec2timespec( duration ) );
    somatic_metadata_set_until( pb, until.tv_sec, until.tv_nsec );
}

//=== Multi Transform ===
Somatic__MultiTransform *somatic_multi_transform_alloc(size_t n) {
    Somatic__MultiTransform *pb = AA_NEW0( Somatic__MultiTransform );
    somatic__multi_transform__init( pb );
    pb->tf = AA_NEW_AR( Somatic__Transform*, n );
    pb->n_tf = n;
    for( size_t i = 0; i < n; i ++ ) {
        pb->tf[i] = somatic_transform_alloc();
    }
    pb->meta = somatic_metadata_alloc();
    pb->meta->type = SOMATIC__MSG_TYPE__MULTI_TRANSFORM;
    return pb;
}
void somatic_multi_transform_free(Somatic__MultiTransform *pb) {
    if( pb ) {
        somatic_transform_free( pb->origin );
        somatic_metadata_free( pb->meta );
        if( pb->tf ) {
            for( size_t i = 0; i < pb->n_tf; i++ ) {
                somatic_transform_free( pb->tf[i] );
            }
        }
    }
}

//=== Force Moment ===
Somatic__ForceMoment *somatic_force_moment_alloc( int alloc_force, int alloc_moment ) {
    Somatic__ForceMoment *pb = AA_NEW0(Somatic__ForceMoment);
    somatic__force_moment__init( pb );
    if( alloc_force ) pb->force = somatic_vector_alloc(3);
    if( alloc_moment ) pb->moment = somatic_vector_alloc(3);
    pb->meta = somatic_metadata_alloc();
    pb->meta->type = SOMATIC__MSG_TYPE__FORCE_MOMENT;
    return pb;
}

void somatic_force_moment_free( Somatic__ForceMoment *pb ) {
    if( pb ) {
        somatic_vector_free( pb->force );
        somatic_vector_free( pb->moment );
        somatic_metadata_free( pb->meta );
    }
}

void somatic_force_moment_set( Somatic__ForceMoment *pb, const double v[6] ) {
    aa_fcpy( pb->force->data, v, 3 );
    aa_fcpy( pb->moment->data, v+3, 3 );
}
void somatic_force_moment_get( const Somatic__ForceMoment *pb, double v[6] ) {
    aa_fcpy( v, pb->force->data, 3 );
    aa_fcpy( v+3, pb->moment->data, 3 );
}

//=== Motor Cmd ===
Somatic__MotorCmd *somatic_motor_cmd_alloc( size_t n ) {
    Somatic__MotorCmd *pb = AA_NEW0(Somatic__MotorCmd);
    somatic__motor_cmd__init( pb );
    pb->values = somatic_vector_alloc( n );
    pb->meta = somatic_metadata_alloc();
    pb->meta->type = SOMATIC__MSG_TYPE__MOTOR_CMD;
    pb->meta->has_type = 1;
    return pb;
}
void somatic_motor_cmd_free( Somatic__MotorCmd *pb ) {
    if( pb ) {
        somatic_vector_free( pb->values );
        somatic_metadata_free( pb->meta );
        free(pb);
    }
}
void somatic_motor_cmd_set( Somatic__MotorCmd *pb,
                            Somatic__MotorParam param, const double *x, size_t n ) {
    if( NULL == pb->values ) {
        pb->values = somatic_vector_alloc( n );
    }
    pb->param = param;
    pb->has_param = 1;
    if( x ) somatic_vector_set_data( pb->values, x, n );
}

//=== Motor State ===
Somatic__MotorState *somatic_motor_state_alloc() {
    Somatic__MotorState *pb = AA_NEW0(Somatic__MotorState);
    somatic__motor_state__init(pb);
    pb->meta = somatic_metadata_alloc();
    pb->meta->type = SOMATIC__MSG_TYPE__MOTOR_STATE;
    return pb;
}
void somatic_motor_state_free(Somatic__MotorState *pb) {
    if( pb ) {
        somatic_vector_free( pb->position );
        somatic_vector_free( pb->velocity );
        somatic_vector_free( pb->acceleration );
        somatic_vector_free( pb->current );
        somatic_metadata_free( pb->meta );
        free(pb);
    }
}
void somatic_motor_state_set_position( Somatic__MotorState *pb,
                                       const double *x, size_t n ) {
    VECTOR_FIELD_SET( pb, position, n, x );
}
void somatic_motor_state_set_velocity( Somatic__MotorState *pb,
                                       const double *x, size_t n ) {
    VECTOR_FIELD_SET( pb, velocity, n, x );
}
void somatic_motor_state_set_acceleraton( Somatic__MotorState *pb,
                                          const double *x, size_t n ) {
    VECTOR_FIELD_SET( pb, acceleration, n, x );
}
void somatic_motor_state_set_current( Somatic__MotorState *pb,
                                      const double *x, size_t n ) {
    VECTOR_FIELD_SET( pb, current, n, x );
}

//=== Joystick ===
Somatic__Joystick *somatic_joystick_alloc(size_t n_axes, size_t n_buttons) {
    Somatic__Joystick *pb = AA_NEW0(Somatic__Joystick);
    somatic__joystick__init(pb);
    pb->meta = somatic_metadata_alloc();
    pb->meta->type = SOMATIC__MSG_TYPE__JOYSTICK;
    pb->meta->has_type = 1;
    pb->axes = somatic_vector_alloc(n_axes);
    pb->buttons = somatic_ivector_alloc(n_buttons);
    return pb;
}
void somatic_joystick_free(Somatic__Joystick *pb) {
    if(pb) {
        somatic_vector_free(pb->axes);
        somatic_ivector_free(pb->buttons);
        somatic_metadata_free(pb->meta);
        free(pb);
    }
}
