/* -*- mode: C; c-basic-offset: 4  -*- */
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


#include "somatic/ez.h"



/// initialize somatic
void somatic_ez_init() {

}

static Somatic__EzMsg *
somatic_ez_slow_last_unpack( somatic_channel_id chan ) {
    size_t n;
    int r = ach_copy_last( chan, NULL, 0, &n );
    somatic_hard_assert( ACH_OVERFLOW == r, "Unknown error: %s\n",
                         ach_result_to_string( r ) );
    uint8_t buf[n];
    size_t nread;
    r = ach_copy_last( chan, &buf[0], n, &nread );
    somatic_hard_assert( n == nread &&
                         (ACH_OK == r || ACH_MISSED_FRAME == r),
                         "EZ fail: %s\n", ach_result_to_string( r ) );
    return somatic__ez_msg__unpack( &protobuf_c_system_allocator, n, buf );

}

/// open a channel (must already have been created)
somatic_channel_id somatic_ez_open( const char *name ) {
    somatic_channel_id chan = SOMATIC_NEW( ach_channel_t );
    int r = ach_open( chan, name, NULL );
    somatic_hard_assert( ACH_OK == r, "Error opening channel: %s\n",
                         ach_result_to_string( r ) );
    r = ach_flush( chan );
    somatic_hard_assert( ACH_OK == r, "Error flushing channel: %s\n",
                         ach_result_to_string( r ) );
    return chan;
}

/// close a channel
void somatic_ez_close( somatic_channel_id chan ) {
    int r = ach_close( chan );
    somatic_hard_assert( ACH_OK == r, "Error closing channel: %s\n",
                         ach_result_to_string( r ) );
}


/// read a single integer
int somatic_ez_read_int( somatic_channel_id chan ) {
    Somatic__EzMsg *msg = somatic_ez_slow_last_unpack( chan );
    somatic_hard_assert( msg->has_code, "message didn't have the integer\n");
    int i = (int)msg->code;
    somatic__ez_msg__free_unpacked( msg, &protobuf_c_system_allocator );
    return i;
}
/// write a single integer
void somatic_ez_write_int( somatic_channel_id chan, int i ) {
    Somatic__EzMsg msg;
    memset(&msg, 0, sizeof(msg));
    somatic__ez_msg__init( &msg );
    msg.has_code = 1;
    msg.code = i;
    int r =SOMATIC_PACK_SEND( chan, somatic__ez_msg, &msg );
    somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n",
                         ach_result_to_string( r ) );
}

/// read an array of floating point
void somatic_ez_read_real_vec( somatic_channel_id chan, double *x, int n) {
    Somatic__EzMsg *msg = somatic_ez_slow_last_unpack( chan );
    somatic_hard_assert( msg->x && msg->x->data,
                         "message didn't have the real vector\n");
    somatic_hard_assert( (size_t)n == msg->x->n_data,
                         "buffer should be %d elements (%d passed)\n",
                         msg->x->n_data, n );
    somatic_realcpy( x, msg->x->data, (size_t) n );
    somatic__ez_msg__free_unpacked( msg, &protobuf_c_system_allocator );
}
/// write an array of floating point
void somatic_ez_write_real_vec( somatic_channel_id chan, const double *x, int n ) {
    Somatic__EzMsg msg;
    Somatic__Vector vec;
    memset(&msg, 0, sizeof(msg));
    memset(&vec, 0, sizeof(vec));
    somatic__ez_msg__init( &msg );
    somatic__vector__init( &vec );
    msg.x = &vec;
    vec.data = (double*)x;
    vec.n_data = (size_t) n;
    int r =SOMATIC_PACK_SEND( chan, somatic__ez_msg, &msg );
    somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n",
                         ach_result_to_string( r ) );
}

/// read an array of int
void somatic_ez_read_int_vec( somatic_channel_id chan, int *x, int n) {
    Somatic__EzMsg *msg = somatic_ez_slow_last_unpack( chan );
    somatic_hard_assert( msg->i && msg->i->data,
                         "message didn't have the real vector\n");
    somatic_hard_assert( (size_t)n == msg->i->n_data,
                         "buffer should be %d elements (%d passed)\n",
                         msg->i->n_data, n );
    for( size_t i = 0; i < (size_t)n; i ++ ) {
        x[i] = (int) msg->i->data[i];
    }
    somatic__ez_msg__free_unpacked( msg, &protobuf_c_system_allocator );
}
/// write an array of int
void somatic_ez_write_int_vec( somatic_channel_id chan, const int *x, int n ) {
    Somatic__EzMsg msg;
    Somatic__Ivector vec;
    int64_t data[n];
    memset(&msg, 0, sizeof(msg));
    memset(&vec, 0, sizeof(vec));
    somatic__ez_msg__init( &msg );
    somatic__ivector__init( &vec );
    msg.i = &vec;
    vec.data = data;
    vec.n_data = (size_t) n;
    for( size_t i = 0; i < (size_t)n; i ++ ) {
        data[i] = (int64_t)x[i];
    }
    int r = SOMATIC_PACK_SEND( chan, somatic__ez_msg, &msg );
    somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n",
                         ach_result_to_string( r ) );
}

