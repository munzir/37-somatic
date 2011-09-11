/* -*- mode: C; c-basic-offset: 4 -*- */
/* ex: set shiftwidth=4 tabstop=4 expandtab: */
/*
 * Copyright (c) 2010-2011, Georgia Tech Research Corporation
 * All rights reserved.
 *
 * Author(s): Neil T. Dantam <ntd@gatech.edu>
 * Georgia Tech Humanoid Robotics Lab
 * Under Direction of Prof. Mike Stilman <mstilman@cc.gatech.edu>
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

#include "somatic.pb-c.h"
#include "somatic/msg.h"


btTransform somatic_ply_transform( Somatic__Transform * T) {
    return btTransform( btQuaternion( T->rotation->data[0],
                                      T->rotation->data[1],
                                      T->rotation->data[2],
                                      T->rotation->data[3] ),
                        btVector3( T->translation->data[0],
                                   T->translation->data[1],
                                   T->translation->data[2] ) );
}

btTransform somatic_ply_transform( const double *x, size_t n ) {
    assert( 7 == n );
    return btTransform( btQuaternion( x[0],
                                      x[1],
                                      x[2],
                                      x[3] ),
                        btVector3( x[4],
                                   x[5],
                                   x[6] ) );
}


void somatic_opine_quaternion( double *x, size_t n, const btQuaternion &q ) {
    assert( 4 <= n );
    x[0] = q.x();
    x[1] = q.y();
    x[2] = q.z();
    x[3] = q.w();

}

void somatic_opine_vector3( double *x, size_t n, const btVector3 &v ) {
    assert( 3 <= n );
    x[0] = v.x();
    x[1] = v.y();
    x[2] = v.z();
}


void somatic_opine_transform( double *x, size_t n, const btTransform &T ) {
    assert( 7 == n );
    somatic_opine_quaternion(x, n, T.getRotation() );
    somatic_opine_vector3(x + 4, n - 4, T.getOrigin() );
}
