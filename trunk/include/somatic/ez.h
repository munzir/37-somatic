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


#ifndef SOMATIC_EZ_H
#define SOMATIC_EZ_H

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <ach.h>
#include "somatic.h"
#include "somatic.pb-c.h"
#include "util.h"

/** \file ez.h
 * \author Neil T. Dantam
 * \brief Super-Simplified ach + protocol buffers
 */

/// handle to a channel
typedef ach_channel_t *somatic_channel_id;

/// initialize somatic
void somatic_ez_init();

/// open a channel (must already have been created)
somatic_channel_id somatic_ez_open( const char *name );

/// close a channel
void somatic_ez_close( somatic_channel_id chan );


/// read a single integer
int somatic_ez_read_int( somatic_channel_id chan );
/// write a single integer
void somatic_ez_write_int( somatic_channel_id chan, int i );

/// read an array of floating point
void somatic_ez_read_real_vec( somatic_channel_id chan, double *x, int n);
/// write an array of floating point
void somatic_ez_write_real_vec( somatic_channel_id chan, const double *x, int n );

/// read an array of int
void somatic_ez_read_int_vec( somatic_channel_id chan, int *x, int n);
/// write an array of int
void somatic_ez_write_int_vec( somatic_channel_id chan, const int *x, int n );

#endif //SOMATIC_EZ_H
