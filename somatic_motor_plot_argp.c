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

/** Author: Neil Dantam
 */

#include <argp.h>


static int parse_opt( int key, char *arg, struct argp_state *state);
/// argp program version
const char *argp_program_version = "somatic_motor_plot 0.0";

struct argp_option argp_options[] = {
    {
        .name = "verbose",
        .key = 'v',
        .arg = NULL,
        .flags = 0,
        .doc = "Causes verbose output"
    },
    {
        .name = "channel",
        .key = 'c',
        .arg = "channel-name",
        .flags = 0,
        .doc = "motor state channel"
    },
    {
        .name = "quantity",
        .key = 'x',
        .arg = "position|velocity|acceleration|current",
        .flags = 0,
        .doc = "quantity to plot"
    },
    {
        .name = "samples",
        .key = 'n',
        .arg = "count",
        .flags = 0,
        .doc = "number of samples to plot"
    },
    {
        .name = "min",
        .key = '0',
        .arg = "min-value",
        .flags = 0,
        .doc = "minimum range value"
    },
    {
        .name = "max",
        .key = '1',
        .arg = "max-value",
        .flags = 0,
        .doc = "maximum range value"
    },
    {
        .name = "frequency",
        .key = 'f',
        .arg = "hertz",
        .flags = 0,
        .doc = "Frequency to plot"
    },
    {
        .name = NULL,
        .key = 0,
        .arg = NULL,
        .flags = 0,
        .doc = NULL
    }
};

/// argp object
struct argp argp = { argp_options, parse_opt,
                     "args", "Plots a motor quantity",
                     NULL, NULL, NULL };
