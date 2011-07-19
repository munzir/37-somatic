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

#include "somatic.h"
#include "somatic/daemon.h"
#include <somatic.pb-c.h>
#include <argp.h>
#include <ach.h>
#include <unistd.h>

/// argp program version
const char *argp_program_version = "somatic_live_plot 0.0";
#define ARGP_DESC "Plots stuff"





/*------------*/
/* PROTOTYPES */
/*------------*/

typedef struct {
    FILE* gnuplot;
    double *data;
    size_t n_samples;
    size_t n_each;
    size_t i;
    int printed_header;
    char **labels;
} gnuplot_live_t;

typedef struct {
    somatic_d_t d_cx;
    somatic_d_opts_t d_opts;
    ach_channel_t chan_in;
    uint8_t work[1024*64];
    gnuplot_live_t plot;
} cx_t;

/** Initialize the daemon */
static void init(cx_t *cx);
/** Main daemon run loop */
static void destroy(cx_t *cx);
/** Cleanup for exit */
static void run(cx_t *cx);
/** Update state */
static void update(cx_t *cx);

/** Get next msg */
static Somatic__Event* next_msg(cx_t *cx);

/** display plot */
static void plot(gnuplot_live_t *pl);

/// argp object
static int parse_opt( int key, char *arg, struct argp_state *state);
extern struct argp_option argp_options[];
extern struct argp argp;



/* ------- */
/* GLOBALS */
/* ------- */

static const char *opt_channel_name = "foo";
//static const char *opt_quantity = "position";
static double opt_range_min = -10;
static double opt_range_max = 10;
static size_t opt_samples = 100;
static double opt_frequency = 10;
static char *opt_labels = NULL;



/* ------- */
/* HELPERS */
/* ------- */

static void init(cx_t *cx) {
    memset(&cx->d_cx, 0, sizeof cx->d_cx); // zero initialize
    cx->d_opts.ident = "liveplot";

    somatic_d_init(&cx->d_cx,
                   &cx->d_opts);  // init daemon variables, channels, log, etc

    // open channel
    somatic_d_channel_open( &cx->d_cx, &cx->chan_in,
                                opt_channel_name, NULL );

    // open gnuplot
    cx->plot.gnuplot = popen("gnuplot -persist", "w");

    fprintf(cx->plot.gnuplot, "set title 'Channel: %s\n",
            opt_channel_name);
    fprintf(cx->plot.gnuplot, "set xlabel 'Time (s)'\n");
    //fprintf(cx->plot.gnuplot, "set ylabel '%s\n", opt_quantity);
    fprintf(cx->plot.gnuplot, "set yrange [%f:%f]\n", opt_range_min, opt_range_max);



    // init struct
    Somatic__Event *msg = next_msg(cx);
    somatic_d_check(&cx->d_cx, SOMATIC__EVENT__PRIORITIES__EMERG,
                    SOMATIC__EVENT__CODES__COMM_FAILED_TRANSPORT,
                    NULL != msg, "next_msg",
                    "no initial msg");
    if( NULL == msg ) { somatic_d_die(&cx->d_cx); }
    cx->plot.n_samples = opt_samples;
    cx->plot.n_each = msg->attr->n_data;
    cx->plot.data = AA_NEW0_AR(double, cx->plot.n_samples*cx->plot.n_each);
    // data labels
    if( opt_labels ) {
        size_t n_labels = 0;
        size_t n = strlen(opt_labels);
        for(size_t i = 0; i<n; i ++ )
            if(':' == opt_labels[i]) n_labels++;
        somatic_d_check(&cx->d_cx, SOMATIC__EVENT__PRIORITIES__WARNING,
                        SOMATIC__EVENT__CODES__BAD_PARAM,
                        n_labels + 1 == cx->plot.n_each, "plot-init",
                        "wrong number of labels");
        if( n_labels + 1 == cx->plot.n_each ) {
            cx->plot.labels = AA_NEW0_AR( char*, n_labels + 1 );
            int i = 0;
            for( char *s = strtok(opt_labels, ":"); s;
                 s = strtok(NULL, ":"), i++ ) {
                cx->plot.labels[i] = s;
            }
        }
    }
    somatic__event__free_unpacked( msg, &protobuf_c_system_allocator );

}


static Somatic__Event *next_msg(cx_t *cx) {
    size_t nread;
    ach_status_t r = (ach_status_t)
        ach_wait_last( &cx->chan_in, cx->work, sizeof(cx->work),
                       &nread, NULL );
    int recv_ok = (ACH_OK == r) || (ACH_MISSED_FRAME == r);
    somatic_d_check(&cx->d_cx, SOMATIC__EVENT__PRIORITIES__ERR,
                    SOMATIC__EVENT__CODES__COMM_FAILED_TRANSPORT,
                    recv_ok, "ach_wait_last",
                    "reading `%s': %s\n",
                    opt_channel_name, ach_result_to_string(r));
    Somatic__Event *msg = NULL;
    if( ACH_OK == r ) {
        msg = somatic__event__unpack( &protobuf_c_system_allocator,
                                    nread, cx->work );
        // check msg
        int msg_ok =  msg->attr &&
            msg->attr->data &&
            (msg->attr->n_data == cx->plot.n_each
             || NULL == cx->plot.data);
        somatic_d_check(&cx->d_cx, SOMATIC__EVENT__PRIORITIES__ERR,
                        SOMATIC__EVENT__CODES__COMM_BAD_MSG,
                        msg_ok, "somatic_event",
                        "no data");
        if( !msg_ok ) {
            somatic__event__free_unpacked( msg,
                                           &protobuf_c_system_allocator );
            msg = NULL;
        }
    }
    return msg;
}

static void update(cx_t *cx) {
    // get message
    Somatic__Event *msg = next_msg(cx);
    if( msg ) {
        // store message
        aa_fcpy( cx->plot.data + (cx->plot.i*cx->plot.n_each), msg->attr->data,
                 cx->plot.n_each );
        cx->plot.i = (cx->plot.i + 1) % cx->plot.n_samples;
        // free message
        somatic__event__free_unpacked( msg, &protobuf_c_system_allocator );
    }
}


static void run(cx_t *cx) {
    somatic_d_event( &cx->d_cx, SOMATIC__EVENT__PRIORITIES__NOTICE,
                     SOMATIC__EVENT__CODES__PROC_RUNNING,
                     NULL, NULL );
    while(!somatic_sig_received) {
        update(cx);
        plot(&cx->plot);
        usleep( (int) (1e6 / opt_frequency));
    }
    somatic_d_event( &cx->d_cx, SOMATIC__EVENT__PRIORITIES__NOTICE,
                     SOMATIC__EVENT__CODES__PROC_STOPPING,
                     NULL, NULL );

}

void destroy(cx_t *cx) {
    int r;
    // close channel
    somatic_d_channel_close( &cx->d_cx, &cx->chan_in );
    // close gnuplot
    r = fclose( cx->plot.gnuplot );
    somatic_d_check(&cx->d_cx, SOMATIC__EVENT__PRIORITIES__ERR,
                    SOMATIC__EVENT__CODES__UNKNOWN,
                    0 == r, "fclose",
                    "closing gnuplot: %s\n",
                    strerror(errno));
    // end daemon
    somatic_d_destroy(&cx->d_cx);
}


static void plot(gnuplot_live_t *pl) {
    // header
    if( ! pl->printed_header ) {
        pl->printed_header = 1;
        if( pl->labels )
            fprintf(pl->gnuplot, "plot '-' with lines title '%s'",
                pl->labels[0]);
        else
            fprintf(pl->gnuplot, "plot '-' with lines title '0'");
        for( size_t j = 1; j < pl->n_each; j++ ) {
            if( pl->labels )
                fprintf(pl->gnuplot, ", '-' with lines title '%s'",
                    pl->labels[j]);
            else
                fprintf(pl->gnuplot, ", '-' with lines title '%d'", j);
        }
        fprintf(pl->gnuplot, "\n");
    } else {
        fprintf(pl->gnuplot, "replot\n");
    }
    // data
    for (size_t j = 0; j < pl->n_each; j++ ) {
        for( size_t k = 0;  k < pl->n_samples; k ++ ) {
            size_t i = (k+pl->i) % pl->n_samples;
            size_t idx = j + i*pl->n_each;
            fprintf(pl->gnuplot, "%f, %f\n",
                    k/opt_frequency, (pl->data)[idx] );
        }
        fprintf(pl->gnuplot, "e\n" );
    }
    fflush( pl->gnuplot );
}

/* ---- */
/* MAIN */
/* ---- */
int main( int argc, char **argv ) {
  (void) argc;
  (void) argv;
  static cx_t cx;
  //somatic_verbprintf_prefix = "motor_plot";
  argp_parse (&argp, argc, argv, 0, NULL, NULL);
  init(&cx);
  run(&cx);
  destroy(&cx);

  return 0;
}

static void parse_error( const char *name) {
    fprintf(stderr, "Error parsing %s\n", name);
    exit(EXIT_FAILURE);
}

int parse_opt( int key, char *arg, struct argp_state *state) {
    (void)state;
    char *endptr = 0;
    switch (key) {
    case 'v':
        somatic_opt_verbosity++;
        break;
    case 'c':
        opt_channel_name = strdup(arg);
        break;
    case 'n':
        opt_samples = strtol( arg, &endptr, 10 );
        if( NULL == endptr ) parse_error( "samples (-n)");
        break;
    case '0':
        opt_range_min = strtod( arg, &endptr );
        if( NULL == endptr ) parse_error( "min (-0)");
        break;
    case '1':
        opt_range_max = strtod( arg, &endptr );
        if( NULL == endptr ) parse_error( "max (-1)");
        break;
    case 'f':
        opt_frequency = strtod( arg, &endptr );
        if( NULL == endptr ) parse_error( "frequency (-f)");
        break;
    case 'L':
        opt_labels = strdup(arg);
        break;
    };
    return 0;
}

/* ---------- */
/* ARGP Junk  */
/* ---------- */


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
        .name = "labels",
        .key = 'L',
        .arg = "label-list",
        .flags = 0,
        .doc = "colon-seprated list of data labels"
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
                     "args", ARGP_DESC,
                     NULL, NULL, NULL };
