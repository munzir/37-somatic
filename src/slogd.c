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
#include <syslog.h>

/// argp program version
const char *argp_program_version = "slogd 0.0";
#define ARGP_DESC "writes somatic events to syslog"





/*------------*/
/* PROTOTYPES */
/*------------*/

typedef struct {
    somatic_d_t d;
    somatic_d_opts_t d_opts;
    ach_channel_t chan;
} cx_t;

/** Initialize the daemon */
static void init(cx_t *cx);
/** Main daemon run loop */
static void destroy(cx_t *cx);
/** Cleanup for exit */
static void run(cx_t *cx);
/** Update state */
static void update(cx_t *cx);

/// argp object
static int parse_opt( int key, char *arg, struct argp_state *state);
extern struct argp_option argp_options[];
extern struct argp argp;



/* ------- */
/* GLOBALS */
/* ------- */

static const char *opt_channel_name = "event";
static int opt_daemonize = 0;



/* ------- */
/* HELPERS */
/* ------- */

static void init(cx_t *cx) {
    memset(cx, 0, sizeof(*cx)); // zero initialize
    cx->d_opts.ident = "cslogd";
    cx->d_opts.daemonize = opt_daemonize;

    somatic_d_init(&cx->d,
                   &cx->d_opts);  // init daemon variables, channels, log, etc

    // open channel
    somatic_d_channel_open( &cx->d, &cx->chan,
                            opt_channel_name, NULL );
}



static void update(cx_t *cx) {
    int r;
    struct timespec abstimeout = aa_tm_future( aa_tm_sec2timespec(1) );

    // get message
    Somatic__Event *msg = SOMATIC_D_GET( &r, somatic__event, &cx->d,
                                         &cx->chan, &abstimeout, ACH_O_WAIT );
    // validate
    // log
    if( msg ) {
        aa_region_t *reg = &cx->d.memreg;
        char *head = aa_region_printf( reg,
                                       "[%s].(%s)",
                                       msg->ident ? msg->ident : "",
                                       msg->has_code ? somatic_event_code2str(msg->code) : "?" );
        const char *type = msg->type ? aa_region_printf(reg, ".(%s)", msg->type) : "";
        const char *proc =
            ( msg->has_code &&
              (SOMATIC__EVENT__CODES__PROC_STARTING == msg->code ||
               SOMATIC__EVENT__CODES__PROC_RUNNING == msg->code ||
               SOMATIC__EVENT__CODES__PROC_STOPPING == msg->code ||
               SOMATIC__EVENT__CODES__PROC_HALTED == msg->code ) ) ?
              aa_region_printf(reg, " %d@%s",
                               msg->has_pid ? msg->pid : 0,
                               msg->host ? msg->host : "?") : "";
        const char *comment =
            msg->comment ? aa_region_printf(reg, " %s", msg->comment) : "";
        int pri;
        if( msg->has_priority  && msg->priority <= LOG_DEBUG
            /* unsigned, always true && msg->priority >= LOG_EMERG*/ ) {
            pri = msg->priority;
        } else { pri = LOG_ERR; }
        syslog(pri, "%s%s%s%s", head, type, proc, comment );
    }
}


static void run(cx_t *cx) {
    somatic_d_event( &cx->d, SOMATIC__EVENT__PRIORITIES__NOTICE,
                     SOMATIC__EVENT__CODES__PROC_RUNNING,
                     NULL, NULL );
    while(!somatic_sig_received) {
        update(cx);
        aa_region_release( &cx->d.memreg );  // free buffers allocated during this cycle
    }
    somatic_d_event( &cx->d, SOMATIC__EVENT__PRIORITIES__NOTICE,
                     SOMATIC__EVENT__CODES__PROC_STOPPING,
                     NULL, NULL );
}

void destroy(cx_t *cx) {
    // close channel
    somatic_d_channel_close( &cx->d, &cx->chan );
    // end daemon
    somatic_d_destroy(&cx->d);
}

/* ---- */
/* MAIN */
/* ---- */
int main( int argc, char **argv ) {
  argp_parse (&argp, argc, argv, 0, NULL, NULL);
  static cx_t cx;
  init(&cx);
  run(&cx);
  destroy(&cx);

  return 0;
}


int parse_opt( int key, char *arg, struct argp_state *state) {
    (void)state;
    switch (key) {
    case 'v':
        somatic_opt_verbosity++;
        break;
    case 'c':
        opt_channel_name = strdup(arg);
        break;
    case 'd':
        opt_daemonize = 1;
        break;
    }
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
        .name = "daemonize",
        .key = 'd',
        .arg = NULL,
        .flags = 0,
        .doc = "fork off daemon process"
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