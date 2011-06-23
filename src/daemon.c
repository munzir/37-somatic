/* -*- mode: C; c-basic-offset: 4 -*- */
/*
 * Copyright (c) 2011, Georgia Tech Research Corporation
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

#include <amino.h>
#include "somatic.h"
#include "somatic/daemon.h"

#include <syslog.h>

#define SOMATIC_SYSLOG_IDENT "somatic"
#define SOMATIC_SYSLOG_FACILITY LOG_USER
#define SOMATIC_SYSLOG_OPTION (LOG_CONS | LOG_NDELAY | LOG_PERROR)

AA_API void somatic_d_init( somatic_d_t *d, somatic_d_opts_t *opts ) {
    // open syslog
    openlog( (opts && opts->ident) ? opts->ident: "somatic",
            SOMATIC_SYSLOG_OPTION,
            SOMATIC_SYSLOG_FACILITY);

    // early check
    if( 0 != d->is_initialized ) {
        syslog(LOG_EMERG,
               "Tried to reinitialize somatic_d_t instance.  Quitting.");
        exit(1);
    }

    // open channels
    int r;
    if( 0 != (r = ach_open(&d->chan_event, "event", NULL)) ) {
        syslog(LOG_EMERG, "Couldn't open event channel: %s\n",
               ach_result_to_string(r));
        exit(1);
    }

    // set pid

    // set ident
    d->ident = strdup((opts && opts->ident) ? opts->ident : "somatic");

    // set host

    // set state

    // log direct
    syslog(LOG_NOTICE, "init daemon");

    // notify event
    somatic_d_event( d, SOMATIC__EVENT__PRIORITIES__NOTICE,
                     SOMATIC__EVENT__CODES__PROC_STARTING,
                     NULL, NULL );

}

AA_API void somatic_d_destroy( somatic_d_t *d) {
    somatic_d_event( d, SOMATIC__EVENT__PRIORITIES__NOTICE,
                     SOMATIC__EVENT__CODES__PROC_HALTED,
                     NULL, NULL );

    int r;
    // close channels
    r = ach_close(&d->chan_event);

    // free things
    if( d->ident ) free(d->ident);

    // close log
    syslog(LOG_NOTICE, "destroy daemon");
    closelog();
}

AA_API void somatic_d_state( somatic_d_t *d, int state );


AA_API void somatic_d_heartbeat( somatic_d_t *d );



static void somatic_d_vevent( somatic_d_t *d, int level, int code,
                              const char *type, const char comment_fmt[], va_list argp ) {
    Somatic__Event pb;
    memset(&pb, 0, sizeof pb);
    somatic__event__init(&pb);
    pb.priority = level;
    pb.has_priority = 1;
    pb.code = code;
    pb.has_code = 1;
    pb.ident = d->ident;
    char cpy_type[ type ? strlen(type) : 0 ];
    char fmt_buf[ 512 ] = {0};
    if(type) {
        strcpy(cpy_type, type);
        pb.type = cpy_type;
    }

    if( comment_fmt ) {
        vsnprintf( fmt_buf, sizeof(fmt_buf)-2, comment_fmt, argp );
        pb.comment =  fmt_buf;
    }

    int r = SOMATIC_PACK_SEND( &d->chan_event, somatic__event, &pb );
    if( ACH_OK != r ) {
        syslog( LOG_ERR, "couldn't send event: %s",
                ach_result_to_string(r));
    }
}

AA_API void somatic_d_event( somatic_d_t *d, int level, int code,
                             const char *type, const char comment_fmt[], ... ) {
    va_list argp;
    va_start( argp, comment_fmt );
    somatic_d_vevent(d, level, code, type, comment_fmt, argp);
    va_end( argp );

}

AA_API void somatic_d_limit( somatic_d_t *d, int level, int quantity,
                        int range, int index, double limit, double actual );


AA_API int somatic_d_check( somatic_d_t *d, int priority, int code,
                            int test, const char *type, const char fmt[], ... ) {
    if( 0 != test ) {
        va_list argp;
        va_start( argp, fmt );
        somatic_d_vevent(d, priority, code, type, fmt, argp);
        va_end( argp );
    }
}
AA_API void somatic_d_die() {
    abort();
}
