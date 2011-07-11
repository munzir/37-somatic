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
#include <unistd.h>
#include "somatic.h"
#include "somatic/daemon.h"

#include <syslog.h>

#define SOMATIC_SYSLOG_IDENT "somatic"
#define SOMATIC_SYSLOG_FACILITY LOG_USER
#define SOMATIC_SYSLOG_OPTION (LOG_CONS | LOG_NDELAY | LOG_PERROR)

#define HOSTNAME_MAX_SIZE 512

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
    {
        int r;
        if( 0 != (r = ach_open(&d->chan_event, "event", NULL)) ) {
            syslog(LOG_EMERG, "Couldn't open event channel: %s\n",
                   ach_result_to_string(r));
            exit(1);
        }
    }

    // set pid
    d->pid = getpid();

    // set ident
    d->ident = strdup((opts && opts->ident) ? opts->ident : "somatic");

    // set host
    {
        char buf[HOSTNAME_MAX_SIZE];
        int r = gethostname(buf,HOSTNAME_MAX_SIZE-2);
        if( 0 == r ) {
            d->host = strdup(buf);
        }else{
            d->host = strdup("0.0.0.0");
            syslog(LOG_ERR, "Couldn't get hostname: %s", strerror(errno));
        }
    }


    // log direct
    syslog(LOG_NOTICE, "init daemon");

    // install signale handler
    somatic_sighandler_simple_install();

    // set state
    d->is_initialized = 1;

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
    pb.host = d->host;
    pb.pid = d->pid;
    pb.has_pid = 1;
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



static int somatic_d_vcheck( somatic_d_t *d, int priority, int code,
                             int test, const char *type,
                             const char fmt[], va_list argp ) {
    if( !test ) {
        va_list argp2;
        va_copy( argp2, argp );
        if( d && d->is_initialized ) {
            somatic_d_vevent(d, priority, code, type, fmt, argp);
        }else {
            fprintf(stderr, "no valid somatic context, can't send event\n");
        }

        fprintf(stderr, "[%s]:%d (%d).(%s) ",
                d ? d->ident : "unknown",
                priority, code, type ? type : "");
        vfprintf(stderr, fmt, argp );
        fputc('\n', stderr);
        va_end(argp2);
    }
    return test;
}

AA_API int somatic_d_check( somatic_d_t *d, int priority, int code,
                            int test, const char *type, const char fmt[], ... ) {
    if( !test ) {
        va_list argp;
        va_start( argp, fmt );
        somatic_d_vcheck( d, priority, code, test, type, fmt, argp );
        va_end( argp );
    }
    return test;
}

AA_API void somatic_d_die(somatic_d_t *d) {
    (void)d;
    abort();
}

void somatic_d_channel_open(somatic_d_t *d,
                                   ach_channel_t *chan, const char *name,
                                   ach_attr_t *attr) {
    int r =  ach_open( chan, name, attr );
    somatic_d_check(d, SOMATIC__EVENT__PRIORITIES__EMERG,
                    SOMATIC__EVENT__CODES__COMM_FAILED_TRANSPORT,
                    ACH_OK == r, "ach_open",
                    "opening channel `%s': %s\n",
                    name, ach_result_to_string(r));
    if( ACH_OK != r ) somatic_d_die(d);
    r =  ach_flush( chan );
    somatic_d_check(d, SOMATIC__EVENT__PRIORITIES__EMERG,
                    SOMATIC__EVENT__CODES__COMM_FAILED_TRANSPORT,
                    ACH_OK == r, "fflush",
                    "flushing channel `%s': %s\n",
                    name, ach_result_to_string(r));
    if( ACH_OK != r ) somatic_d_die(d);
}

void somatic_d_channel_close(somatic_d_t *d, ach_channel_t *chan ) {
    int r =  ach_close( chan );
    // not much to do if it fails, just log it
    somatic_d_check(d, SOMATIC__EVENT__PRIORITIES__ERR,
                    SOMATIC__EVENT__CODES__COMM_FAILED_TRANSPORT,
                    ACH_OK == r, "ach_close",
                    "closing channel: %s\n",
                    ach_result_to_string(r));
}

AA_API void somatic_d_limit( somatic_d_t *d, int level,
                             const char *type, int quantity,
                             int idx, double actual,
                             double min, double max ) {

    Somatic__Event pb;
    memset(&pb, 0, sizeof pb);
    somatic__event__init(&pb);
    pb.priority = level;
    pb.has_priority = 1;
    pb.code = SOMATIC__EVENT__CODES__LIMIT;
    pb.has_code = 1;
    pb.ident = d->ident;
    pb.host = d->host;
    pb.pid = d->pid;
    pb.has_pid = 1;
    char cpy_type[ type ? strlen(type)+2 : 0 ];
    if(type) {
        strcpy(cpy_type, type);
        pb.type = cpy_type;
    }
    // FIXME: add limit stuff


    int r = SOMATIC_PACK_SEND( &d->chan_event, somatic__event, &pb );
    if( ACH_OK != r ) {
        syslog( LOG_ERR, "couldn't send event: %s",
                ach_result_to_string(r));
    }

    fprintf(stderr, "[%s]:%d (%d).(%s) LIMIT\n",
            d ? d->ident : "unknown",
            pb.priority, pb.code, type ? type : "");

 };

AA_API int somatic_d_check_v( somatic_d_t *d, int priority,
                              int code,
                              const char *type,
                              double *data, size_t n,
                              double *min, double *max, size_t n_desired ) {
    int r = aa_valid_v( data, n, min, max, n_desired );
    somatic_d_check( d, priority, code,
                     r >= 0 && n == n_desired, type,
                     "length mismatch: %d given, %d wanted", n, n_desired );
    if( r > 0 ) {
        int idx = r-1;
        somatic_d_limit( d, priority, type, 0,
                         idx, data[idx], min[idx], max[idx] );

    }
    return 0 == r ? 1 : 0;
}

AA_API int somatic_d_check_param( somatic_d_t *d, int test_val,
                                  const char *file_name,
                                  unsigned int line_no,
                                  const char *fun_name,
                                  const char *test_exp) {
    if( !test_val ) {
        somatic_d_check( d, SOMATIC__EVENT__PRIORITIES__ERR,
                         SOMATIC__EVENT__CODES__BAD_PARAM,
                         test_val,
                         fun_name, "%s:%d `%s'",
                         file_name, line_no, test_exp );
    }
    return test_val;
}

AA_API int somatic_d_check_msg( somatic_d_t *d, int test,
                                const char *type, const char *fmt, ... ) {
    if( !test) {
        va_list argp;
        va_start( argp, fmt );
        somatic_d_vcheck( d, SOMATIC__EVENT__PRIORITIES__ERR,
                          SOMATIC__EVENT__CODES__COMM_BAD_MSG,
                          test, type, fmt, argp );
        va_end(argp);
    }
    return test;
}

AA_API int somatic_d_check_msg_v( somatic_d_t *d, const char *type,
                              double *data, size_t n,
                              double *min, double *max, size_t n_desired ) {
    return somatic_d_check_v( d, SOMATIC__EVENT__PRIORITIES__ERR,
                              SOMATIC__EVENT__CODES__COMM_BAD_MSG,
                              type, data, n, min, max, n_desired );
}
