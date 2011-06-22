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
    // set category

    // set host

    // set state
}

AA_API void somatic_d_destroy( somatic_d_t *d) {
    int r;
    r = ach_close(&d->chan_event);
    closelog();
}

AA_API void somatic_d_state( somatic_d_t *d, int state );


AA_API void somatic_d_heartbeat( somatic_d_t *d );


AA_API void somatic_d_event( somatic_d_t *d, int level, int code,
                        const char *type, const char comment_fmt[], ... );

AA_API void somatic_d_limit( somatic_d_t *d, int level, int quantity,
                        int range, int index, double limit, double actual );


AA_API int somatic_d_check( somatic_d_t *d, int level, int test,
                            const char fmt[], ... );

AA_API void somatic_d_die() {
    abort();
}
