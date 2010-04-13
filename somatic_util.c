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
#include <signal.h>
#include "somatic/includes.h"
#include "somatic/util.h"
#include "somatic/lapack.h"

int somatic_opt_verbosity = 0;
int somatic_sig_received = 0;

void somatic_verbprintf( int level, const char fmt[], ... ) {
    va_list argp;
    va_start( argp, fmt );
    if( level <= somatic_opt_verbosity ) {
        fprintf(stderr, "verb: ");
        vfprintf( stderr, fmt, argp );
    }
    va_end( argp );
}

void somatic_fail( const char fmt[], ... ) {
    va_list argp;
    va_start( argp, fmt );
    fprintf(stderr, "ERROR: ");
    vfprintf( stderr, fmt, argp );
    va_end( argp );
    abort();
    exit(EXIT_FAILURE);
}


void somatic_hard_assert( int test, const char fmt[], ... ) {
    if( ! test ) {
        va_list argp;
        va_start( argp, fmt );
        fprintf(stderr, "ERROR: ");
        vfprintf( stderr, fmt, argp );
        va_end( argp );
        abort();
        exit(EXIT_FAILURE);
    }
}


static void somatic_sighandler_simple (int sig, siginfo_t *siginfo, void *context)
{
    (void) context;
    somatic_verbprintf (1,
                        "Received Signal: %d, Sending PID: %ld, UID: %ld\n",
                        sig, (long)siginfo->si_pid, (long)siginfo->si_uid);
    somatic_sig_received = 1;
}

void somatic_sighandler_simple_install() {
    struct sigaction act;
    memset(&act, 0, sizeof(act));

    act.sa_sigaction = &somatic_sighandler_simple;

    /* The SA_SIGINFO flag tells sigaction() to use the sa_sigaction field,
       not sa_handler. */
    act.sa_flags = SA_SIGINFO;

    if (sigaction(SIGTERM, &act, NULL) < 0) {
        perror ("sigaction");
        somatic_fail( "Couldn't install handler\n");
    }

    if (sigaction(SIGINT, &act, NULL) < 0) {
        perror ("sigaction");
        somatic_fail( "Couldn't install handler\n");
    }

}


int somatic_la_invert( size_t m, size_t n, double *A ) {
    int ipiv[m];
    int mi = (int) m;
    int ni = (int) n;
    int info;

    // LU-factor
    dgetrf_( &mi, &ni, A, &mi, ipiv, &info );

    // find optimal size
    double swork[1];
    int lwork_query = -1;
    dgetri_( &ni, A, &mi, ipiv, swork, &lwork_query, &info );
    int lwork = (int) swork[0];

    // invert
    double work[lwork];
    dgetri_( &ni, A, &mi, ipiv, work, &lwork, &info );

    return info;
}

// for fortran
int somatic_la_invert_( const int *m, const int *n, double *A ) {
    return somatic_la_invert( (size_t)*m, (size_t)*n, A );
}

/*****************************************************************
 * For managing ach channels, so we don't have to use achtool
 *
 * These are just functions pulled from ez.c and achtool.c, so
 * anyone developing a daemon can use them
 */

/*
 * Creates an ach channel
 */
int somatic_create_channel(const char *name, size_t frame_cnt, size_t frame_size) {

    int i;
    {
        ach_create_attr_t attr;
        ach_create_attr_init(&attr);
        i = ach_create( (char*)name, frame_cnt, frame_size, &attr );
    }
    somatic_hard_assert( ACH_OK == i, "Error creating channel: %s\n",
                             ach_result_to_string( i ) );

    return i;
}

/*
 * Opens named ach channel
 */
ach_channel_t* somatic_open_channel(const char *name)
{
	ach_channel_t *chan = SOMATIC_NEW( ach_channel_t );
    int r = ach_open( chan, name, NULL );
    somatic_hard_assert( ACH_OK == r, "Error opening channel: %s\n",
                         ach_result_to_string( r ) );

    // Uncomment to set channel permissions on every open call
    //ach_chmod( chan, SOMATIC_CHANNEL_MODE );

    r = ach_flush( chan );
    somatic_hard_assert( ACH_OK == r, "Error flushing channel: %s\n",
                         ach_result_to_string( r ) );

	return(chan);
}

/*
 * Closes a channel
 */
int somatic_close_channel(ach_channel_t *chan)
{
	int r = ach_close( chan );
	somatic_hard_assert( ACH_OK == r, "Error closing channel: %s\n",
	                         ach_result_to_string( r ) );

	return(r);
}
