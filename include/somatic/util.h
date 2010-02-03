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

#ifndef SOMATIC_UTIL_H
#define SOMATIC_UTIL_H

extern int somatic_opt_verbosity;


#define SOMATIC_UNUSED_ARG(expr) do { (void)(expr); } while (0)

/*------*/
/* MISC */
/*------*/

// Fortran modulo, Ada mod
static inline int64_t somatic_modulo( int a, int b ) {
    return ((a % b) + b) % b;
}

// Fortran mod, Ada rem
static inline int64_t somatic_remainder( int a, int b ) {
    return a % b;
}

/*----------------*/
/* Time Functions */
/*----------------*/

static struct timespec somatic_make_timespec( time_t sec, long nsec ) {
    struct timespec t;
    t.tv_sec = sec;
    t.tv_nsec = nsec;
    return t;
}


static struct timespec somatic_make_timespec_norm( time_t sec, long nsec ) {
    // FIXME: this is crap
    int32_t billion = 1e9;
    if( nsec > billion ){
        sec += nsec/billion;
        nsec %= billion;
    }else if (nsec < -billion) {
        sec += (nsec/billion - 1);
        nsec = somatic_modulo( nsec, billion );
    }


    return somatic_make_timespec( sec, nsec );
}

/*
static struct timespec somatic_timespec_delta( struct timespec t1,
                                               struct timespec t0 ) {
    return somatic_make_timespec( t1.tv_sec - t0.tv_sec,
                                  t1.tv_nsec - t0.tv_nsec );
}
*/

static struct timespec somatic_timespec_add( struct timespec t1,
                                               struct timespec t0 ) {
    return somatic_make_timespec_norm( t1.tv_sec + t0.tv_sec,
                                       t1.tv_nsec + t0.tv_nsec );
}


static struct timespec somatic_timespec_now() {
    struct timespec t;
    int r = clock_gettime( CLOCK_MONOTONIC, &t );
    return t;
}

/** returns reltime + now */
static struct timespec somatic_timespec_future( struct timespec reltime ) {
    return somatic_timespec_add( reltime, somatic_timespec_now() );
}

/** t1 < t2: negative; t1 == t2: 0; t1 > t2: positive */
static int somatic_timespec_cmp( struct timespec t1, struct timespec t2 ) {
    return ( t1.tv_sec != t2.tv_sec ) ?
        (t1.tv_sec - t2.tv_sec) :
        (t1.tv_nsec - t2.tv_nsec);
}

static int somatic_timespec_after( struct timespec abstime ) {
    return somatic_timespec_cmp(somatic_timespec_now(), abstime) > 0;
}

static int64_t somatic_timespec2us( struct timespec t ) {
    return t.tv_sec*1000000 + t.tv_nsec/1000;
}

static struct timespec somatic_s2timespec( double t ) {
    int sec = t;
    int nsec = (t-sec)*1e9;
    return somatic_make_timespec_norm( t, nsec );
}

static void somatic_timespec_dump( struct timespec t ) {
    fprintf( stderr, "{.tv_sec = %ld, .tv_nsec = %ld}\n",
             t.tv_sec, t.tv_nsec );
}

/*----------------*/
/* Printing/Error */
/*----------------*/

/** printf's message if level <= somatic_opt_verbosity */
void somatic_verbprintf( int level, const char fmt[], ... );

/** printf's message and exits */
void somatic_fail( const char fmt[], ... );

/** If test is false, printf's message and exits */
void somatic_hard_assert( int test, const char fmt[], ... );


/*--------*/
/* Arrays */
/*--------*/

static double *somatic_malloc_real( size_t n ) {
    return malloc( sizeof(double) * n );
}

static void somatic_realcpy( double *dst, double *src, size_t n ) {
    memcpy( dst, src, sizeof( dst[0] ) * n );
}


static void somatic_realset( double *dst, double val, size_t n ) {
    for( size_t i = 0; i < n; i ++ )
        dst[i] = val;
}


#endif // SOMATIC_UTIL_H


