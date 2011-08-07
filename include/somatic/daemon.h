/* -*- mode: C; c-basic-offset: 4  -*- */
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

#ifndef SOMATIC_DAEMON_H
#define SOMATIC_DAEMON_H


/**  \file daemon.h
 *   \page daemon Daemons
 *
 * This page explains the policy for how the daemons in somatic
 * interact and handle process management, errors, and logging.
 *
 * Daemons to control robots is must operate differently compared to
 * information processing systems, ie httpd.  In particular, failures
 * must be handled much more carefully to minize the chances of
 * physical damage or personal injury.
 *
 * \section approach Approach
 *
 * The general approach for developing each software module here is
 * "Do the right thing."
 *
 * \section event Events
 *
 * Anything notable in the system is communicated as an event.  Events
 * include process startup/shutdown, fault conditions, heartbeat
 * messages.  These events are sent as protobuf messages over ach.
 *
 * \section proc Process Management
 *
 * Processes are managed by the monitor daemon, smond.  Smond has
 * several responsibilities:
 * - Start, stop, and restart any robot-related processes.
 * - Monitor robot processes via signals
 * - Check robot procecess heartbeat.
 * - Respond to operator process management requests.
 *
 * \section log Logging
 *
 * System events sent over ach are relayed to syslog by the slogd
 * daemon.
 *
 * Daemons may also open syslog directly to communicate errors if the
 * ach transport fails or is unavailable.  This should be avoided if
 * possible during realtime code since (1) logging is slow and (2)
 * logging could possibly block.
 *
 * \section err Error Handling
 *
 * Typical approaches to error handling do not apply.  Any
 * interruption of the input commands to the robot's motors could
 * result in significant damage depending on the current position and
 * velocity.  The module receiving the error cannot abort, because
 * that may interrupt the input.  The module cannot throw an
 * exception, and expect someone else to deal with it.  The module
 * cannot fall to a debugger, because the pause waiting for the
 * operator to debug would interrupt the robot's input.
 *
 * Three things must happen when an error occurs.  First, the the
 * operator must be informed of the error so that he can take the
 * correct action to minimize damage.  Second, the controller of the
 * robot must be informed of the error so that it can take the correct
 * action to minize damage. Finally, the module recognizing the error
 * should attempt to correct it if at all possible.

 *
 *
 */


typedef struct {
    const char *ident;
    const char *prefix; ///< unused
    size_t region_size;
    size_t tmpregion_size;
    int skip_sighandler;
    int daemonize;
} somatic_d_opts_t;

/** A somatic daemon context.*/
typedef struct {
    int is_initialized;           ///< is the struct initialized
    ach_channel_t chan_debug;     ///< channel that gets debug events
    ach_channel_t chan_heartbeat; ///< channel that gets heartbeat events
    ach_channel_t chan_event;     ///< channel the gets other events
    pid_t pid;                    ///< pid of this process
    int facility;                 ///< facility code for this daemon
    char *ident;                  ///< identifier for this daemon
    char *host;                   ///< hostname for this daemon
    int state;                    ///< {starting,running,stopping,halted,err}
    aa_region_t memreg;           ///< memory region
    aa_region_t tmpreg;           ///< memory region for temporaries, ie ach buffers, lapack work arrays
    somatic_pbregalloc_t pballoc; ///< protobuf-c allocator that uses memreg
    somatic_d_opts_t opts;        ///< options used for this daemon
    FILE *lockfile;               ///< lock file
} somatic_d_t;

#define SOMATIC_D_DEFAULT_REGION_SIZE    (64 * (1<<10))
#define SOMATIC_D_DEFAULT_TMPREGION_SIZE (16 * (1<<10))

#define SOMATIC_RUNROOT "/var/run/somatic/"

/** Initialize somatic daemon context struct.

    Call this function before doing anything else in your daemon.
 */
AA_API void somatic_d_init( somatic_d_t *d, somatic_d_opts_t *opts );

/** Destroy somatic daemon context struct.

    Call this function before your daemon exists.
 */
AA_API void somatic_d_destroy( somatic_d_t *d);


/** Sets daemon state and posts event message.

    \param state Defined in the event message as PROC_STARTING, PROC_RUNNING,
    PROC_STOPPING, PROC_HALTED, PROC_ERR.
 */
AA_API void somatic_d_state( somatic_d_t *d, int state );


/** Sends heartbeat event.
 */
AA_API void somatic_d_heartbeat( somatic_d_t *d );


/** Sends an event message on the event channel.
 */
AA_API void somatic_d_event( somatic_d_t *d, int level, int code,
                        const char *type, const char comment_fmt[], ... );

/** Sends a limit message on the event channel.
 */
AA_API void somatic_d_limit( somatic_d_t *d, int level,
                             const char *type, int quantity,
                             int index, double actual,
                             double min, double max );

/** Checks that test is true, otherwise logs an event at level.
 * \param priority The severity level of the check
 * \param code The event type to issue on failure
 * \param test The condition to check, true on success
 * \param fmt A format string for the log message.
 * \return The test parameter.
 */
AA_API int somatic_d_check( somatic_d_t *d, int priority, int code,
                            int test, const char *type, const char fmt[], ... );


/** Logs an essertion error if test is false. */
AA_API int somatic_d_assert_err( somatic_d_t *d, int test,
                                 const char fmt[], ... );


/** Checks if data is outside of limits.
 * \return 1 if within limits, zero otherwise
 */
AA_API int somatic_d_check_v( somatic_d_t *d, int priority, int code,
                              const char *type,
                              double *data, size_t n,
                              double *min, double *max, size_t n_desired );

/// Use macro SOMATIC_D_CHECK_PARM instead of calling directly
AA_API int somatic_d_check_param( somatic_d_t *d, int test,
                                  const char *file_name, unsigned int line_no,
                                  const char *fun_name, const char *test_exp);

#define SOMATIC_D_CHECK_PARM( d, test ) \
    somatic_d_check_param( d, test, __FILE__, __LINE__, __func__, #test )

/** Validate a message
 * \returns test parameter
 */
AA_API int somatic_d_check_msg( somatic_d_t *d, int test,
                                const char *type, const char *fmt, ... );
/** Validate a vector in a message
 */
AA_API int somatic_d_check_msg_v( somatic_d_t *d, const char *type,
                                  double *data, size_t n,
                                  double *min, double *max, size_t n_desired );

/** Terminates the process when things get really bad.*/
AA_API void somatic_d_die(somatic_d_t *d);


/** Opens a channel or dies if it can't */
AA_API void somatic_d_channel_open(somatic_d_t *d,
                                   ach_channel_t *chan, const char *name,
                                   ach_attr_t *attr);

/** Closes a channel */
AA_API void somatic_d_channel_close(somatic_d_t *d, ach_channel_t *chan );

/** Gets a frame from the ach channel, storing in memory region
 * d->tmpreg.  If d->tmpreg is too small to hold the next frame, the
 * region will be enlarged and the get() retried.
 *
 * \pre chan is an opened and d is initialized
 *
 * \post The head pointer of d->tmpreg is NOT incremented.  The head
 * pointer of d->tmpreg points to the frame buffer.
 *
 * \return pointer to the frame data buffer, allocated from d->tmpreg
 */
AA_API void *somatic_d_get( somatic_d_t *d, ach_channel_t *chan, size_t *frame_size,
                            const struct timespec *ACH_RESTRICT abstime, int options, int *ret );

/**
 * \param d: somatic_daemon_t context struct
 * \param ret: ach return code
 * \param type: protobuf message type string (i.e. somatic__vector,
 *  			NOT actual Somatic__Vector type)
 * \param alloc: protobuf allocator (ie, &protobuf_c_system_allocator)
 * \param size: size of buffer to give ach
 * \param chan: ach channel pointer
 */
#define SOMATIC_D_GET(ret, type, d, chan, abstime, opts )               \
    ({                                                                  \
        size_t _somatic_private_nread = 0;                              \
        uint8_t *_somatic_private_buf = (uint8_t*)                      \
            somatic_d_get( (d), (chan), &_somatic_private_nread,        \
                           (abstime), (opts), (ret) );                  \
        ( _somatic_private_nread ) ?                                    \
            type ## __unpack( &(d)->pballoc, _somatic_private_nread,     \
                              _somatic_private_buf ) :                  \
            NULL ;                                                      \
    })

/**
 * \param d: daemon context
 * \param chan: ach channel to send over
 * \param type: protobuf message type string (i.e. somatic__vector,
 *  			NOT actual Somatic__Vector type)
 * \param msg: pointer to the protobuf message
 */
#define SOMATIC_D_PUT( type, d, chan, msg )                     \
    ({                                                          \
        size_t _somatic_private_n =                             \
            type ## __get_packed_size(msg);                     \
        uint8_t *_somatic_private_buf =                         \
            aa_region_tmpalloc(&d->tmpreg, _somatic_private_n); \
        type ## __pack( msg, &_somatic_private_buf[0] );        \
        ach_put( chan, _somatic_private_buf,                    \
                 _somatic_private_n );                          \
    })

#endif // SOMATIC_DAEMON_H
