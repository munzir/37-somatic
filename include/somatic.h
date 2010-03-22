/* -*- mode: C; c-basic-offset: 4  -*- */
/*
 * Copyright (c) 2009, Georgia Tech Research Corporation
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


#ifndef SOMATIC_H
#define SOMATIC_H

#include <ach.h>
#include <genmsg.h>

/** \file somatic.h
 *  \author Neil T. Dantam
 */


/*
static inline int somatic_msg_send_buf_stack( ach_channel_t *chan,
                                              size_t buf_size,
                                              void *msg_struct,
                                              int (*encoder)(char *, size_t, void*)) {
    char buf[ buf_size ];
    int r = encoder( buf, buf_size, msg_struct );
    assert( buf_size == r );
    return ach_put( chan, buf, r );
}
*/



/** Sends p_message of type msg_type on channel chan, holding buffer on stack.
 */
#define SOMATIC_MSG_SEND_BUF_STACK( chan, p_msg_struct, msg_type )      \
    ({                                                                  \
        msg_type ## _t *_somatic_private_msg = p_msg_struct;            \
        int _somatic_private_n =                                        \
            msg_type ## _size( _somatic_private_msg );                  \
        char _somatic_private_buf[_somatic_private_n];                  \
        int _somatic_private_r =                                        \
            msg_type ## _encode( _somatic_private_buf,                  \
                                 _somatic_private_n,                    \
                                 _somatic_private_msg );                \
        assert( _somatic_private_n == _somatic_private_r );             \
        ach_put( chan, _somatic_private_buf, (size_t)_somatic_private_r ); \
    })



#endif
