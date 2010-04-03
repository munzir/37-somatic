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

/**
 * \param chan: ach channel to send over
 * \param type: protobuf message type string (i.e. somatic__vector,
 *  			NOT actual Somatic__Vector type)
 * \param msg: pointer to the protobuf message
 */
#define SOMATIC_PACK_SEND( chan, type, msg )                    \
    ({                                                          \
        size_t _somatic_private_n =                             \
            type ## __get_packed_size(msg);                     \
        uint8_t _somatic_private_buf[_somatic_private_n];       \
        type ## __pack( msg, &_somatic_private_buf[0] );        \
        ach_put( chan, _somatic_private_buf,                    \
                 _somatic_private_n );                          \
    })

/**
 * \param ret: ach return code
 * \param type: protobuf message type string (i.e. somatic__vector,
 *  			NOT actual Somatic__Vector type)
 * \param alloc: protobuf allocator (ie, &protobuf_c_system_allocator)
 * \param size: size of buffer to give ach
 * \param chan: ach channel pointer
 */
#define SOMATIC_GET_LAST_UNPACK( ret, type, alloc, size, chan )         \
    ({                                                                  \
        uint8_t _somatic_private_buf[size];                             \
        size_t _somatic_private_nread;                                  \
        ret = ach_get_last( chan, _somatic_private_buf, size,           \
                            &_somatic_private_nread );                  \
        ( ACH_OK == ret || ACH_MISSED_FRAME == ret ) ?                  \
            type ## __unpack( alloc, _somatic_private_nread,            \
                              _somatic_private_buf ) :                  \
            NULL;                                                       \
    })


#define SOMATIC_DEF_GET_LAST_UNPACK( name, ftype, rtype, size ) \
    rtype *name( ach_channel_t *chan, int *ach_result, \
                 ProtobufCAllocator *alloc) {                           \
        return SOMATIC_GET_LAST_UNPACK( ach_result, ftype, alloc, size, chan ); \
    }

#endif
