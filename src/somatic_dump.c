/* -*- mode: C; c-basic-offset: 4  -*- */
/* ex: set shiftwidth=4 expandtab: */
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
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *
 *     * Neither the name of the copyright holder(s) nor the names of
 *       its/their contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER(S) AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include <amino.h>
#include <argp.h>
#include "somatic.h"

// Option Vars
const char *opt_chan_name = NULL;
const char *opt_msg_type = NULL;


ach_channel_t sd_chan;
uint8_t *sd_achbuf;
size_t sd_n_achbuf = 1024;
size_t sd_frame_size;
size_t sd_indent = 0;

/* ---------- */
/* ARGP Junk  */
/* ---------- */

static struct argp_option options[] = {
    {
        .name = "verbose",
        .key = 'v',
        .arg = NULL,
        .flags = 0,
        .doc = "Causes verbose output"
    },
    {
        .name = "chan",
        .key = 'c',
        .arg = "ach channel",
        .flags = 0,
        .doc = "ach channel to send data to"
    },
    {
        .name = "protobuf",
        .key = 'p',
        .arg = "protobuf-msg",
        .flags = 0,
        .doc = "name of protobuf message"
    },
    {
        .name = NULL,
        .key = 0,
        .arg = NULL,
        .flags = 0,
        .doc = NULL
    }
};


/// argp parsing function
static int parse_opt( int key, char *arg, struct argp_state *state);
/// argp program version
const char *argp_program_version = "somatic_dump 0.0";
/// argp program arguments documention
static char args_doc[] = "";
/// argp program doc line
static char doc[] = "dump somatic msg to stdout";
/// argp object
static struct argp argp = {options, parse_opt, args_doc, doc, NULL, NULL, NULL };


static int parse_opt( int key, char *arg, struct argp_state *state) {
    (void) state; // ignore unused parameter
    switch(key) {
    case 'v':
        somatic_opt_verbosity++;
        break;
    case 'c':
        if( strlen(arg) > ACH_CHAN_NAME_MAX ) {
            fprintf(stderr, "ERROR: channel is too long\n");
            exit(1);
        }else {
            opt_chan_name = strdup( arg );
        }
        break;
    case 'p':
        opt_msg_type = strdup( arg );
    case 0:
        break;
    }
    return 0;
}

void read_ach() {
    int r = ach_wait_next( &sd_chan, sd_achbuf, sd_n_achbuf,
                           &sd_frame_size, NULL );
    if( ACH_OVERFLOW == r ) {
        sd_n_achbuf = AA_MAX( sd_frame_size, 2*sd_n_achbuf );
        free( sd_achbuf );
        sd_achbuf = AA_NEW_AR(uint8_t,  sd_n_achbuf );
        read_ach();
        return;
    }
    aa_hard_assert( ACH_OK == r || ACH_MISSED_FRAME == r,
                    "Error reading frame: %s\n",
                    ach_result_to_string( r ) );
}

void init() {
    // buffer
    sd_achbuf = AA_NEW_AR(uint8_t,  sd_n_achbuf );
    // channel
    int r = ach_open( &sd_chan, opt_chan_name, NULL );
    aa_hard_assert( ACH_OK == r, "Couldn't open channel %s\n",
                    opt_chan_name );
    r = ach_flush( &sd_chan );
    aa_hard_assert( ACH_OK == r, "Couldn't flush channel\n");

    somatic_sighandler_simple_install();
}

void indent() {
    char buf[2*sd_indent+1];
    memset( buf, ' ', 2*sd_indent );
    buf[2*sd_indent] = '\0';
    printf("%s", buf);

}


void dump_vector (Somatic__Vector *pb, const char *fmt) {
    for( size_t i = 0; i < pb->n_data; i++ ) {
        printf(fmt, pb->data[i]);
    }
}
void dump_ivector (Somatic__Ivector *pb, const char *fmt) {
    for( size_t i = 0; i < pb->n_data; i++ ) {
        printf(fmt, pb->data[i]);
    }
}

void dump_transform( Somatic__Transform *pb ) {
    indent();
    printf("[Transform] ");
    if( pb->translation ) {
        printf("\t%6.3f\t%6.3f\t%6.3f",
               pb->translation->data[0],
               pb->translation->data[1],
               pb->translation->data[2]);
    } else printf("\t\t\t");
    printf("\t|");
    if( pb->rotation ) {
        printf("\t%6.3f\t%6.3f\t%6.3f\t%6.3f",
               pb->rotation->data[0],
               pb->rotation->data[1],
               pb->rotation->data[2],
               pb->rotation->data[3] );
    }
    printf("\n");
}

void dump_timespec( Somatic__Timespec *pb, const char *name ) {
    indent();
    printf("[%s : Timespec]\t%lld.%09ds\n",
           name, pb->sec, pb->has_nsec ? pb->nsec : 0 );
}

void dump_metadata( Somatic__Metadata *pb ) {
    indent();
    printf("[Metadata] \n");
    sd_indent++;
    if( pb->time ) {
        dump_timespec(pb->time, "time");
    }
    if( pb->until ) {
        dump_timespec(pb->until, "until");
    }
    if( pb->has_type ) {
        indent();
        const char *c;
        switch( pb->type ) {
        case SOMATIC__MSG_TYPE__FORCE_MOMENT:
            c = "ForceMoment";
            break;
        default:
            c = "unknown";
        }
        printf("[type] %s\n", c);
    }
    sd_indent--;
}

void dump_multi_transform( Somatic__MultiTransform *pb ) {
    indent();
    printf("[MultiTransform]\t");
    sd_indent++;
    if( pb->tf ) {
        for( size_t i = 0; i < pb->n_tf; i++ )
            dump_transform(pb->tf[i]);
    }
    if( pb->meta )
        dump_metadata( pb->meta );
    sd_indent--;

}


void dump_force_moment( Somatic__ForceMoment *pb ) {
    indent();
    printf("[ForceMoment]\n");
    sd_indent++;
    indent();
    printf("[force]");
    if( pb->force )
        dump_vector(pb->force, "\t%6.3f");
    printf("\n");
    indent();
    printf("[moment]");
    if( pb->moment )
        dump_vector(pb->force, "\t%6.3f");
    printf("\n");
    if( pb->meta )
        dump_metadata( pb->meta );
    sd_indent--;

}

void dump_joystick( Somatic__Joystick *pb ) {
    indent();
    printf("[Joystick]\n");
    sd_indent++;
    indent();
    printf("[axes]");
    dump_vector(pb->axes, "\t%6.3f");
    printf("\n");
    indent();
    printf("[buttons]\t");
    dump_ivector(pb->buttons, "%d:");
    printf("\n");
    sd_indent--;
}

void dump_motor_cmd( Somatic__MotorCmd *pb ) {
    indent();
    printf("[MotorCmd]\n");
    sd_indent++;
    indent();
    printf("[values]");
    dump_vector(pb->values, "\t%6.3f");
    printf("\n");
    sd_indent--;
}

void run() {
    while( !somatic_sig_received ) {
        read_ach();

        Somatic__BaseMsg *base = somatic__base_msg__unpack( &protobuf_c_system_allocator,
                                                            sd_frame_size, sd_achbuf);
        if( base->meta && base->meta->has_type ) {
            switch ( base->meta->type ) {
            case SOMATIC__MSG_TYPE__FORCE_MOMENT:
            {
                Somatic__ForceMoment *msg =
                    somatic__force_moment__unpack( &protobuf_c_system_allocator,
                                                   sd_frame_size, sd_achbuf);
                dump_force_moment( msg );
                somatic__force_moment__free_unpacked( msg,  &protobuf_c_system_allocator );
                break;
            }
            case SOMATIC__MSG_TYPE__MULTI_TRANSFORM:
            {
                Somatic__MultiTransform *msg =
                    somatic__multi_transform__unpack(&protobuf_c_system_allocator,
                                                     sd_frame_size, sd_achbuf);
                dump_multi_transform(msg);
                somatic__multi_transform__free_unpacked(
                    msg, &protobuf_c_system_allocator);
                break;
            }
            case SOMATIC__MSG_TYPE__JOYSTICK:
            {
                Somatic__Joystick *msg =
                    somatic__joystick__unpack(&protobuf_c_system_allocator,
                                                     sd_frame_size, sd_achbuf);
                dump_joystick(msg);
                somatic__joystick__free_unpacked( msg, &protobuf_c_system_allocator);
                break;
            }
            case SOMATIC__MSG_TYPE__MOTOR_CMD:
            {
                Somatic__MotorCmd *msg =
                    somatic__motor_cmd__unpack(&protobuf_c_system_allocator,
                                               sd_frame_size, sd_achbuf);
                dump_motor_cmd(msg);
                somatic__motor_cmd__free_unpacked( msg, &protobuf_c_system_allocator);
                break;
            }
            default: printf("Unknown Message: %d\n",base->meta->type);
            }
        } else {
            printf("Unknown Message, no type info\n");
        }
        somatic__base_msg__free_unpacked( base, &protobuf_c_system_allocator );

        /* if( 0 == strcasecmp( opt_msg_type, "multi_transform" ) || */
        /*     0 == strcasecmp( opt_msg_type, "multitransform" ) ) { */
        /* } else if( 0 == strcasecmp(opt_msg_type, "force_moment") || */
        /*            0 == strcasecmp( opt_msg_type, "forcemoment" ) ) { */
        /* }else { */
        /*     printf("Unknown Message Type\n"); */
        /* } */
        assert( 0 == sd_indent );
    }
}

void destroy() {
    ach_close(&sd_chan);
}

int main( int argc, char **argv ) {
    // parse options
    argp_parse (&argp, argc, argv, 0, NULL, NULL);
    somatic_verbprintf_prefix="somatic_dump";

    aa_hard_assert( NULL != opt_chan_name, "Must set channel name\n");
    //aa_hard_assert( NULL != opt_msg_type, "Must set protobuf type\n");

    somatic_verbprintf( 1, "Channel %s\n", opt_chan_name );
    somatic_verbprintf( 1, "Protobuf %s\n", opt_msg_type );

    init();
    run();
    destroy();

    return 0;
}

