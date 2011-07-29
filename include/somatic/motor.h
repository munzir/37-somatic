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


#ifndef SOMATIC_MOTOR_H
#define SOMATIC_MOTOR_H

///< max number of joints,
#define SOMATIC_MOTOR_N_MAX 1024

typedef struct {
    size_t n;    ///< number of joints
    double *pos; ///< position state
    double *vel; ///< velocity state
    double *cur; ///< current state

    double *pos_offset; ///< position offset

    double *pos_limit_max; ///< position limit
    double *pos_limit_min; ///< position limit

    double *vel_limit_max; ///< velocity limit
    double *vel_limit_min; ///< velocity limit

    double *cur_limit_max; ///< current limit
    double *cur_limit_min; ///< current limit


    double *pos_valid_max; ///< maximum valid position
    double *pos_valid_min; ///< minimum valid position

    double *vel_valid_max; ///< maximum valid velocity
    double *vel_valid_min; ///< minimum valid velocity

    Somatic__MotorCmd *cmd_msg;
    ach_channel_t state_chan;
    ach_channel_t cmd_chan;

} somatic_motor_t;

AA_API void somatic_motor_init(somatic_d_t *d, somatic_motor_t *m, size_t n,
                               const char *chan_cmd_name,
                               const char *chan_fb_name );

AA_API void somatic_motor_destroy(somatic_d_t *d, somatic_motor_t *m);

AA_API void somatic_motor_cmd( somatic_d_t *d, somatic_motor_t *m,
                               int cmd_type,
                               double *x, size_t n );
AA_API void somatic_motor_update( somatic_d_t *d, somatic_motor_t *m );



#endif //SOMATIC_MOTOR_H
