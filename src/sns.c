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

/** Author: Neil Dantam
 */

#include "somatic.h"
#include "somatic/daemon.h"
#include <somatic.pb-c.h>
#include <argp.h>
#include <ach.h>
#include <unistd.h>
#include <syslog.h>

/// argp program version
const char *argp_program_version = "sns 0.0";
#define ARGP_DESC "somatic control program"

/*------------*/
/* PROTOTYPES */
/*------------*/

typedef struct {
} cx_t;

static int sns_kill(const char *ident);

/// argp object
static int parse_opt( int key, char *arg, struct argp_state *state);
extern struct argp_option argp_options[];
extern struct argp argp;



/* ------- */
/* GLOBALS */
/* ------- */

aa_region_t memreg;


/* ------- */
/* HELPERS */
/* ------- */

static int sns_kill(const char *ident) {
    // get pid
    char *nam = aa_region_printf(&memreg, SOMATIC_RUNROOT"%s/pid", ident);
    FILE *fp = fopen(nam, "r");
    if( NULL == fp ) {
        fprintf(stderr, "Couldn't open %s: %s\n", nam, strerror(errno));
        return -1;
    }
    pid_t pid = 0;
    fscanf(fp, "%d", &pid);
    fclose(fp);
    if( !pid ) {
        fprintf(stderr, "Invalid pid\n");
        return -1;
    }
    // kill pid
    int r = kill(pid, SIGTERM);
    if( r != 0 ) {
        fprintf(stderr, "Couldn't kill `%s': %s\n", ident, strerror(errno));
    }
    return 0;
}


/* ---- */
/* MAIN */
/* ---- */
int main( int argc, char **argv ) {
    aa_region_init(&memreg, 4*1<<10);
    argp_parse (&argp, argc, argv, 0, NULL, NULL);
    //static cx_t cx;

    return 0;
}


int parse_opt( int key, char *arg, struct argp_state *state) {
    (void)state;
    switch (key) {
    case 'k':
        if(sns_kill(arg)) exit(EXIT_FAILURE);
        break;
    }
    return 0;
}

/* ---------- */
/* ARGP Junk  */
/* ---------- */

struct argp_option argp_options[] = {
    {
        .name = "kill",
        .key = 'k',
        .arg = "IDENT",
        .flags = 0,
        .doc = "Kill daemon `IDENT'"
    },
    {
        .name = NULL,
        .key = 0,
        .arg = NULL,
        .flags = 0,
        .doc = NULL
    }
};

/// argp object
struct argp argp = { argp_options, parse_opt,
                     "args", ARGP_DESC,
                     NULL, NULL, NULL };
