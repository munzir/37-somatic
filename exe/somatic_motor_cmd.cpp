/*
 * Copyright (c) 2011, Georgia Tech Research Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted
 * provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notice, this list of
 *       conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright notice, this list of
 *       conditions and the following disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *     * Neither the name of the Georgia Tech Research Corporation nor the names of its
 *       contributors may be used to endorse or promote products derived from this software without
 *       specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY GEORGIA TECH RESEARCH CORPORATION ''AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL GEORGIA TECH RESEARCH
 * CORPORATION BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <somatic.h>
#include <somatic/daemon.h>
#include <somatic.pb-c.h>
#include <somatic/motor.h>
#include <ach.h>
#include <unistd.h>

#include <iomanip>

#include <argp.h>

/* ##########################################################################
   ##########################################################################
   #### options and variables
   ##########################################################################
   ########################################################################## */

somatic_d_t daemon_cx;

static char cmd_chan_name[1024];
static char state_chan_name[1024];
static int cmd_type = SOMATIC__MOTOR_PARAM__MOTOR_POSITION;

static double cmd_values[32];
static int motor_size;

/* ##########################################################################
   ##########################################################################
   #### main
   ##########################################################################
   ########################################################################## */

int main(int argc, char* argv[]) {
    somatic_motor_t motor;

    // parse options
    memset(cmd_chan_name, 0, 1024);
    memset(state_chan_name, 0, 1024);
    sprintf(cmd_chan_name, "%s-cmd", argv[1]);
    sprintf(state_chan_name, "%s-state", argv[1]);

    if (strcmp("pos", argv[2]) == 0) cmd_type = SOMATIC__MOTOR_PARAM__MOTOR_POSITION;
    else if (strcmp("vel", argv[2]) == 0) cmd_type = SOMATIC__MOTOR_PARAM__MOTOR_VELOCITY;
    else if (strcmp("cur", argv[2]) == 0) cmd_type = SOMATIC__MOTOR_PARAM__MOTOR_CURRENT;
    else if (strcmp("halt", argv[2]) == 0) cmd_type = SOMATIC__MOTOR_PARAM__MOTOR_HALT;
    else if (strcmp("reset", argv[2]) == 0) cmd_type = SOMATIC__MOTOR_PARAM__MOTOR_RESET;
    else {
	    fprintf(stderr, "Must specify a valid command type: 'pos', 'vel', 'cur', 'halt', or 'reset'\n");
	    exit(EXIT_FAILURE);
    }

    // init daemon
    somatic_d_opts_t daemon_opt;
    memset(&daemon_opt, 0, sizeof(daemon_opt)); // zero initialize
    daemon_opt.ident = "somatic_motor_cmd";
    somatic_d_init(&daemon_cx, &daemon_opt);

	// read values out of standard input
	motor_size = 0;
	double tempd;
	int tempi;
	while(std::cin.good()) {
		std::cin.clear();
		std::cin >> tempd;
		if (std::cin.good()) {
			cmd_values[motor_size++] = tempd;
			continue;
		}

		std::cin.clear();
		std::cin >> std::hex >> tempi;
		if (std::cin.good()) {
			cmd_values[motor_size++] = (double)tempi;
			continue;
		}
	}
	if (!std::cin.eof()) {
		fprintf(stderr, "failed\n");
		exit(EXIT_FAILURE);
	}

	// print out what we plan to send
	std::cout << "Sending ";
	switch(cmd_type) {
		case SOMATIC__MOTOR_PARAM__MOTOR_POSITION: std::cout << "position "; break;
		case SOMATIC__MOTOR_PARAM__MOTOR_VELOCITY: std::cout << "velocity "; break;
		case SOMATIC__MOTOR_PARAM__MOTOR_CURRENT: std::cout << "current "; break;
		case SOMATIC__MOTOR_PARAM__MOTOR_HALT: std::cout << "halt "; break;
		case SOMATIC__MOTOR_PARAM__MOTOR_RESET: std::cout << "reset "; break;
	}
	std::cout << "message to " << motor_size << " motors using channels "
	          << cmd_chan_name << " and " << state_chan_name << std::endl;
	for(int i = 0; i < motor_size; i++) std::cout << std::setw(12) << std::right << std::fixed << cmd_values[i];
	std::cout << std::endl;

    // init motor
    somatic_motor_init(&daemon_cx, &motor, motor_size, cmd_chan_name, state_chan_name);
    
    // open the motor up
    somatic_motor_reset(&daemon_cx, &motor);
    
    // set the joint limits to $bignum
    double** motor_minimum_values[] = { &motor.pos_valid_min, &motor.vel_valid_min,
                                       &motor.pos_limit_min, &motor.vel_limit_min };
    double** motor_maximum_values[] = { &motor.pos_valid_max, &motor.vel_valid_max,
                                       &motor.pos_limit_max, &motor.vel_limit_max };
    for(size_t i = 0; i < 4; i++) aa_fset(*motor_minimum_values[i], -1024.1, motor_size);
    for(size_t i = 0; i < 4; i++) aa_fset(*motor_maximum_values[i], 1024.1, motor_size);

    // start our daemon running
    somatic_d_event(&daemon_cx, SOMATIC__EVENT__PRIORITIES__NOTICE, SOMATIC__EVENT__CODES__PROC_RUNNING, NULL, NULL);

    // update the motor
    somatic_motor_update(&daemon_cx, &motor);
    
    // wait a bit
    usleep(1e5);

    // construct the command and send it
    somatic_motor_cmd(&daemon_cx, &motor, cmd_type, cmd_values, motor_size, NULL);
    
    // Send stopping event
    somatic_d_event(&daemon_cx, SOMATIC__EVENT__PRIORITIES__NOTICE, SOMATIC__EVENT__CODES__PROC_STOPPING, NULL, NULL);

    // close the motor
    somatic_motor_destroy(&daemon_cx, &motor);

    // Destroy daemon resources
    somatic_d_destroy(&daemon_cx);

    // and we're done
    exit(0);
}