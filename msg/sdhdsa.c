/*
 * dsa_smm.c
 *
 *  Created on: Apr 2, 2010
 *      Author: Hyun-Soo Yi
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <ach.h>

#include "somatic/msg/base.h"
#include "somatic/msg/sdhdsa.h"
//#include "include/dsawrap.h"

/*
 * Allocate a Somatic__Joystick message
 */
Somatic__Dsa *somatic_dsa_alloc(size_t n_data) {

	Somatic__Dsa *msg = SOMATIC_NEW(Somatic__Dsa);
	somatic__dsa__init(msg);

	msg->output = somatic_ivector_alloc(n_data);

	return (msg);
}
/**
 * Free memory allocated to Somatic__Joystick msg
 */
int somatic_dsa_free(Somatic__Dsa *msg) {
	somatic_ivector_free(msg->output);
	free(msg);

	return (0);
}


/**
 * Publish a Somatic__Joystick message on specified channel
 */
int somatic_dsa_publish(Somatic__Dsa *msg, ach_channel_t *chan) {
	int r = SOMATIC_PACK_SEND( chan, somatic__dsa, msg );
	somatic_hard_assert(ACH_OK == r, "Failed to send message: %s\n",
			ach_result_to_string(r));

	return (r);
}

//Already defined in sdhdsa.h file. Do I have to redefine?
///*
// * Defines a receive function
// */
//SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_dsa_receive,
//							somatic__dsa,
//							Somatic__Dsa);

/*
 * Print the contents of a Somatic__Dsa message
 */
void somatic_dsa_print(Somatic__Dsa *msg){
	int i,j,k;
	//int total = 0;

	int64_t output[6][14][6];

	somatic_dsa_decode(msg,output);

	for (i = 0; i < 14; i++) {
		for (j = 0; j < 6; j++) {
			for (k = 0; k < 6; k++) {
				fprintf(stderr, "%lld ",output[j][i][k]);
			}
			fprintf(stderr, "   ");
		}
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n");
}

/*
 * Decode dsa msg to a 6 x 16 x 6 array.
 */
void somatic_dsa_decode(Somatic__Dsa *msg, int64_t output[6][14][6]){
	int i,j,k;
	int total = 0;
	for (i = 0; i < 6; i++) {
		for (j = 0; j < 14; j++) {
			for (k = 0; k < 6; k++) {
				output[i][j][k] = msg->output->data[total++];
			}
		}
	}
}

