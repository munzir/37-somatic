/*
 * hokuyo_smm.c
 *
 *  Created on: Apr 01, 2010
 *      Author: Hai-Ning Wu
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>

#include <ach.h>

#include "somatic/msg/base.h"
#include "somatic/msg/hokuyo.h"

/*
 * Allocate a Somatic__Hokuyo message
 */
Somatic__Hokuyo* somatic_hokuyo_allocate_msg(int nranges)
{
	Somatic__Hokuyo *msg = SOMATIC_NEW(Somatic__Hokuyo);
	somatic__hokuyo__init(msg);

	msg->ranges = somatic_vector_alloc(nranges);

	return (msg);
}

int somatic_hokuyo_free(Somatic__Hokuyo *msg)
{
	somatic_vector_free(msg->ranges);
	free(msg);

	return(0);
}

int somatic_hokuyo_publish(Somatic__Hokuyo *msg, ach_channel_t *chan)
{
	int r = SOMATIC_PACK_SEND( chan, somatic__hokuyo, msg );
	//somatic_hard_assert( ACH_OK == r, "Failed to send message: %s\n", ach_result_to_string( r ) );
	somatic_hard_assert( ACH_OK == r, "Failed to send message: \n");

	return(r);
}

/**
 * Defines a receive function for hokuyo message type
 */
SOMATIC_DEF_WAIT_LAST_UNPACK(somatic_hokuyo_receive,
							somatic__hokuyo,
							Somatic__Hokuyo);

/*
 * Print the contents of a Somatic__Hokuyo message
 */
void somatic_hokuyo_print(Somatic__Hokuyo *msg)
{
    int i;
    printf("================================\n");
    for (i=0; i<HOKUYO_CHANNEL_FRAMES; i++)
	printf("%f\n", msg->ranges->data[i]);
}
