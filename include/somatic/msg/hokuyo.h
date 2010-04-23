/*
 * hokuyo.h
 *
 *  Created on: Apr 01, 2010
 *      Author: Hai-Ning Wu
 */

#ifndef HOKUYO_H_
#define HOKUYO_H_

/* calling C functions in CPP */
#ifdef __cplusplus
extern "C" {
#endif

#include "somatic.h"
#include "somatic/util.h"
#include "somatic.pb-c.h"



// <#define any macros describing your channel>
#define HOKUYO_CHANNEL_NAME "hokuyo-data"
#define HOKUYO_CHANNEL_FRAMES (1128*2)

// Include headers for your device, if necessary


// Allocate a message
Somatic__Hokuyo* somatic_hokuyo_allocate_msg(int nranges);

// Free memory allocated by Somatic__Hokuyo_allocate_msg
int somatic_hokuyo_free(Somatic__Hokuyo *msg);

// Publish message on Ach channel
int somatic_hokuyo_publish(Somatic__Hokuyo *msg, ach_channel_t *chan);

/**
 * Declare a receive function for hokuyo message type
 */
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_hokuyo_receive,
							somatic__hokuyo,
							Somatic__Hokuyo);

// Print the contents of your message type
void somatic_hokuyo_print(Somatic__Hokuyo *msg);

#ifdef __cplusplus
}
#endif

#endif /* HOKUYO_H_ */
