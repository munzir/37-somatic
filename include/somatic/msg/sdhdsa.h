/*
 * sdhdsa.h
 *
 *  Created on: Apr 2, 2010
 *      Author: Hyun-Soo Yi
 */

#ifndef DSA_SMM_H_
#define DSA_SMM_H_

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus

//==================================================
//definitions
typedef void* DSApointer;


//==================================================
//function prototypes
int somatic_dsa_allocate_msg(Somatic__Dsa *msg, size_t n_data);
int somatic_dsa_free_msg(Somatic__Dsa *msg);
int somatic_dsa_publish(Somatic__Dsa *msg, ach_channel_t *chan);
void somatic_dsa_print(Somatic__Dsa *msg);
void somatic_dsa_decode(Somatic__Dsa *msg, int64_t output[6][14][6]);
// Declares a receive function
SOMATIC_DEC_WAIT_LAST_UNPACK(somatic_dsa_receive,
							somatic__dsa,
							Somatic__Dsa);

#ifdef __cplusplus
}
#endif //__cplusplus

#endif /* SDHDSA_H_ */
