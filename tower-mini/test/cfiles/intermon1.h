#ifndef __INTERMON1_H__
#define __INTERMON1_H__

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 *  A stub for one of the intermediate monitors. The bool return value
 *  indicates whether valid data was written to the pointer
 */
bool intermon1_get_data(uint8_t*);

void intermon1_put_data(const uint8_t*);

#ifdef __cplusplus
}
#endif

#endif
