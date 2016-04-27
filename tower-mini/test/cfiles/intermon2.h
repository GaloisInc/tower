#ifndef __INTERMON2_H__
#define __INTERMON2_H__

#include <stdlib.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 *  A stub for one of the intermediate monitors. The bool return value
 *  indicates whether valid data was written to the pointer
 */
bool intermon2_get_data(bool*);

void intermon2_put_data(const bool*);

#ifdef __cplusplus
}
#endif

#endif
