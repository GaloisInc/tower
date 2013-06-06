
#ifndef __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__
#define __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__

#include <stdbool.h>
#include <stdint.h>

/* we're using uint8_t to stand in for void, because ivory doesn't have a
 * concept of void. an xSemaphoreHandle is a void*, so a uint8_t** is analogous
 * to a xSemaphoreHandle*, so we must deref the argument value once before using
 * it.*/
void ivory_freertos_semaphore_create(uint8_t** semhandle);

bool ivory_freertos_semaphore_take(uint8_t** semhandle, uint32_t max_delay);

void ivory_freertos_semaphore_takeblocking(uint8_t** semhandle);

bool ivory_freertos_semaphore_takenonblocking(uint8_t** semhandle);

void ivory_freertos_semaphore_give(uint8_t** semhandle);

#endif // __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__
