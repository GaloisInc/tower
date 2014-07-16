
#ifndef __IVORY_GRTOS_SEMAPHORE_WRAPPER_H__
#define __IVORY_GRTOS_SEMAPHORE_WRAPPER_H__

#include <stdbool.h>
#include <stdint.h>

/* we're using uint8_t to stand in for void, because ivory doesn't have a
 * concept of void. an xSemaphoreHandle is a void*, so a uint8_t** is analogous
 * to a xSemaphoreHandle*, so we must deref the argument value once before using
 * it.*/
void ivory_grtos_semaphore_create_mutex(uint8_t** semhandle);

void ivory_grtos_semaphore_create_counting(uint8_t** semhandle, uint32_t max, uint32_t init);

void ivory_grtos_semaphore_create_binary(uint8_t** semhandle);

bool ivory_grtos_semaphore_take(uint8_t** semhandle, uint32_t max_delay);

void ivory_grtos_semaphore_takeblocking(uint8_t** semhandle);

bool ivory_grtos_semaphore_takenonblocking(uint8_t** semhandle);

void ivory_grtos_semaphore_give(uint8_t** semhandle);

void ivory_grtos_semaphore_give_from_isr(uint8_t** semhandle);

#endif // __IVORY_GRTOS_SEMAPHORE_WRAPPER_H__
