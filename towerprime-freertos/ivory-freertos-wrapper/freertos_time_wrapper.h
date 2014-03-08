
#ifndef __IVORY_FREERTOS_TIME_WRAPPER_H__
#define __IVORY_FREERTOS_TIME_WRAPPER_H__

#include <stdint.h>

void ivory_freertos_task_delay(uint32_t time_ms);

void ivory_freertos_task_delayuntil(uint32_t *lastwaketime, uint32_t dt);

uint32_t ivory_freertos_task_getmilliscount(void);

uint32_t ivory_freertos_task_gettickcount(void);

uint32_t ivory_freertos_millistoticks(uint32_t ms);

#endif // __IVORY_FREERTOS_TIME_WRAPPER_H__
