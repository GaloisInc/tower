
#ifndef __IVORY_ECHRONOS_TASK_WRAPPER_H__
#define __IVORY_ECHRONOS_TASK_WRAPPER_H__

#include <stdint.h>

void ivory_echronos_task_create(void (*tsk)(void),
        uint32_t stacksize, uint8_t priority);

void ivory_echronos_task_delay(uint32_t time_ms);

void ivory_echronos_task_delayuntil(uint32_t *lastwaketime, uint32_t dt);

uint32_t ivory_echronos_task_getmilliscount(void);

uint32_t ivory_echronos_task_gettickcount(void);

uint32_t ivory_echronos_millistoticks(uint32_t ms);

#endif // __IVORY_ECHRONOS_TASK_WRAPPER_H__
