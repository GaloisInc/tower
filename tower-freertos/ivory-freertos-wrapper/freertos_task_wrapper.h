
#ifndef __IVORY_FREERTOS_TASK_WRAPPER_H__
#define __IVORY_FREERTOS_TASK_WRAPPER_H__

#include <stdint.h>

struct taskarg {
	uint32_t taskarg_garbage;
};

void ivory_freertos_task_create(void (*tsk)(struct taskarg*),
        uint32_t stacksize, uint8_t priority);

#endif // __IVORY_FREERTOS_TASK_WRAPPER_H__
