
#ifndef __IVORY_GRTOS_TASK_WRAPPER_H__
#define __IVORY_GRTOS_TASK_WRAPPER_H__

#include <stdint.h>

struct taskarg {
	uint32_t taskarg_garbage;
};

void ivory_grtos_task_create(void (*tsk)(struct taskarg*),
        uint32_t stacksize, uint8_t priority, const char* const name);

#endif // __IVORY_GRTOS_TASK_WRAPPER_H__
