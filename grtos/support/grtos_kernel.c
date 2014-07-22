/*
 * GRTOS Kernel
 *
 * Copyright (C) 2014, Galois, Inc.
 * All Rights Reserved.
 *
 * Portions derived from FreeRTOS 8.0.1 - Copyright (C) Real Time Engineers Ltd.
 * All rights reserved
 *
 * This software is icensed under the same license terms as FreeRTOS:
 *
 *  FreeRTOS is free software; you can redistribute it and/or modify it under
 *  the terms of the GNU General Public License (version 2) as published by the
 *  Free Software Foundation >>!AND MODIFIED BY!<< the FreeRTOS exception.
 *
 *  >>!   NOTE: The modification to the GPL is included to allow you to     !<<
 *  >>!   distribute a combined work that includes FreeRTOS without being   !<<
 *  >>!   obliged to provide the source code for proprietary components     !<<
 *  >>!   outside of the FreeRTOS kernel.                                   !<<
 *
 *  FreeRTOS is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE.  Full license text is available from the following
 *  link: http://www.freertos.org/a00114.html
 *
 */


#include <string.h>
#include "grtos_kernel.h"

#define tskSTACK_FILL_BYTE (0xA5U)

static struct task_control_block _idle_tcb;
static void _idle_task_loop(void);
static uint32_t _idle_stack[32];

volatile struct task_control_block *pxCurrentTCB = NULL;
static volatile uint8_t scheduler_running = 0;

void task_create(
	struct task_control_block *tcb,
	void (*entry)(void),
	uint32_t *stack_start,
	size_t stack_len,
	const char *name,
	uint8_t priority)
{
	configASSERT(scheduler_running == 0)
	configASSERT(tcb != NULL);
	configASSERT(stack_start != NULL);
	configASSERT(stack_len > 0);
	uint32_t *stack_end = &(stack_start[stack_len]);
	configASSERT(stack_start < stack_end);

	memset(stack_start, tskSTACK_FILL_BYTE, stack_len * sizeof(uint32_t));
	tcb->pxStack = stack_start;
	uint32_t *stack_top = (uint32_t *) (((portPOINTER_SIZE_TYPE) stack_end) &
		((portPOINTER_SIZE_TYPE) ~portBYTE_ALIGNMENT_MASK));

	tcb->pxTopOfStack = pxPortInitialiseStack(stack_top, (TaskFunction_t)entry, NULL);

	for( int i = 0; i < configMAX_TASK_NAME_LEN; i++) {
		tcb->pcTaskName[i] = name[i];
		if (name[i] == 0) break;
	}
	tcb->pcTaskName[configMAX_TASK_NAME_LEN-1] = 0;

	tcb->uxPriority = priority;
	tcb->state = TASK_STATE_SUSPENDED;

	if (pxCurrentTCB == NULL || pxCurrentTCB->uxPriority < priority) {
		pxCurrentTCB = tcb;
	}
}

void scheduler_start(void) {
	configASSERT(scheduler_running == 0);
	task_create(&_idle_tcb,
		_idle_task_loop,
		_idle_stack,
		sizeof(_idle_stack),
		"idle",
		0);
	scheduler_running = 1;
	xPortStartScheduler();
}

void kernel_wait(void) {
	kernelDISABLE_INTERRUPTS();
	pxCurrentTCB->state = TASK_STATE_WAITING;
	kernelYIELD(); // Scheduler will defer until after interrupts enabled.
	kernelENABLE_INTERRUPTS();
}

extern struct task_control_block *scheduler_pick_next_task(struct task_control_block * running);

void kernel_switch_context(void) {
	struct task_control_block *running;
	struct task_control_block *scheduled;
	running = (struct task_control_block *) pxCurrentTCB;
	scheduled = scheduler_pick_next_task(running);
	if (running != scheduled) {
		if (running->state == TASK_STATE_RUNNING) {
			running->state = TASK_STATE_SUSPENDED;
		}
	}
	scheduled->state = TASK_STATE_RUNNING;
	if (scheduled == NULL) {
		pxCurrentTCB = &_idle_tcb;
	} else {
		pxCurrentTCB = scheduled;
	}
}

static void _idle_task_loop(void) {
	for(;;);
}

