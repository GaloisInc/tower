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

#ifndef __KERNEL_H__
#define __KERNEL_H__
#include "grtos_FreeRTOS.h"
#include "grtos_tcb_type.h"

void task_create(
	struct task_control_block *tcb,
	void (*entry)(void),
	uint32_t *stack_start,
	uint32_t *stack_end,
	const char *name,
	uint8_t priority);

void scheduler_start(void);

void kernel_switch_context(void);
void kernel_wait(void);

#define kernelENABLE_INTERRUPTS()   portENABLE_INTERRUPTS()
#define kernelDISABLE_INTERRUPTS()  portDISABLE_INTERRUPTS()
#define kernelYIELD()               portYIELD()

#endif // __KERNEL_H__

