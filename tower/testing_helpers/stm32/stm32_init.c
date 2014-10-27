
#include "stm32_init.h"

extern uint32_t _sidata;
extern uint32_t _sdata;
extern uint32_t _edata;
extern uint32_t _sbss;
extern uint32_t _ebss;

void init_relocate(void){
	uint32_t *src, *dst;
	/* relocate sidata to sdata */
	src = &_sidata;
	dst = &_sdata;
	if (src != dst) {
		while (dst < &_edata) {
			*dst++ = *src++;
		}
	}
	/* zero bss */
	dst = &_sbss;
	while (dst < &_ebss) {
		*dst++ = 0;
	}
}

extern void __libc_init_array(void);

void init_libc(void) {
	__libc_init_array();
}

#include <FreeRTOS.h>
#include <task.h>
extern void tower_entry(void);

static void tower_launch_task(void *machtnichts) {
	tower_entry();
	for(;;);
}

int main (void) {
	xTaskCreate(tower_launch_task, (signed char *)"twrlaunch",
		256, NULL, 0, NULL);
	vTaskStartScheduler();
	for(;;);
	return 0; // unreachable
}

