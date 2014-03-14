
#ifndef __IVORY_FREERTOS_TIME_WRAPPER_H__
#define __IVORY_FREERTOS_TIME_WRAPPER_H__

#include <stdint.h>

void ivory_freertos_time_delay(uint32_t ticks);

void ivory_freertos_time_delayuntil(uint32_t *lastwaketicks, uint32_t ticks);

uint32_t ivory_freertos_time_gettickcount(void);

uint32_t ivory_freertos_time_gettickrate_ms(void);


#endif // __IVORY_FREERTOS_TIME_WRAPPER_H__
