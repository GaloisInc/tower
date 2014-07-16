
#include "grtos_time_wrapper.h"

#include <FreeRTOS.h>
#include <task.h>

void ivory_grtos_time_delay(uint32_t ticks)
{   vTaskDelay(ticks);
}

void ivory_grtos_time_delayuntil(uint32_t *lastwaketicks, uint32_t ticks)
{
    vTaskDelayUntil(lastwaketicks, ticks);
}

uint32_t ivory_grtos_time_gettickcount(void) {
    return xTaskGetTickCount();
}

uint32_t ivory_grtos_time_gettickrate_ms(void)
{
  return portTICK_RATE_MS;
}

