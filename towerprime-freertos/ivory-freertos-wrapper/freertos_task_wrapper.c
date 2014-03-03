
#include "freertos_task_wrapper.h"

#include <FreeRTOS.h>
#include <task.h>

void ivory_freertos_task_create(void (*tsk)(void),
        uint32_t stacksize, uint8_t priority)
{
    portBASE_TYPE created = xTaskCreate(
            (void (*)(void*)) tsk, /* this cast is undefined behavior */
            NULL, stacksize, NULL, priority, NULL);
    if (created != pdPASS) {
        for(;;);
    }
}

void ivory_freertos_task_delay(uint32_t time_ms)
{   vTaskDelay(time_ms);
}

void ivory_freertos_task_delayuntil(uint32_t *lastwaketime, uint32_t dt)
{
    vTaskDelayUntil(lastwaketime, dt);
}

uint32_t ivory_freertos_task_getmilliscount(void) {
    return xTaskGetTickCount() / portTICK_RATE_MS;
}

uint32_t ivory_freertos_task_gettickcount(void) {
    return xTaskGetTickCount();
}

uint32_t ivory_freertos_millistoticks(uint32_t ms)
{
  return ms * portTICK_RATE_MS;
}
