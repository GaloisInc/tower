
#include "freertos_task_wrapper.h"

#include <FreeRTOS.h>
#include <task.h>

void ivory_freertos_task_create(void (*tsk)(void),
        uint32_t stacksize, uint8_t priority)
{
    xTaskCreate((void (*)(void*)) tsk, /* this cast is undefined behavior */ 
            NULL, stacksize, NULL, priority, NULL);
}

void ivory_freertos_task_delay(uint32_t time_ms)
{
    vTaskDelay(time_ms);    
}

void ivory_freertos_task_delayuntil(uint32_t *lastwaketime, uint32_t dt)
{
    vTaskDelayUntil(lastwaketime,dt);
}

uint32_t ivory_freertos_task_gettickcount(void) {
    return xTaskGetTickCount();
}

