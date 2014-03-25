
#include "freertos_task_wrapper.h"

#include <FreeRTOS.h>
#include <task.h>

void ivory_freertos_task_create(void (*tsk)(struct taskarg*),
        uint32_t stacksize, uint8_t priority)
{
    portBASE_TYPE created = xTaskCreate(
            (void (*)(void*)) tsk,
            NULL, stacksize, NULL, priority, NULL);
    if (created != pdPASS) {
        // FAILURE! possible causes:
        // - FreeRTOS heap is out of memory for allocating stack or tcb
        // - priority level is invalid
        for(;;);
    }
}
