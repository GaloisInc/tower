
#include "freertos_queue_wrapper.h"

#include <FreeRTOS.h>
#include <queue.h>


void ivory_freertos_queue_create(uint16_t **queuehandle, uint32_t len)
{
    *((xQueueHandle*)queuehandle) = xQueueCreate(len, sizeof(uint32_t));
}

bool ivory_freertos_queue_send(uint16_t **queuehandle,
        uint32_t value, uint32_t max_delay)
{
    xQueueHandle q = *((xQueueHandle*)queuehandle);
    if (xQueueSend(q, (void*)&value, max_delay) == pdTRUE)
        return true;
    else
        return false;
}

bool ivory_freertos_queue_receive(uint16_t **queuehandle,
        uint32_t* value, uint32_t max_delay)
{
    xQueueHandle q = *((xQueueHandle*)queuehandle);
    if (xQueueReceive(q, (void*)value, max_delay) == pdTRUE)
        return true;
    else
        return false;
}


uint32_t ivory_freertos_queue_give(uint16_t **queuehandle)
{
    xQueueHandle q = *((xQueueHandle*)queuehandle);
    return (uint32_t) uxQueueMessagesWaiting(q);
}


