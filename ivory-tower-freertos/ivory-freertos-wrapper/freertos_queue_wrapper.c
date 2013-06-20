
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

bool ivory_freertos_queue_send_isr(uint16_t **queuehandle,
        uint32_t value)
{
    xQueueHandle q = *((xQueueHandle*)queuehandle);
    portBASE_TYPE higherPriorityTaskWoken;
    portBASE_TYPE res = xQueueSendFromISR(q, (void*)&value, &higherPriorityTaskWoken);

    // On the ARM_CM3/CM4F port, yield from ISR uses PENDSV, which our ISR is
    // higher priority than. So, we will not switch context until the ISR is
    // complete.  Break this invariant and you will surely have to redesign the
    // system.

    if (higherPriorityTaskWoken == pdTRUE)
        vPortYieldFromISR();

    if (res == pdTRUE)
        return true;
    else
        return false;
}

bool ivory_freertos_queue_receive_isr(uint16_t **queuehandle,
        uint32_t* value)
{
    xQueueHandle q = *((xQueueHandle*)queuehandle);
    portBASE_TYPE higherPriorityTaskWoken;
    portBASE_TYPE res = xQueueReceiveFromISR(q, (void*)value, &higherPriorityTaskWoken);

    // On the ARM_CM3/CM4F port, yield from ISRuses PENDSV, which our ISR
    // is higher priority than. So, we will not switch context until the ISR is
    // complete.
    // Break this invariant and you will surely have to redesign thesystem.

    if (higherPriorityTaskWoken == pdTRUE)
        vPortYieldFromISR();

    if (res == pdTRUE)
        return true;
    else
        return false;
}


uint32_t ivory_freertos_queue_give(uint16_t **queuehandle)
{
    xQueueHandle q = *((xQueueHandle*)queuehandle);
    return (uint32_t) uxQueueMessagesWaiting(q);
}


