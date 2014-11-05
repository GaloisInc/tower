
#include "freertos_semaphore_wrapper.h"

#include "FreeRTOS.h"
#include "semphr.h"


void ivory_freertos_semaphore_create_mutex(uint8_t **semhandle)
{
    *((xSemaphoreHandle*)semhandle) = xSemaphoreCreateMutex();
}

void ivory_freertos_semaphore_create_counting(uint8_t **semhandle, uint32_t max, uint32_t init)
{
    *((xSemaphoreHandle*)semhandle) = xSemaphoreCreateCounting(max,init);
}

void ivory_freertos_semaphore_create_binary(uint8_t **semhandle)
{
    vSemaphoreCreateBinary(*semhandle);
}

bool ivory_freertos_semaphore_take(uint8_t **semhandle, uint32_t max_delay)
{
    xSemaphoreHandle sem = *((xSemaphoreHandle*)semhandle);
    if (xSemaphoreTake(sem, max_delay) == pdTRUE)
        return true;
    else
        return false;
}

void ivory_freertos_semaphore_takeblocking(uint8_t **semhandle)
{
    xSemaphoreHandle sem = *((xSemaphoreHandle*)semhandle);
    xSemaphoreTake(sem, portMAX_DELAY);
}

bool ivory_freertos_semaphore_takenonblocking(uint8_t **semhandle)
{
    return ivory_freertos_semaphore_take(semhandle,0);
}

void ivory_freertos_semaphore_give(uint8_t **semhandle)
{
    xSemaphoreHandle sem = *((xSemaphoreHandle*)semhandle);
    xSemaphoreGive(sem);
}

void ivory_freertos_semaphore_give_from_isr(uint8_t **semhandle)
{
    xSemaphoreHandle sem = *((xSemaphoreHandle*)semhandle);
    portBASE_TYPE higherPriorityTaskWoken;
    xSemaphoreGiveFromISR(sem, &higherPriorityTaskWoken);

    // On the ARM_CM3/CM4F port, yield from ISR uses PENDSV, which our ISR is
    // higher priority than. So, we will not switch context until the ISR is
    // complete.  Break this invariant and you will surely have to redesign the
    // system.

    if (higherPriorityTaskWoken == pdTRUE)
        vPortYieldFromISR();
}

