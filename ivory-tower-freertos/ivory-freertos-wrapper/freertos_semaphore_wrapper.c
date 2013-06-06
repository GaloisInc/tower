
#include "freertos_semaphore_wrapper.h"

#include <FreeRTOS.h>
#include <semphr.h>


void ivory_freertos_semaphore_create(uint8_t **semhandle)
{
    *((xSemaphoreHandle*)semhandle) = xSemaphoreCreateMutex();
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


