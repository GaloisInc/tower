
#include "freertos_atomic_wrapper.h"

#include <FreeRTOS.h>

void ivory_freertos_begin_atomic(void) {
    portENTER_CRITICAL();
}

void ivory_freertos_end_atomic(void) {
    portEXIT_CRITICAL();
}
