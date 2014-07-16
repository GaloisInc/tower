
#include "grtos_atomic_wrapper.h"

#include <FreeRTOS.h>

void ivory_grtos_begin_atomic(void) {
    portENTER_CRITICAL();
}

void ivory_grtos_end_atomic(void) {
    portEXIT_CRITICAL();
}
