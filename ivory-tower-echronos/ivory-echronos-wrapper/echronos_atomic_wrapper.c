
#include "echronos_atomic_wrapper.h"

#include <eChronos.h>

void ivory_echronos_begin_atomic(void) {
    portENTER_CRITICAL();
}

void ivory_echronos_end_atomic(void) {
    portEXIT_CRITICAL();
}
