
#ifndef __IVORY_ECHRONOS_QUEUE_WRAPPER_H__
#define __IVORY_ECHRONOS_QUEUE_WRAPPER_H__

#include <stdbool.h>
#include <stdint.h>

/* we're using uint16_t to stand in for void, because ivory doesn't have a
 * concept of void. an xQueueHandle is a void*, so a uint16_t** is analogous
 * to a xQueueHandle*, so we must deref the argument value once before using
 * it.*/
void ivory_echronos_queue_create(uint16_t** queuehandle, uint32_t len);

bool ivory_echronos_queue_send(uint16_t** queuehandle,
        uint32_t value, uint32_t max_delay);

bool ivory_echronos_queue_receive(uint16_t** queuehandle,
        uint32_t* value, uint32_t max_delay);

bool ivory_echronos_queue_send_isr(uint16_t** queuehandle,
        uint32_t value);

bool ivory_echronos_queue_receive_isr(uint16_t** queuehandle,
        uint32_t* value);

uint32_t ivory_echronos_queue_messages_waiting(uint16_t** queuehandle);

#endif // __IVORY_ECHRONOS_QUEUE_WRAPPER_H__
