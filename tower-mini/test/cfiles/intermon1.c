#include "intermon1.h"

static uint8_t the_data;
static bool has_data = false;

bool intermon1_get_data(uint8_t *out) {
	if (has_data) {
		*out = the_data;
		has_data = false;
		return true;
	} else {
		return false;
	}
}

void intermon1_put_data(const uint8_t *data) {
	the_data = *data;
	has_data = true;
	return;
}
