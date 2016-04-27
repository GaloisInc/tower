#include "intermon2.h"

static bool the_data;
static bool has_data = false;

bool intermon2_get_data(bool *out) {
	if (has_data) {
		*out = the_data;
		has_data = false;
		return true;
	} else {
		return false;
	}
}

void intermon2_put_data(const bool *data) {
	the_data = *data;
	has_data = true;
	return;
}
