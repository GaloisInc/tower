
#include <smaccm_uart.h>
#include <uart_driver.h>
#include <driver.h>
#include <string.h>


bool send_write_Data_Types__uart_packet_impl(const Data_Types__uart_packet_impl *arg) {
	int32_t result = -1;

	// Other options rather than fail: re-send up to a bounded # of times?
	// int32_t retries = 0;
	result = uart_write(arg->uart_num, arg->datum);
	if (result == -1) {
		printf("send failed!.\n");
		return false;
	}
	return true;
}

void pilot_recv(int32_t uart_num, int32_t c) {
	Data_Types__uart_packet_impl packet;
	packet.uart_num = uart_num;
	packet.datum = c;

	// MWW: might want to do error checking here(!)
	recv_write_Data_Types__uart_packet_impl(&packet);
}
	
