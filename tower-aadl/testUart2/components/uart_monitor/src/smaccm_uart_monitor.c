#include <uart_monitor.h>

bool Input0_write_Data_Types__uart_packet_impl(const Data_Types__uart_packet_impl * input) {
  int r = uart_write(input->uart_num, input->datum);
  if (r < 0) {
    printf("Error from uart_write, return code: %d\n", r);
  }
  return (r >= 0);
}
