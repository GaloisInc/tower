mkdir components
cp -r /Users/leepike/vm-shared/testUart-works/components/uart_driver components
make ramses
cp /Users/leepike/vm-shared/testUart-works/components/uart_monitor_11/uart_monitor_11.camkes ./components/uart_monitor_11
cp /Users/leepike/vm-shared/testUart-works/othercamkestargets.mk ./
