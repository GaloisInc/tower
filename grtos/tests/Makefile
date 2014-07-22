
CC := arm-none-eabi-gcc
CFLAGS += -std=gnu99 -Wall
CFLAGS += -I.
CFLAGS += -O2
CFLAGS += -mlittle-endian -mthumb -mcpu=cortex-m4 -mfloat-abi=hard -mfpu=fpv4-sp-d16
CFLAGS += -Wno-parentheses -Wno-unused-variable -DIVORY_TEST

LDFLAGS += -mlittle-endian -mthumb -mcpu=cortex-m4 -mfloat-abi=hard -mfpu=fpv4-sp-d16

OBJS := grtos_kernel.o  grtos_port.o  grtos_syscalls.o
OBJS += ivory_grtos_kernel.o
OBJS += pkg.o
OBJS += stub_main.o
OBJS += launch.o


all: app.elf

stub_main.c:
	echo "" > stub_main.c
	echo "#include \"grtos_kernel.h\"" >> stub_main.c
	echo "#include \"launch.h\"" >> stub_main.c
	echo "" >> stub_main.c
	echo "int main (void) { launch_proc(); scheduler_start(); for(;;); return 0; }" >> stub_main.c
	echo "void Reset_Handler (void) { main(); }" >> stub_main.c

app.elf: $(OBJS)
	$(CC) -o app.elf $(LDFLAGS) -Wl,--script=./stm32.lds -Wl,-Map=app.elf $(OBJS)

clean:
	-rm *.c
	-rm *.h
	-rm *.o
	-rm *.elf
	-rm *.map

regen:
	cd .. && cabal run grtos-test-gen -- --src-dir=tests --include-dir=tests

