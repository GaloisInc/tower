/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#include <stdio.h>

#include <platsupport/chardev.h>
#include <platsupport/serial.h>
#include <utils/util.h>
#include <driver.h>
#include <uart_driver.h>

//#define BAUD_RATE 115200
#define BAUD_RATE 57600

#ifdef CONFIG_PLAT_EXYNOS5410
#define DEV_ID  PS_SERIAL1
#elif CONFIG_PLAT_IMX31
#define DEV_ID  IMX31_UART1
#else
#error
#endif

struct uart_token {
    size_t cur_bytes;
    size_t req_bytes;
    char* buf;
};

char client_buffer[4];

static ps_chardevice_t serial_device;

static void
interrupt_event(void* token)
{
    ps_chardevice_t* device;
    device = (ps_chardevice_t*)token;
    ps_cdev_handle_irq(device, 0);
    interrupt_reg_callback(&interrupt_event, token);
}

/* UART 1 dummy input clock */
static freq_t
dummy_get_freq(clk_t* clk) {
    return 24000000;
}

static freq_t
dummy_set_freq(clk_t* clk, UNUSED freq_t hz){
    return dummy_get_freq(clk);
}

#define DUMMY_CLK(cust_id)              \
    {                                   \
        .id = cust_id,                  \
        .name = "DUMMY " #cust_id,      \
        .priv = NULL,                   \
        .req_freq = 0,                  \
        .parent = NULL,                 \
        .sibling = NULL,                \
        .child = NULL,                  \
        .clk_sys = NULL,                \
        .init = NULL,                   \
        .get_freq = &dummy_get_freq,    \
        .set_freq = &dummy_set_freq,    \
        .recal = NULL                   \
    }

#if DEV_ID == 1
clk_t uart_dummy_clk = DUMMY_CLK(CLK_UART1);
#else
clk_t uart_dummy_clk = DUMMY_CLK(CLK_UART3);
#endif

void uart__init(void)
{
    /* Iniitialise the UART */
    printf("Initialising UART driver\n");
    if(exynos_serial_init(DEV_ID, uart0base, NULL, &uart_dummy_clk,
            &serial_device)){
        printf("Failed to initialise UART\n");
        while(1);
    }
    serial_configure(&serial_device, BAUD_RATE, 8, PARITY_NONE, 1);
    /* Prime semaphores */
    read_sem_wait();
    write_sem_wait();
    /* Register for IRQs */
    interrupt_reg_callback(&interrupt_event, &serial_device);
}


static void
read_callback(ps_chardevice_t* device, enum chardev_status stat,
              size_t bytes_transfered, void* token){
    struct uart_token* t;
    t = (struct uart_token*)token;
    /* We might get a short read due to a timeout. */
    t->cur_bytes += bytes_transfered;
    t->buf += bytes_transfered;
    if(t->cur_bytes < t->req_bytes){
        int ret;
        ret = ps_cdev_read(device, t->buf, t->req_bytes - t->cur_bytes,
                           &read_callback, token);
        if(ret < 0){
            printf("Error reading from UART\n");
            read_sem_post();
        }
    }else{
        read_sem_post();
    }
}

static void
write_callback(ps_chardevice_t* device, enum chardev_status stat,
              size_t bytes_transfered, void* token){
    struct uart_token* t;
    t = (struct uart_token*)token;
    t->cur_bytes += bytes_transfered;
    if(t->cur_bytes == t->req_bytes){
        write_sem_post();
    }
}


int uart_read(int uart_num, char *c, int rsize)
{
    struct uart_token token;
    if (uart_num != 0) {
        printf("Only support UART0!\n");
        return -1;
    }

    token.cur_bytes = 0;
    token.req_bytes = rsize;
    token.buf = c;
    if(ps_cdev_read(&serial_device, token.buf, token.req_bytes, &read_callback, &token) < 0){
        printf("Error reading from UART\n");
        return -1;
    }
    read_sem_wait();

    return token.cur_bytes;
}

int uart_write(int uart_num, int datum)
{
    struct uart_token token;
    if (uart_num != 0) {
        printf("Only support UART0!\n");
        return -1;
    }

    token.cur_bytes = 0;
    token.buf = (char*)client_buffer;

    //////////////////////////////////////////////
    // changed: MWW
    // token.req_bytes = wsize;
    token.req_bytes = 4;
    *((int *)token.buf) = datum;
    //////////////////////////////////////////////
    
    

    if(ps_cdev_write(&serial_device, token.buf, token.req_bytes, &write_callback, &token) < 0){
        printf("Error writing to UART\n");
        return -1;
    }
    write_sem_wait();

    return token.req_bytes;
}

int run(void)
{
	char c;
	printf("Started UART driver.");
	uart__init();
	while (1) {
		uart_read(0, &c, 1);
		pilot_recv(ID, (int)c);
	}

	return 0;
}
