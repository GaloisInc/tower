

	.syntax unified
	.thumb
	.cpu cortex-m3
	.fpu softvfp


.global Reset_Handler
.global g_vectors
.word _sidata
.word _sdata
.word _edata
.word _sbss
.word _ebss


	.section .text.defaultExceptionHandler, "ax",%progbits
defaultExceptionHandler:
	b defaultExceptionHandler
	.size defaultExceptionHandler, .-defaultExceptionHandler

	.section .isr_vector,"ax"
	.code    16
	.align   2
	.globl   g_vectors
	.type    g_vectors, function

g_vectors:

	.word _estack
	.word Reset_Handler
	.word NonMaskable_Handler
	.word HardFault_Handler
	.word MemoryManagment_Handler
	.word BusFault_Handler
	.word UsageFault_Handler
	.word 0
	.word 0
	.word 0
	.word 0
	.word SVCall_Handler
	.word DebugMonitor_Handler
	.word 0
	.word PendSV_Handler
	.word SysTick_Handler
	.word WWDG_IRQHandler
	.word PVD_IRQHandler
	.word TAMP_STAMP_IRQHandler
	.word RTC_WKUP_IRQHandler
	.word FLASH_IRQHandler
	.word RCC_IRQHandler
	.word EXTI0_IRQHandler
	.word EXTI1_IRQHandler
	.word EXTI2_IRQHandler
	.word EXTI3_IRQHandler
	.word EXTI4_IRQHandler
	.word DMA1_Stream0_IRQHandler
	.word DMA1_Stream1_IRQHandler
	.word DMA1_Stream2_IRQHandler
	.word DMA1_Stream3_IRQHandler
	.word DMA1_Stream4_IRQHandler
	.word DMA1_Stream5_IRQHandler
	.word DMA1_Stream6_IRQHandler
	.word ADC_IRQHandler
	.word CAN1_TX_IRQHandler
	.word CAN1_RX0_IRQHandler
	.word CAN1_RX1_IRQHandler
	.word CAN1_SCE_IRQHandler
	.word EXTI9_5_IRQHandler
	.word TIM1_BRK_TIM9_IRQHandler
	.word TIM1_UP_TIM10_IRQHandler
	.word TIM1_TRG_COM_TIM11_IRQHandler
	.word TIM1_CC_IRQHandler
	.word TIM2_IRQHandler
	.word TIM3_IRQHandler
	.word TIM4_IRQHandler
	.word I2C1_EV_IRQHandler
	.word I2C1_ER_IRQHandler
	.word I2C2_EV_IRQHandler
	.word I2C2_ER_IRQHandler
	.word SPI1_IRQHandler
	.word SPI2_IRQHandler
	.word USART1_IRQHandler
	.word USART2_IRQHandler
	.word USART3_IRQHandler
	.word EXTI15_10_IRQHandler
	.word RTC_Alarm_IRQHandler
	.word OTG_FS_WKUP_IRQHandler
	.word TIM8_BRK_TIM12_IRQHandler
	.word TIM8_UP_TIM13_IRQHandler
	.word TIM8_TRG_COM_TIM14_IRQHandler
	.word TIM8_CC_IRQHandler
	.word DMA1_Stream7_IRQHandler
	.word FSMC_IRQHandler
	.word SDIO_IRQHandler
	.word TIM5_IRQHandler
	.word SPI3_IRQHandler
	.word UART4_IRQHandler
	.word UART5_IRQHandler
	.word TIM6_DAC_IRQHandler
	.word TIM7_IRQHandler
	.word DMA2_Stream0_IRQHandler
	.word DMA2_Stream1_IRQHandler
	.word DMA2_Stream2_IRQHandler
	.word DMA2_Stream3_IRQHandler
	.word DMA2_Stream4_IRQHandler
	.word ETH_IRQHandler
	.word ETH_WKUP_IRQHandler
	.word CAN2_TX_IRQHandler
	.word CAN2_RX0_IRQHandler
	.word CAN2_RX1_IRQHandler
	.word CAN2_SCE_IRQHandler
	.word OTG_FS_IRQHandler
	.word DMA2_Stream5_IRQHandler
	.word DMA2_Stream6_IRQHandler
	.word DMA2_Stream7_IRQHandler
	.word USART6_IRQHandler
	.word I2C3_EV_IRQHandler
	.word I2C3_ER_IRQHandler
	.word OTG_HS_EP1_OUT_IRQHandler
	.word OTG_HS_EP1_IN_IRQHandler
	.word OTG_HS_WKUP_IRQHandler
	.word OTG_HS_IRQHandler
	.word DCMI_IRQHandler
	.word CRYP_IRQHandler
	.word HASH_RNG_IRQHandler
	.word FPU_IRQHandler

	.size g_vectors, .-g_vectors

	.weak NonMaskable_Handler
	.thumb_set NonMaskable_Handler,defaultExceptionHandler

	.weak HardFault_Handler
	.thumb_set HardFault_Handler,defaultExceptionHandler

	.weak MemoryManagment_Handler
	.thumb_set MemoryManagment_Handler,defaultExceptionHandler

	.weak BusFault_Handler
	.thumb_set BusFault_Handler,defaultExceptionHandler

	.weak UsageFault_Handler
	.thumb_set UsageFault_Handler,defaultExceptionHandler





	.weak SVCall_Handler
	.thumb_set SVCall_Handler,defaultExceptionHandler

	.weak DebugMonitor_Handler
	.thumb_set DebugMonitor_Handler,defaultExceptionHandler


	.weak PendSV_Handler
	.thumb_set PendSV_Handler,defaultExceptionHandler

	.weak SysTick_Handler
	.thumb_set SysTick_Handler,defaultExceptionHandler

	.weak WWDG_IRQHandler
	.thumb_set WWDG_IRQHandler,defaultExceptionHandler

	.weak PVD_IRQHandler
	.thumb_set PVD_IRQHandler,defaultExceptionHandler

	.weak TAMP_STAMP_IRQHandler
	.thumb_set TAMP_STAMP_IRQHandler,defaultExceptionHandler

	.weak RTC_WKUP_IRQHandler
	.thumb_set RTC_WKUP_IRQHandler,defaultExceptionHandler

	.weak FLASH_IRQHandler
	.thumb_set FLASH_IRQHandler,defaultExceptionHandler

	.weak RCC_IRQHandler
	.thumb_set RCC_IRQHandler,defaultExceptionHandler

	.weak EXTI0_IRQHandler
	.thumb_set EXTI0_IRQHandler,defaultExceptionHandler

	.weak EXTI1_IRQHandler
	.thumb_set EXTI1_IRQHandler,defaultExceptionHandler

	.weak EXTI2_IRQHandler
	.thumb_set EXTI2_IRQHandler,defaultExceptionHandler

	.weak EXTI3_IRQHandler
	.thumb_set EXTI3_IRQHandler,defaultExceptionHandler

	.weak EXTI4_IRQHandler
	.thumb_set EXTI4_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream0_IRQHandler
	.thumb_set DMA1_Stream0_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream1_IRQHandler
	.thumb_set DMA1_Stream1_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream2_IRQHandler
	.thumb_set DMA1_Stream2_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream3_IRQHandler
	.thumb_set DMA1_Stream3_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream4_IRQHandler
	.thumb_set DMA1_Stream4_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream5_IRQHandler
	.thumb_set DMA1_Stream5_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream6_IRQHandler
	.thumb_set DMA1_Stream6_IRQHandler,defaultExceptionHandler

	.weak ADC_IRQHandler
	.thumb_set ADC_IRQHandler,defaultExceptionHandler

	.weak CAN1_TX_IRQHandler
	.thumb_set CAN1_TX_IRQHandler,defaultExceptionHandler

	.weak CAN1_RX0_IRQHandler
	.thumb_set CAN1_RX0_IRQHandler,defaultExceptionHandler

	.weak CAN1_RX1_IRQHandler
	.thumb_set CAN1_RX1_IRQHandler,defaultExceptionHandler

	.weak CAN1_SCE_IRQHandler
	.thumb_set CAN1_SCE_IRQHandler,defaultExceptionHandler

	.weak EXTI9_5_IRQHandler
	.thumb_set EXTI9_5_IRQHandler,defaultExceptionHandler

	.weak TIM1_BRK_TIM9_IRQHandler
	.thumb_set TIM1_BRK_TIM9_IRQHandler,defaultExceptionHandler

	.weak TIM1_UP_TIM10_IRQHandler
	.thumb_set TIM1_UP_TIM10_IRQHandler,defaultExceptionHandler

	.weak TIM1_TRG_COM_TIM11_IRQHandler
	.thumb_set TIM1_TRG_COM_TIM11_IRQHandler,defaultExceptionHandler

	.weak TIM1_CC_IRQHandler
	.thumb_set TIM1_CC_IRQHandler,defaultExceptionHandler

	.weak TIM2_IRQHandler
	.thumb_set TIM2_IRQHandler,defaultExceptionHandler

	.weak TIM3_IRQHandler
	.thumb_set TIM3_IRQHandler,defaultExceptionHandler

	.weak TIM4_IRQHandler
	.thumb_set TIM4_IRQHandler,defaultExceptionHandler

	.weak I2C1_EV_IRQHandler
	.thumb_set I2C1_EV_IRQHandler,defaultExceptionHandler

	.weak I2C1_ER_IRQHandler
	.thumb_set I2C1_ER_IRQHandler,defaultExceptionHandler

	.weak I2C2_EV_IRQHandler
	.thumb_set I2C2_EV_IRQHandler,defaultExceptionHandler

	.weak I2C2_ER_IRQHandler
	.thumb_set I2C2_ER_IRQHandler,defaultExceptionHandler

	.weak SPI1_IRQHandler
	.thumb_set SPI1_IRQHandler,defaultExceptionHandler

	.weak SPI2_IRQHandler
	.thumb_set SPI2_IRQHandler,defaultExceptionHandler

	.weak USART1_IRQHandler
	.thumb_set USART1_IRQHandler,defaultExceptionHandler

	.weak USART2_IRQHandler
	.thumb_set USART2_IRQHandler,defaultExceptionHandler

	.weak USART3_IRQHandler
	.thumb_set USART3_IRQHandler,defaultExceptionHandler

	.weak EXTI15_10_IRQHandler
	.thumb_set EXTI15_10_IRQHandler,defaultExceptionHandler

	.weak RTC_Alarm_IRQHandler
	.thumb_set RTC_Alarm_IRQHandler,defaultExceptionHandler

	.weak OTG_FS_WKUP_IRQHandler
	.thumb_set OTG_FS_WKUP_IRQHandler,defaultExceptionHandler

	.weak TIM8_BRK_TIM12_IRQHandler
	.thumb_set TIM8_BRK_TIM12_IRQHandler,defaultExceptionHandler

	.weak TIM8_UP_TIM13_IRQHandler
	.thumb_set TIM8_UP_TIM13_IRQHandler,defaultExceptionHandler

	.weak TIM8_TRG_COM_TIM14_IRQHandler
	.thumb_set TIM8_TRG_COM_TIM14_IRQHandler,defaultExceptionHandler

	.weak TIM8_CC_IRQHandler
	.thumb_set TIM8_CC_IRQHandler,defaultExceptionHandler

	.weak DMA1_Stream7_IRQHandler
	.thumb_set DMA1_Stream7_IRQHandler,defaultExceptionHandler

	.weak FSMC_IRQHandler
	.thumb_set FSMC_IRQHandler,defaultExceptionHandler

	.weak SDIO_IRQHandler
	.thumb_set SDIO_IRQHandler,defaultExceptionHandler

	.weak TIM5_IRQHandler
	.thumb_set TIM5_IRQHandler,defaultExceptionHandler

	.weak SPI3_IRQHandler
	.thumb_set SPI3_IRQHandler,defaultExceptionHandler

	.weak UART4_IRQHandler
	.thumb_set UART4_IRQHandler,defaultExceptionHandler

	.weak UART5_IRQHandler
	.thumb_set UART5_IRQHandler,defaultExceptionHandler

	.weak TIM6_DAC_IRQHandler
	.thumb_set TIM6_DAC_IRQHandler,defaultExceptionHandler

	.weak TIM7_IRQHandler
	.thumb_set TIM7_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream0_IRQHandler
	.thumb_set DMA2_Stream0_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream1_IRQHandler
	.thumb_set DMA2_Stream1_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream2_IRQHandler
	.thumb_set DMA2_Stream2_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream3_IRQHandler
	.thumb_set DMA2_Stream3_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream4_IRQHandler
	.thumb_set DMA2_Stream4_IRQHandler,defaultExceptionHandler

	.weak ETH_IRQHandler
	.thumb_set ETH_IRQHandler,defaultExceptionHandler

	.weak ETH_WKUP_IRQHandler
	.thumb_set ETH_WKUP_IRQHandler,defaultExceptionHandler

	.weak CAN2_TX_IRQHandler
	.thumb_set CAN2_TX_IRQHandler,defaultExceptionHandler

	.weak CAN2_RX0_IRQHandler
	.thumb_set CAN2_RX0_IRQHandler,defaultExceptionHandler

	.weak CAN2_RX1_IRQHandler
	.thumb_set CAN2_RX1_IRQHandler,defaultExceptionHandler

	.weak CAN2_SCE_IRQHandler
	.thumb_set CAN2_SCE_IRQHandler,defaultExceptionHandler

	.weak OTG_FS_IRQHandler
	.thumb_set OTG_FS_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream5_IRQHandler
	.thumb_set DMA2_Stream5_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream6_IRQHandler
	.thumb_set DMA2_Stream6_IRQHandler,defaultExceptionHandler

	.weak DMA2_Stream7_IRQHandler
	.thumb_set DMA2_Stream7_IRQHandler,defaultExceptionHandler

	.weak USART6_IRQHandler
	.thumb_set USART6_IRQHandler,defaultExceptionHandler

	.weak I2C3_EV_IRQHandler
	.thumb_set I2C3_EV_IRQHandler,defaultExceptionHandler

	.weak I2C3_ER_IRQHandler
	.thumb_set I2C3_ER_IRQHandler,defaultExceptionHandler

	.weak OTG_HS_EP1_OUT_IRQHandler
	.thumb_set OTG_HS_EP1_OUT_IRQHandler,defaultExceptionHandler

	.weak OTG_HS_EP1_IN_IRQHandler
	.thumb_set OTG_HS_EP1_IN_IRQHandler,defaultExceptionHandler

	.weak OTG_HS_WKUP_IRQHandler
	.thumb_set OTG_HS_WKUP_IRQHandler,defaultExceptionHandler

	.weak OTG_HS_IRQHandler
	.thumb_set OTG_HS_IRQHandler,defaultExceptionHandler

	.weak DCMI_IRQHandler
	.thumb_set DCMI_IRQHandler,defaultExceptionHandler

	.weak CRYP_IRQHandler
	.thumb_set CRYP_IRQHandler,defaultExceptionHandler

	.weak HASH_RNG_IRQHandler
	.thumb_set HASH_RNG_IRQHandler,defaultExceptionHandler

	.weak FPU_IRQHandler
	.thumb_set FPU_IRQHandler,defaultExceptionHandler

