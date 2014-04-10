--
-- Types.hs -- Interrupt types for STM32F4
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Interrupt.Types where

data IRQ = Exception Exception
         | Interrupt Interrupt
         deriving (Eq, Show, Read)

irqn :: IRQ -> Int
irqn (Exception e) = exceptionIRQn e
irqn (Interrupt i) = interruptIRQn i

data Exception
  = NonMaskable
  | MemoryManagment
  | BusFault
  | UsageFault
  | SVCall
  | DebugMonitor
  | PendSV
  | SysTick
  deriving (Eq, Show, Enum, Read)

exceptionIRQn :: Exception -> Int
exceptionIRQn NonMaskable     = -14
exceptionIRQn MemoryManagment = -12
exceptionIRQn BusFault        = -11
exceptionIRQn UsageFault      = -10
exceptionIRQn SVCall          = -5
exceptionIRQn DebugMonitor    = -4
exceptionIRQn PendSV          = -2
exceptionIRQn SysTick         = -1

data Interrupt
  = WWDG                -- Window WatchDog Interrupt
  | PVD                 -- PVD through EXTI Line detection Interrupt
  | TAMP_STAMP          -- Tamper and TimeStamp interrupts through the EXTI line
  | RTC_WKUP            -- RTC Wakeup interrupt through the EXTI line
  | FLASH               -- FLASH global Interrupt
  | RCC                 -- RCC global Interrupt
  | EXTI0               -- EXTI Line0 Interrupt
  | EXTI1               -- EXTI Line1 Interrupt
  | EXTI2               -- EXTI Line2 Interrupt
  | EXTI3               -- EXTI Line3 Interrupt
  | EXTI4               -- EXTI Line4 Interrupt
  | DMA1_Stream0        -- DMA1 Stream 0 global Interrupt
  | DMA1_Stream1        -- DMA1 Stream 1 global Interrupt
  | DMA1_Stream2        -- DMA1 Stream 2 global Interrupt
  | DMA1_Stream3        -- DMA1 Stream 3 global Interrupt
  | DMA1_Stream4        -- DMA1 Stream 4 global Interrupt
  | DMA1_Stream5        -- DMA1 Stream 5 global Interrupt
  | DMA1_Stream6        -- DMA1 Stream 6 global Interrupt
  | ADC                 -- ADC1, ADC2 and ADC3 global Interrupts
  | CAN1_TX             -- CAN1 TX Interrupt
  | CAN1_RX0            -- CAN1 RX0 Interrupt
  | CAN1_RX1            -- CAN1 RX1 Interrupt
  | CAN1_SCE            -- CAN1 SCE Interrupt
  | EXTI9_5             -- External Line[9:5] Interrupts
  | TIM1_BRK_TIM9       -- TIM1 Break interrupt and TIM9 global interrupt
  | TIM1_UP_TIM10       -- TIM1 Update Interrupt and TIM10 global interrupt
  | TIM1_TRG_COM_TIM11  -- TIM1 Trigger and Commutation Interrupt and TIM11 global interrupt
  | TIM1_CC             -- TIM1 Capture Compare Interrupt
  | TIM2                -- TIM2 global Interrupt
  | TIM3                -- TIM3 global Interrupt
  | TIM4                -- TIM4 global Interrupt
  | I2C1_EV             -- I2C1 Event Interrupt
  | I2C1_ER             -- I2C1 Error Interrupt
  | I2C2_EV             -- I2C2 Event Interrupt
  | I2C2_ER             -- I2C2 Error Interrupt
  | SPI1                -- SPI1 global Interrupt
  | SPI2                -- SPI2 global Interrupt
  | USART1              -- USART1 global Interrupt
  | USART2              -- USART2 global Interrupt
  | USART3              -- USART3 global Interrupt
  | EXTI15_10           -- External Line[15:10] Interrupts
  | RTC_Alarm           -- RTC Alarm (A and B) through EXTI Line Interrupt
  | OTG_FS_WKUP         -- USB OTG FS Wakeup through EXTI line interrupt
  | TIM8_BRK_TIM12      -- TIM8 Break Interrupt and TIM12 global interrupt
  | TIM8_UP_TIM13       -- TIM8 Update Interrupt and TIM13 global interrupt
  | TIM8_TRG_COM_TIM14  -- TIM8 Trigger and Commutation Interrupt and TIM14 global interrupt
  | TIM8_CC             -- TIM8 Capture Compare Interrupt
  | DMA1_Stream7        -- DMA1 Stream7 Interrupt
  | FSMC                -- FSMC global Interrupt
  | SDIO                -- SDIO global Interrupt
  | TIM5                -- TIM5 global Interrupt
  | SPI3                -- SPI3 global Interrupt
  | UART4               -- UART4 global Interrupt
  | UART5               -- UART5 global Interrupt
  | TIM6_DAC            -- TIM6 global and DAC1&2 underrun error  interrupts
  | TIM7                -- TIM7 global interrupt
  | DMA2_Stream0        -- DMA2 Stream 0 global Interrupt
  | DMA2_Stream1        -- DMA2 Stream 1 global Interrupt
  | DMA2_Stream2        -- DMA2 Stream 2 global Interrupt
  | DMA2_Stream3        -- DMA2 Stream 3 global Interrupt
  | DMA2_Stream4        -- DMA2 Stream 4 global Interrupt
  | ETH                 -- Ethernet global Interrupt
  | ETH_WKUP            -- Ethernet Wakeup through EXTI line Interrupt
  | CAN2_TX             -- CAN2 TX Interrupt
  | CAN2_RX0            -- CAN2 RX0 Interrupt
  | CAN2_RX1            -- CAN2 RX1 Interrupt
  | CAN2_SCE            -- CAN2 SCE Interrupt
  | OTG_FS              -- USB OTG FS global Interrupt
  | DMA2_Stream5        -- DMA2 Stream 5 global interrupt
  | DMA2_Stream6        -- DMA2 Stream 6 global interrupt
  | DMA2_Stream7        -- DMA2 Stream 7 global interrupt
  | USART6              -- USART6 global interrupt
  | I2C3_EV             -- I2C3 event interrupt
  | I2C3_ER             -- I2C3 error interrupt
  | OTG_HS_EP1_OUT      -- USB OTG HS End Point 1 Out global interrupt
  | OTG_HS_EP1_IN       -- USB OTG HS End Point 1 In global interrupt
  | OTG_HS_WKUP         -- USB OTG HS Wakeup through EXTI interrupt
  | OTG_HS              -- USB OTG HS global interrupt
  | DCMI                -- DCMI global interrupt
  | CRYP                -- CRYP crypto global interrupt
  | HASH_RNG            -- Hash and Rng global interrupt
  | FPU                 -- FPU global interrupt
  deriving (Eq, Show, Enum, Read)

interruptIRQn :: Interrupt -> Int
interruptIRQn = fromEnum

isrException :: Exception -> String
isrException e = show e ++ "_IRQHandler"

isrInterrupt :: Interrupt -> String
isrInterrupt i = show i ++ "_IRQHandler"

