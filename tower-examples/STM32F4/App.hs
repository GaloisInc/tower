{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Compile
import Ivory.HW
import Ivory.HW.Module (hw_moduledef)

import Interrupt
import ATIM18

data STM32F4Platform = STM32F4Platform
instance Signalable STM32F4Platform where
  data SignalType STM32F4Platform = STM32F4Signal IRQ
                                  deriving (Eq, Show, Read)
  signalName (STM32F4Signal (Exception e)) = isrException e
  signalName (STM32F4Signal (Interrupt i)) = isrInterrupt i
  signalHandler s = \b ->
    (incl ((proc (signalName s ++ "_Handler") $ body $ b) :: Def('[]:->())))

simple_counter_monitor :: ChanOutput (Stored ITime) -> Monitor p ()
simple_counter_monitor p = do
  ctr <- state "counter"
  lasttime <- state "lasttime"

  handler p "periodic" $ callback $ \timeRef -> do
    deref timeRef >>= store lasttime
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

tim1_monitor :: ChanOutput (Stored ITime) -> Monitor STM32F4Platform ()
tim1_monitor tim1_up = do
  ctr <- state "counter"
  monitorModuleDef $ hw_moduledef

  let max_syscall_priority = 12

  handler systemInit "systemInit" $ callback $ \_ -> do
    atimRCCEnable tim1
    modifyReg (atimRegDIER tim1) $ setBit atim_dier_uie
    setReg    (atimRegPSC  tim1) $ setField atim_psc_psc (fromRep 167)
    setReg    (atimRegARR  tim1) $ setField atim_16_data (fromRep 0xFFFF)
    interrupt_set_priority TIM1_UP_TIM10 max_syscall_priority
    interrupt_enable       TIM1_UP_TIM10

  handler tim1_up "tim1_up_handler" $ callback $ \_ -> do
    modifyReg (atimRegSR tim1) $ clearBit atim_sr_uif
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

tower_stm32f4test:: Tower STM32F4Platform ()
tower_stm32f4test = do
  per <- period (Milliseconds 25)
  monitor "counter" (simple_counter_monitor per)
  tim1_up <- signal (STM32F4Signal (Interrupt TIM1_UP_TIM10)) (Microseconds 100)
  monitor "tim1" (tim1_monitor tim1_up)

main :: IO ()
main = undefined
  --  where conf = searchPathConf [HW.searchDir]

