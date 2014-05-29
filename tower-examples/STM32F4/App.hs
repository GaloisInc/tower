{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List (find)
import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend
import Ivory.HW
import Ivory.HW.Module (hw_moduledef)
import qualified Ivory.HW.SearchDir as HW

import Interrupt
import ATIM18
--import Artifacts

data STM32F4Platform = STM32F4Platform
instance Signalable STM32F4Platform where
  data SignalType STM32F4Platform = STM32F4Signal IRQ
                                  deriving (Eq, Show, Read)
  signals = map STM32F4Signal
              ((map Exception (enumFrom NonMaskable))
              ++ (map Interrupt (enumFrom WWDG)))
  signalName (STM32F4Signal (Exception e)) = isrException e
  signalName (STM32F4Signal (Interrupt i)) = isrInterrupt i
  signalFromName n = maybe (error "invalid arg to signalFromName") id
                   $ find (\s -> signalName s == n) signals

task_simple_per :: Task p ()
task_simple_per = do
  ctr <- taskLocal "counter"
  lasttime <- taskLocal "lasttime"
  p <- withPeriodicEvent (Milliseconds 100)

  handle p "periodic" $ \timeRef -> do
    deref timeRef >>= store lasttime
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

tim1_counter :: Task STM32F4Platform ()
tim1_counter = do
  ctr <- taskLocal "counter"
  taskModuleDef $ hw_moduledef

  let max_syscall_priority = 12

  taskInit $ do
    atimRCCEnable tim1
    modifyReg (atimRegDIER tim1) $ setBit atim_dier_uie
    setReg    (atimRegPSC  tim1) $ setField atim_psc_psc (fromRep 167)
    setReg    (atimRegARR  tim1) $ setField atim_16_data (fromRep 0xFFFF)
    interrupt_set_priority TIM1_UP_TIM10 max_syscall_priority
    interrupt_enable       TIM1_UP_TIM10

  tim1_up <- withSignalEvent (STM32F4Signal (Interrupt TIM1_UP_TIM10)) "tim1_up_irq"
  handle tim1_up "tim1_up_handler" $ \_ -> do
    modifyReg (atimRegSR tim1) $ clearBit atim_sr_uif
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

tower_stm32f4test:: Tower STM32F4Platform ()
tower_stm32f4test = do
  task "per_trivial" task_simple_per
  task "tim1_counter" tim1_counter

main :: IO ()
main = compile conf tower_stm32f4test
  where conf = searchPathConf [HW.searchDir]

