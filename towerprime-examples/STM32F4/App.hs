{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List (find)
import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Frontend

import Interrupt
import Artifacts

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

  signalArtifacts = const $
    [ Artifact { artifact_filepath = "stm32f4_flash.lds.S"
               , artifact_contents = linker_script
               }
    , Artifact { artifact_filepath = "stm42f4_vectors.s"
               , artifact_contents = vector_table
               }
    ]

  signalSources = const []
  signalHeaders = const []
  signalSearchDir = const (return [])

task_simple_per :: Task p ()
task_simple_per = do
  ctr <- taskLocal "counter"
  lasttime <- taskLocal "lasttime"
  p <- timerEvent (Milliseconds 100)

  handle p "periodic" $ \timeRef -> do
    deref timeRef >>= store lasttime
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

wwdg_counter :: Task STM32F4Platform ()
wwdg_counter = do
  ctr <- taskLocal "counter"
  wwdg <- withSignalEvent (STM32F4Signal (Interrupt WWDG)) "wwdg"
  handle wwdg "wwdg" $ \_ -> do
    deref ctr >>= \(c :: Sint32) -> store ctr (c + 1)

tower_stm32f4test:: Tower STM32F4Platform ()
tower_stm32f4test = do
  task "per_trivial" task_simple_per
  task "wwdg_counter" wwdg_counter

main :: IO ()
main = compile defaultBuildConf tower_stm32f4test

