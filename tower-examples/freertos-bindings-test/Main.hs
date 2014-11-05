
module Main where

import qualified Data.List as L

import Ivory.Artifact
import Ivory.OS.FreeRTOS
import Ivory.Compile.C.CmdlineFrontend

main :: IO ()
main = compile [] as
  where
  as = kernel config ++ wrappers
     ++ [ artifactString "Makefile" makefile ]
  config = Config
    { cpu_clock_hz = 168 * 1000 * 1000
    , tick_rate_hz = 1000
    , max_priorities = 12
    , minimal_stack_size = 512
    , total_heap_size = 64 * 1024 * 1024
    }

makefile :: String
makefile = unlines
  [ "CC := arm-none-eabi-gcc"
  , "CFLAGS := \\"
  , "  -g3 -Wall -Werror -O2 \\"
  , "  -std=gnu99 \\"
  , "  -Wno-parenthesis -Wno-unused-variable \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16"
  , ""
  , "OBJDIR := obj"
  , "OBJS := $(addprefix $(OBJDIR)/," ++ (L.intercalate " " objects) ++ ")"
  , ""
  , "default: $(OBJDIR) $(OBJS)"
  , ""
  , "$(OBJDIR)/%.o : %.c"
  , "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR):"
  , "\tmkdir -p $(OBJDIR)"
  , ""
  , "clean:"
  , "\t-rm -rf obj"
  , ""
  , "veryclean: clean"
  , "\t-rm *.c *.h"
  ]

