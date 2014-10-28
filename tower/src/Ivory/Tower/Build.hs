
module Ivory.Tower.Build
  ( makefile
  ) where

import Text.PrettyPrint.Mainland
import Ivory.Compile.C.CmdlineFrontend
import System.FilePath (replaceDirectory)

makefile :: ModuleFiles -> Doc
makefile sources = stack
  [ toolchain
  , empty
  , text "##############"
  , empty
  , srcs sources
  , empty
  , text "##############"
  , empty
  , targets
  ]

toolchain :: Doc
toolchain = stack
  [ text "CC := arm-none-eabi-gcc"
  , empty
  , text "BASE_CFLAGS := \\"
  , text "    -g3 -Wall -Werror -O2 \\"
  , hwopts
  , text "    -Wno-parentheses -Wno-unused-variable \\"
  , text "    -DIVORY_TEST"
  , empty
  , text "FREERTOS_INCLUDES := \\"
  , text "    -I../testing_helpers/FreeRTOS/include \\"
  , text "    -I../testing_helpers/FreeRTOS/portable/GCC/ARM_CM4F \\"
  , text "    -I../testing_helpers/FreeRTOS/config"
  , text "FREERTOS_LIB := ../testing_helpers/FreeRTOS/libFreeRTOS.a"
  , empty
  , text "STM32_INCLUDES := -I../testing_helpers/stm32"
  , text "STM32_OBJS := \\"
  , text "    ../testing_helpers/stm32/stm32_init.o \\"
  , text "    ../testing_helpers/stm32/stm32f405_vectors.o"
  , empty
  , text "CFLAGS := $(BASE_CFLAGS) -std=gnu99 $(FREERTOS_INCLUDES) $(STM32_INCLUDES)"
  , empty
  , text "LDFLAGS := \\"
  , hwopts
  ]
  where
  hwopts = stack
    [ text "    -mlittle-endian \\"
    , text "    -mthumb -mcpu=cortex-m4 \\"
    , text "    -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\"
    ]

srcs :: ModuleFiles -> Doc
srcs sources = decl
  </> indent 4 items
  </> text "OBJS := $(SRCS:.c=.o)"
  where
  decl = text "SRCS := \\"
  items = stack $ punctuate backslash ivory_sources
  ivory_sources = map mkpath $ mf_sources sources
  mkpath p = string (replaceDirectory p "")
  backslash = text " \\"

targets :: Doc
targets = stack
  [ text "default: test"
  , empty
  , text "test: $(OBJS) $(STM32_OBJS)"
  , text "\t$(CC) -o $@ $(LDFLAGS) -Wl,-Map=$@.map $(OBJS) $(STM32_OBJS) $(FREERTOS_LIB)"
  , empty
  , text "%.o : %.c"
  , text "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , empty
  , text "%.o : %.s"
  , text "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , empty
  , text "clean:"
  , text "\t-rm *.o test"
  , empty
  , text "veryclean: clean"
  , text "\t-rm *.c *.h"
  ]

