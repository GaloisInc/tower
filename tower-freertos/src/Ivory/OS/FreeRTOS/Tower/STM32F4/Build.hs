
module Ivory.OS.FreeRTOS.Tower.STM32F4.Build
  ( makefile
  , artifacts
  ) where

import qualified Data.List as L

import qualified Paths_tower_freertos as P
import Ivory.Artifact
import System.FilePath

makefile :: [FilePath] -> Artifact
makefile userobjs = artifactString "Makefile" $ unlines
  [ "CC := arm-none-eabi-gcc"
  , "CFLAGS := \\"
  , "  -g3 -Wall -Werror -O2 \\"
  , "  -std=gnu99 \\"
  , "  -Wno-parenthesis -Wno-unused-variable \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\"
  , "  -DIVORY_TEST"
  , ""
  , "LDFLAGS := \\"
  , "  -mlittle-endian \\"
  , "  -mthumb -mcpu=cortex-m4 \\"
  , "  -mfloat-abi=hard -mfpu=fpv4-sp-d16"
  , ""
  , "LDSCRIPT := stm32f405.lds"
  , ""
  , "OBJDIR := obj"
  , "OBJS := $(addprefix $(OBJDIR)/," ++ (L.intercalate " " objects) ++ ")"
  , ""
  , "default: $(OBJDIR) $(OBJS)"
  , ""
  , "image: $(OBJS)"
  , "\t$(CC) -o $@ $(LDFLAGS) -Wl,--script=$(LDSCRIPT) -Wl,-Map=$@.map $(OBJS)"
  , ""
  , "$(OBJDIR)/%.o : %.c"
  , "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR)/%.o : %.s"
  , "\t$(CC) $(CFLAGS) -c -o $@ $<"
  , ""
  , "$(OBJDIR):"
  , "\tmkdir -p $(OBJDIR)"
  , ""
  , "clean:"
  , "\t-rm -rf obj"
  , "\t-rm image"
  , "\t-rm image.map"
  , ""
  ]
  where
  objects = userobjs ++ boot_objects

artifacts :: [Artifact]
artifacts = map (artifactCabalFile P.getDataDir)
                (boot_sources ++ boot_headers ++ [ linker_script ])

linker_script :: FilePath
linker_script = "support/stm32f405.lds"

boot_objects :: [FilePath]
boot_objects = [ replaceExtension (takeFileName f) ".o" | f <- boot_sources ]

boot_sources :: [FilePath]
boot_sources =
  [ "support/stm32_init.c"
  , "support/stm32_ivory_init.c"
  , "support/stm32f405_vectors.s"
  ]

-- XXX many of these can be improved:
--    - stm32_ivory_init is actually generated as an ivory module by
--      ivory_bsp_stm32
--    - stm32f405_vectors is generated as an artifact by ivory_bsp_stm32
--    - some parts of stm32_init.c can be moved to ivory generated code
--    - the linker script can be generalized to a template, and filled in
--      by sone config that specifies the exact chip, whether there is a
--      bootloader, and so on

boot_headers :: [FilePath]
boot_headers =
  [ "support/stm32_init.h"
  , "support/stm32_ivory_init.h"
  ]

