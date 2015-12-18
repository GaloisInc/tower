-- Create Ramses build script.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build.EChronos where

import Tower.AADL.Config (AADLConfig(..), lib)
import Tower.AADL.Build.Common

--------------------------------------------------------------------------------
-- Ramses build

-- Ramses Makefile ------------------------------------------------------------
ramsesMakefile :: AADLConfig -> [MkStmt]
ramsesMakefile c =
  [ include    aadlFilesMk
  , includeOpt "../RAMSES_PATH.mk"
  , "RAMSES_PATH" ?= "./"
  , "SMACCM_PATH" ?= "./"
  , export $"RAMSES_DIR" === "$(RAMSES_PATH)/ramses_resource"
  , export $"AADL2RTOS_CONFIG_DIR" === "$(RAMSES_PATH)/aadl2rtos_resource"
  , Target ".PHONY" ["all", "tower-clean", "ramses"] []
  , Target "ramses" []
    ["java -jar $(RAMSES_PATH)/ramses.jar -g rtos -i $(AADL2RTOS_CONFIG_DIR) \
          \-o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$(AADL_LIST)"]
  , Target "tower-clean" []
    [ rm aadlFilesMk
    , rm "*.aadl"
    , rm (configSrcsDir c)
    , rm (configHdrDir  c)
    ]
  ]
  where
  rm s = "-rm -rf " ++ s

--------------------------------------------------------------------------------
makefileLib :: AADLConfig -> [MkStmt]
makefileLib c =
  [ Comment "Targets"
  , "TARGETS" =: lib c ++ ".a"
  , Comment "Header files/directories this library provides"
  , "HDRFILES" =: "$(wildcard ${SOURCE_DIR}/include/*)"
  , "CFILES" =: "$(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/*.c))"
  ]

--------------------------------------------------------------------------------
echronosMakefileName :: FilePath
echronosMakefileName = "Makefile"

echronosMakefile :: [MkStmt]
echronosMakefile =
  [ "SHELL"       =: "/bin/bash"
  , "ROOT"        =: "$(shell pwd)"
  , "SRC"         =: "$(ROOT)/."
  , "EXE"         =: "sys"
  , "AS"          =: "arm-none-eabi-as -mthumb -g3 -mlittle-endian -mcpu=cortex-m4 \
                     \-mfloat-abi=hard -mfpu=fpv4-sp-d16 -I$(SRC) -I$(SRC)/../include"
  , "GCC"         =: "arm-none-eabi-gcc"
  , "GCC_FLAGS"   =: "-ffreestanding -Wall -Werror -mthumb -g3 -mlittle-endian \
                     \-mcpu=cortex-m4 -mfloat-abi=hard -mfpu=fpv4-sp-d16 -Os   \
                     \-I$(SRC) -I$(SRC)/../include -I$(SRC)/../libsmaccmpilot/include"
  , "LDSCRIPT"    =: "default.ld"
  , "LD"          =: "arm-none-eabi-ld -T $(LDSCRIPT)"
  , "SOURCES_GCC" =: "$(notdir $(wildcard $(SRC)/*.c))"
  , "SOURCES_AS"  =: "$(notdir $(wildcard $(SRC)/*.s))"
  , "OBJECTS_GCC" =: "$(SOURCES_GCC:.c=.o)"
  , "OBJECTS_AS"  =: "$(SOURCES_AS:.s=.o)"
  , "VPATH"       =: "$(SRC)"
  , Target "all"  ["$(EXE)"]
    ["@echo building program named $(EXE) in directory $(ROOT)"]
  , Target "$(EXE)" ["$(OBJECTS_AS)", "$(OBJECTS_GCC)"]
    ["@echo building executable from assembly files: $(OBJECTS_AS) and .c files: $(OBJECTS_GCC)"
    ,"@echo linking executable"
    ,"$(LD) -o $@ $^"]
  , Target "%.o" ["%.s"]
    ["@echo Compiling assembly file $<"
    ,"$(AS) -o $@  $<"]
  , Target "%.o" ["%.c"]
    ["@echo Compiling C file $<"
    ,"$(GCC) -o $@ -c $< $(GCC_FLAGS)"]
  , Target ".PHONY" ["clean"] []
  , Target "clean" []
    ["@echo remove all the object files"
    ,"rm -f *.o"
    ,"@echo remove the executable, if any"]
  ]

makefile :: [MkStmt]
makefile = [ includeOpt ramsesMakefileName ]
