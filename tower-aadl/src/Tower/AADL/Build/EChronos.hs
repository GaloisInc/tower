-- Create Ramses build script.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build.EChronos where

import System.FilePath

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
  , Target ".PHONY" ["all", "tower-clean"] []
  , Target ".tag.ramses" []
    ["java -jar $(RAMSES_PATH)/ramses.jar -g rtos -i $(AADL2RTOS_CONFIG_DIR) \
          \-o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$(AADL_LIST)"
    ,"touch .tag.ramses"
    ]
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
-- TODO: deleteme
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
  , "AS"          =: "arm-none-eabi-as -mthumb -g3 -mlittle-endian -mcpu=cortex-m4 \\\n\
               \      -mfloat-abi=hard -mfpu=fpv4-sp-d16 -I$(SRC) -I$(SRC)/include"
  , "CC"          =: "arm-none-eabi-gcc"
  , "CFLAGS"      =: "-Os -g3 -Wall -Werror              \\\n\
           \          -std=gnu99                         \\\n\
           \          -Wno-parentheses                   \\\n\
           \          -Wno-unused-function               \\\n\
           \          -Wno-unused-variable               \\\n\
           \          -Wno-main                          \\\n\
           \          -mlittle-endian                    \\\n\
           \          -mthumb -mcpu=cortex-m4            \\\n\
           \          -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\\n\
           \          -I$(SRC)                           \\\n\
           \          -I$(SRC)/include                   \\\n\
           \          -I$(SRC)/gen                       \\\n\
           \          -I$(SRC)/echronos_gen              \\\n\
           \          -I$(SRC)/libsmaccmpilot/include"
  , "LDSCRIPT"    =: "$(SRC)/echronos_gen/default.ld"
  , "LDFLAGS"     =: "-Wl,--script=$(LDSCRIPT)           \\\n\
          \           -nostartfiles                      \\\n\
          \           -mlittle-endian                    \\\n\
          \           -mthumb -mcpu=cortex-m4            \\\n\
          \           -mfloat-abi=hard -mfpu=fpv4-sp-d16 \\\n\
          \           -lm"
  , "LD"          =: "arm-none-eabi-gcc"
  , "SOURCES_GCC" =: "$(wildcard $(SRC)/libsmaccmpilot/src/*.c) \\\n\
      \               $(wildcard $(SRC)/gen/*.c)                \\\n\
      \               $(wildcard $(SRC)/echronos_gen/*.c)"
  , "SOURCES_AS"  =: "$(wildcard $(SRC)/libsmaccmpilot/src/*.s) \\\n\
       \              $(wildcard $(SRC)/gen/*.s)                \\\n\
       \              $(wildcard $(SRC)/echronos_gen/*.s)"
  , "OBJECTS_GCC" =: "$(SOURCES_GCC:.c=.o)"
  , "OBJECTS_AS"  =: "$(SOURCES_AS:.s=.o)"
  , "VPATH"       =: "$(SRC)"
  , Target "$(EXE)" ["$(OBJECTS_GCC)", "$(OBJECTS_AS)"]
    ["@echo building executable from assembly files: $(OBJECTS_AS) and .c files: $(OBJECTS_GCC)"
    ,"@echo linking executable"
    ,"$(LD) $(LDFLAGS) -o $@ $^"]
  , Target ".PHONY" ["clean"] []
  , Target "clean" []
    ["@echo remove all the object files"
    ,"rm -f *.o"
    ,"@echo remove the executable, if any"]
  ]

makefile :: [MkStmt]
makefile = [  -- Make sure 'all' is the default target
             Target "all" ["generate"]
             [ "make $(EXE)" ] -- This hack is here to deal with $(EXE) depending on
                               -- files that have to be generated first. This requires us
                               -- to do the build in two phases.
           , includeOpt ramsesMakefileName
           , Comment "We assume ECHRONOS_LOCATION and PRJ are set in PRJ_CMD.mk \\\n\
                     \ECHRONOS_LOCATION should be the path to the echronos install where\\\n\
                     \the setenv script and packages can be found. For example, the top of\\\n\
                     \your echronos repository. PRJ should point to the prj tool."
           , includeOpt "../PRJ_CMD.mk"
           , "PRJ" ?= "prj"
           , "ECHRONOS_LOCATION" ?= "$(shell which prj)"
           , Target ".PHONY" ["generate"] []
           , Target "generate" [".tag.echronos", ".tag.ramses"] []
           , Target ".tag.echronos" [".tag.ramses"]
             [ "pushd $(ECHRONOS_LOCATION) && source setenv && popd &&  \\\n\
             \  $(PRJ) --output echronos_gen                            \\\n\
             \         --search-path $(ECHRONOS_LOCATION)/packages      \\\n\
             \         --no-project                                     \\\n\
             \         gen                                              \\\n\
             \         sys_impl.prx"
             , "touch .tag.echronos"
             ]
           , include    ("gen" </> echronosMakefileName) ]
