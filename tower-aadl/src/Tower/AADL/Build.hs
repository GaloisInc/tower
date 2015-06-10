-- Create Ramses build script.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build where

import Data.Char
import System.FilePath ((</>))
import Tower.AADL.Config (AADLConfig(..), lib)

--------------------------------------------------------------------------------
-- Ramses build

ramsesMakefile :: AADLConfig -> String
ramsesMakefile c = unlines
  [ "include " ++ aadlFilesMk
  , ""
  , "-include ../RAMSES_PATH.mk"
  , "RAMSES_PATH ?= ./"
  , ""
  , "export RAMSES_DIR=$(RAMSES_PATH)/ramses_resource"
  , "export AADL2RTOS_CONFIG_DIR=$(RAMSES_PATH)/aadl2rtos_resource"
  , ""
  , ".PHONY: all"
  , "all: " ++ camkesMakefileName
  , ""
  , ".PHONY: ramses"
  , "ramses: "
  , tab "java -jar $(RAMSES_PATH)/ramses.jar -g rtos -i $(AADL2RTOS_CONFIG_DIR) -o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$(AADL_LIST)"
  , ""
  , ".PHONY: " ++ camkesMakefileName
  , camkesMakefileName ++ ":"
  , tab $ "make ramses"
  , tab $ unwords ["cp ", mkTp, camkesMakefileName]
  , tab $ "rm -rf ../../libs/" ++ lib c
  , tab $ unwords ["cp -r ", lib c, "../../libs/"]
  , tab $ unwords ["cp -r ", lib c </> "include" </> "*", configHdrDir c]
  , ""
  , ".PHONY: tower-clean"
  , "tower-clean:"
  , rm aadlFilesMk
  , rm kbuildName
  , rm kconfigName
  , rm camkesMakefileName
  , rm "*.aadl"
  , rm (configSrcsDir c)
  , rm (configHdrDir  c)
  ]
  where
  rm s = tab "-rm -rf " ++ s
  mkTp = "make_template" </> makefileName

ramsesMakefileName :: String
ramsesMakefileName = "ramses.mk"

aadlFilesMk :: String
aadlFilesMk = "AADL_FILES.mk"

--------------------------------------------------------------------------------
-- Kbuild, Kconfig

kbuildLib :: String -> String
kbuildLib dir = unlines
  [ "libs-$(CONFIG_LIB_" ++ shellVar dir ++ ") += " ++ dir
  , dir ++ ": common $(libc)"
  ]

kbuildApp :: String -> String -> String
kbuildApp libdir dir = unlines
  [ "apps-$(CONFIG_APP_" ++ shellVar dir ++ ") += " ++ dir
  , dir ++ ": libsel4 libmuslc libsel4platsupport libsel4muslccamkes "
        ++ "libsel4camkes libsel4sync libsel4debug libsel4bench "
        ++ libdir
  ]

kbuildName :: String
kbuildName = "Kbuild"

kconfigLib :: String -> String -> String
kconfigLib prog dir = unlines
  [ "menuconfig LIB_" ++ shellVar dir
  , "    bool \"Shared code for " ++ prog ++ " app.\""
  , "    default n"
  , "    help"
  , "        Generated from Ivory/Tower."
  ]

kconfigApp :: String -> String -> String
kconfigApp prog dir = unlines
  [ "config APP_" ++ shellVar dir
  , "    bool \"Generated code for " ++ prog ++ " .\""
  , "    default n"
  , "    select APP_CAMKES_EXECUTIVE"
  , "    help"
  , "        Generated AADL from " ++ prog
  ]

kconfigName :: String
kconfigName = "Kconfig"

camkesMakefileName :: String
camkesMakefileName = "camkesmakefile.mk"

otherCamkesTargets :: String
otherCamkesTargets = "othercamkestargets.mk"

makefileLib :: AADLConfig -> String
makefileLib c = unlines
  [ "# Targets"
  , "TARGETS := " ++ lib c ++ ".a"
  , ""
  , "# Header files/directories this library provides"
  , "HDRFILES := $(wildcard ${SOURCE_DIR}/include/*)"
  , ""
  , "CFILES := \\"
  , "$(patsubst $(SOURCE_DIR)/%,%,$(wildcard $(SOURCE_DIR)/src/*.c))"
  , ""
  , "include $(SEL4_COMMON)/common.mk"
  , ""
  , "CFLAGS += -DODROID"
  , ""
  ]

mkLib :: AADLConfig -> String -> String
mkLib c modName = modName ++ "_LIBS += " ++ configLibDir c

makefileApp :: AADLConfig -> [String] -> String -> String
makefileApp c aadlFileNames dir = unlines
  [ "# Include assumes this is driven by seL4 build."
  , "# " ++ otherCamkesTargets ++ " must come first: the main camkes makefile"
  , "# is included at the end of " ++ camkesMakefileName
  , ""
  , "CFLAGS += -DODROID"
  , ""
  , incl (fromApps otherCamkesTargets)
  , incl (fromApps camkesMakefileName)
  , incl ramsesMakefileName
  , ""
  ] ++ unlines (map (mkLib c) aadlFileNames) ++ []
  where
  fromApps fl = "apps" </> dir </> fl
  incl = ("-include " ++)

makefileName :: String
makefileName = "Makefile"

--------------------------------------------------------------------------------
-- Helpers

tab :: String -> String
tab = ('\t' :)

shellVar :: String -> String
shellVar = map toUpper
