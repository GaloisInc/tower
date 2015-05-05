-- Create Ramses build script.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build where

import Data.Char
import System.FilePath ((</>))
import Tower.AADL.Config (Config(..))

--------------------------------------------------------------------------------
-- Ramses build

ramsesMakefile :: Config -> String
ramsesMakefile c = unlines
  [ "include " ++ aadlFilesMk
  , ""
  , "-include ../RAMSES_PATH.mk"
  , "RAMSES_PATH ?= ./"
  , ""
  , "export RAMSES_DIR=$(RAMSES_PATH)/ramses_resource"
  , "export AADL2RTOS_CONFIG_DIR=$(RAMSES_PATH)/aadl2rtos_resource"
  , ""
  , ".PHONY: ramses"
  , "ramses: " ++ camkesMakefileName
  , tab "java -jar $(RAMSES_PATH)/ramses.jar -g rtos -i $(AADL2RTOS_CONFIG_DIR) -o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$(AADL_LIST)"
  , ""
  , camkesMakefileName ++ ":" ++ mkTp
  , tab $ unwords ["cp ", mkTp, camkesMakefileName]
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

kbuild :: String -> String
kbuild dir = unlines
  [ "apps-$(CONFIG_APP_" ++ shellVar dir ++ ") += " ++ dir
  , dir ++ ": libsel4 libmuslc libsel4platsupport libsel4muslccamkes libsel4camkes libsel4sync libsel4debug libsel4bench"
  ]

kbuildName :: String
kbuildName = "Kbuild"

kconfig :: String -> String -> String
kconfig prog dir = unlines
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

makefile :: String -> String
makefile dir = unlines
  [ "# Include assumes this is driven by seL4 build."
  , "# " ++ otherCamkesTargets ++ " must come first: the main camkes makefile"
  , "# is included at the end of " ++ camkesMakefileName
  , incl (fromApps otherCamkesTargets)
  , incl (fromApps camkesMakefileName)
  , incl ramsesMakefileName
  ]
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
