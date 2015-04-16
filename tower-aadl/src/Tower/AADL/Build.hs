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

buildScript :: String
buildScript = unlines
  [
    "#!/bin/sh"
  , ""
  , "# Source AADL_FILES"
  , ". AADL_FILES"
  , ""
  , "# Source Ramses path"
  , ". ../RAMSES_PATH"
  , ""
  , "export RAMSES_DIR=$RAMSES_PATH/ramses_resource"
  , "export AADL2RTOS_CONFIG_DIR=$RAMSES_PATH/aadl2rtos_resource"
  , ""
  , "java -jar $RAMSES_PATH/ramses.jar -g rtos -i $AADL2RTOS_CONFIG_DIR -o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$AADL_LIST"
  ]

buildScriptName :: String
buildScriptName = "ramses-build.sh"

ramsesMakefile :: Config -> String
ramsesMakefile c = unlines
  [
    ".PHONY: ramses"
  , "ramses:"
  , tab "sh " ++ buildScriptName
  , ""
  , camkesMakefileName ++ ":" ++ mkTp
  , tab $ unwords ["cp ", mkTp, camkesMakefileName]
  , ""
  , ".PHONY: tower-clean"
  , "tower-clean:"
  , tab "-rm build.sh"
  , tab "-rm AADL_FILES"
  , tab "-rm Makefile"
  , tab "-rm *.aadl"
  , tab "-rm -rf " ++ configSrcsDir c
  , tab "-rm -rf " ++ configHdrDir  c
  ]
  where
  mkTp = "make_template" </> makefileName

ramsesMakefileName :: String
ramsesMakefileName = "ramses.mk"

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
  , incl (fromApps camkesMakefileName)
  , incl (fromApps otherCamkesTargets)
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
