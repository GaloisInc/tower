-- Create Ramses build script.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build where

import Tower.AADL.Config (Config(..))

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

makefile :: Config -> String
makefile c = unlines
  [
    ".PHONY: ramses"
  , "ramses:"
  , tab "sh build.sh"
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
  where tab = ('\t' :)
