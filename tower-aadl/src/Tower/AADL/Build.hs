-- Create Ramses build script.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Build where

buildScript :: String
buildScript = unlines
  [
    "#!/bin/sh"
  , ""
  , "# Source AADL_FILES"
  , ". $AADL_FILES"
  , ""
  , "RAMSES=$PATH_TO_RAMSES/ramses-demo"
  , ""
  , "export RAMSES_DIR=$RAMSES/ramses_resource"
  , "export AADL2RTOS_CONFIG_DIR=$RAMSES/aadl2rtos_resource"
  , ""
  , "java -jar $RAMSES/ramses.jar -g rtos -i $AADL2RTOS_CONFIG_DIR -o . -l trace -s sys.impl -m SMACCM_SYS.aadl,$AADL_LIST"
  ]
