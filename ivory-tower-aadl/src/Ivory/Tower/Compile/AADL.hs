{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.AADL
  ( compile
  , assemblyDoc
  , searchDir
  ) where

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Tower (assembleTower)

import Ivory.Tower.Compile.AADL.SearchDir (searchDir)
import Ivory.Tower.Compile.AADL.OS (os)
import Ivory.Tower.Compile.AADL.Modules (buildModules)
import Ivory.Tower.Compile.AADL.Assembly (assemblyDoc)

compile :: Tower p () -> (Assembly, [Module])
compile t = (asm, ms)
  where
  asm = assembleTower t os
  ms = buildModules asm

