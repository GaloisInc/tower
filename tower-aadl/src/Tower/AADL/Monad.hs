{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Monad for code generation.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Monad where

import qualified Ivory.Tower.AST as A
import qualified Ivory.Language.Syntax.Type as I

import Control.Applicative
import qualified Data.Map as Map
import MonadLib

--------------------------------------------------------------------------------

newtype GenM a = GenM { unGenM :: ReaderT Config -- E.g., channel type map
                                    (WriterT Log -- Warnings, errors
                                      Id) a
                       } deriving (Functor, Applicative, Monad)

type ChanTypes = Map.Map A.Chan I.Type

data Config = Config
  { configChanTypes :: ChanTypes
  } deriving (Show, Eq)

-- XXX make efficient
type Log = [String]

--------------------------------------------------------------------------------

runGenM :: Config -> GenM a -> (a, Log)
runGenM config m =
  runId (runWriterT (runReaderT config (unGenM m)))

putLog :: String -> GenM ()
putLog s = GenM (lift (put [s]))


