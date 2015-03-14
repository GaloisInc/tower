
module Ivory.Tower.Types.ThreadCode
  ( ThreadCode(..)
  , emptyThreadCode
  , insertUserThreadCode
  , insertEmitterThreadCode
  , insertGenThreadCode
  , addThreadCode
  ) where

import qualified Ivory.Tower.AST.Thread as AST

import Ivory.Language

data ThreadCode =
  ThreadCode
    { threadcode_thread  :: AST.Thread
    , threadcode_user    :: ModuleDef
    , threadcode_gen     :: ModuleDef
    , threadcode_emitter :: ModuleDef
    }

emptyThreadCode :: AST.Thread -> ThreadCode
emptyThreadCode t = ThreadCode
  { threadcode_thread  = t
  , threadcode_user    = return ()
  , threadcode_gen     = return ()
  , threadcode_emitter = return ()
  }

insertUserThreadCode :: ModuleDef -> ThreadCode -> ThreadCode
insertUserThreadCode m t =
  t { threadcode_user = threadcode_user t >> m }

insertEmitterThreadCode :: ModuleDef -> ThreadCode -> ThreadCode
insertEmitterThreadCode m t =
  t { threadcode_emitter = threadcode_emitter t >> m }

insertGenThreadCode :: ModuleDef -> ThreadCode -> ThreadCode
insertGenThreadCode m t =
  t { threadcode_gen = threadcode_gen t >> m }

addThreadCode :: ThreadCode -> ThreadCode -> ThreadCode
addThreadCode a b = insertUserThreadCode (threadcode_user b)
                  $ insertGenThreadCode  (threadcode_gen b)
                  $ insertEmitterThreadCode (threadcode_emitter b)
                  $ a


