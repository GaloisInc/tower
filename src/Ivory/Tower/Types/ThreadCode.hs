
module Ivory.Tower.Types.ThreadCode
  ( ThreadCode(..)
  , emptyThreadCode
  , insertUserThreadCode
  , insertGenThreadCode
  , addThreadCode
  ) where

import qualified Ivory.Tower.AST.Thread as AST

import Ivory.Tower.ToyObjLang

data ThreadCode =
  ThreadCode
    { threadcode_thread :: AST.Thread
    , threadcode_user   :: ModuleM ()
    , threadcode_gen    :: ModuleM ()
    }

emptyThreadCode :: AST.Thread -> ThreadCode
emptyThreadCode t = ThreadCode
  { threadcode_thread = t
  , threadcode_user   = return ()
  , threadcode_gen    = return ()
  }

insertUserThreadCode :: ModuleM () -> ThreadCode -> ThreadCode
insertUserThreadCode m t = t { threadcode_user = threadcode_user t >> m }

insertGenThreadCode :: ModuleM () -> ThreadCode -> ThreadCode
insertGenThreadCode m t = t { threadcode_gen = threadcode_gen t >> m }

addThreadCode :: ThreadCode -> ThreadCode -> ThreadCode
addThreadCode a b = insertUserThreadCode (threadcode_user b)
                  $ insertGenThreadCode  (threadcode_gen b)
                  $ a


