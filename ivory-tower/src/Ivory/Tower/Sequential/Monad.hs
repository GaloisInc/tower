{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Sequential.Monad where

import MonadLib

import Ivory.Language hiding (local)

import Ivory.Tower
import Ivory.Tower.Types
import qualified Ivory.Tower.Sequential.AST as A

newtype Sequential p f t a =
  Sequential
    { unSequential :: WriterT (A.SM f t) (Node TaskSt p) a
    } deriving (Functor, Monad)

runSequentialMonad :: Sequential p f t () -> Task p (A.SM f t)
runSequentialMonad m = do
  (_, sm) <- runWriterT (unSequential m)
  return sm

writeSM :: A.SM f t -> Sequential p f t ()
writeSM s = Sequential $ put s

-- Public API:

local :: (IvoryArea area) => Name -> Sequential p t f (Ref Global area)
local n = Sequential $ lift $ taskLocal n

localInit :: (IvoryArea area) => Name -> Init area -> Sequential p t f (Ref Global area)
localInit n i = Sequential $ lift $ taskLocalInit n i

instance BaseUtils (Sequential p f t) where
  fresh = Sequential $ lift fresh
  getOS = Sequential $ lift getOS

start :: [A.Stmt t] -> Sequential p f t ()
start s = writeSM $ A.start s

receive :: Ref s f -> [A.Stmt t] -> Sequential p f t ()
receive r s = writeSM $ A.blockReceive r s

delay :: Integer -> [A.Stmt t] -> Sequential p f t ()
delay t s = writeSM $ A.blockTime t s

end :: [A.Stmt t] -> Sequential p f t ()
end s = writeSM $ A.end s


