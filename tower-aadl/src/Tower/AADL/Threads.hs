--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Threads where

import Data.Monoid

import qualified Ivory.Tower.AST as A

----------------------------------------

data Threads = Threads
  { threadsExternal     :: [A.Monitor]
  , threadsPeriodic     :: [A.Thread]
  , threadsInit         :: [A.Monitor]
  , threadsFromExternal :: [A.Monitor]
  , threadsPassive      :: [A.Monitor]
  }

instance Monoid Threads where
  mempty = Threads [] [] [] [] []
  Threads a0 b0 c0 d0 e0  `mappend` Threads a1 b1 c1 d1 e1 =
    Threads (a0++a1) (b0++b1) (c0++c1) (d0++d1) (e0++e1)

injectExternalThread :: A.Monitor -> Threads
injectExternalThread m = Threads [m] [] [] [] []

injectPeriodicThread :: A.Thread -> Threads
injectPeriodicThread m = Threads [] [m] [] [] []

injectInitThread :: A.Monitor -> Threads
injectInitThread m = Threads [] [] [m] [] []

injectFromExternalThread :: A.Monitor -> Threads
injectFromExternalThread m = Threads [] [] [] [m] []

injectPassiveThread :: A.Monitor -> Threads
injectPassiveThread m = Threads [] [] [] [] [m]

class ThreadName a where
  threadName :: a -> String

instance ThreadName A.Monitor where
  threadName = A.monitorName

instance ThreadName A.Thread where
  threadName = A.threadName
