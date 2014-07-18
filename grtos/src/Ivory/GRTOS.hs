{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend (compile)

import Ivory.GRTOS.PriorityGroup
import Ivory.GRTOS.AST

test :: IO ()
test = compile [m]
  where
  code pc = package "pkg" $ pg_moduledef pc

  -- XXX is there anything in the ghc 7.8's typelits that can do this
  -- dispatch for us, e.g. a magic dictionary
  m = case ((length es) `div` 32) + 1 of
        1 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 1)
        2 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 2)
        3 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 3)
        4 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 4)
        5 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 5)
        6 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 6)
        _ -> error "priorityGroup test: failed, no singleton for groups larger than 6"

  es = take 33 $ map mkevt [1..]
  mkevt n = ( Event n ("e" ++ (show n))
            , Callback (comment ("stub ivory code for event " ++ (show n))))

