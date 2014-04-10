{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.TaskCode
  ( TaskCode(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.Time
import Ivory.Tower.Types.Unique

data TaskCode =
  TaskCode
    { taskcode_taskname  :: Unique
    , taskcode_commprim  :: ModuleDef
    , taskcode_usercode  :: ModuleDef
    , taskcode_sys_init  :: forall s . Ivory (AllocEffects s) ()
    , taskcode_user_init :: forall s . Ivory (AllocEffects s) ()
    , taskcode_timer     :: forall s s2 . Ref s (Stored ITime)
                                    -> Ivory (AllocEffects s2) ()
    , taskcode_eventrxer :: forall s . Ivory (AllocEffects s) ()
    , taskcode_eventloop :: forall s . Ivory (AllocEffects s) ()
    }

