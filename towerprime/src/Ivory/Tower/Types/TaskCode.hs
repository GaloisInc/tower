{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.TaskCode
  ( TaskCode(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.Time

data TaskCode =
  TaskCode
    { taskcode_commprim  :: ModuleDef
    , taskcode_usercode  :: ModuleDef
    , taskcode_init      :: forall s . Ivory (AllocEffects s) ()
    , taskcode_timer     :: forall s . Ref (Stack s) (Stored ITime)
                                    -> Ivory (AllocEffects s) ()
    , taskcode_eventrxer :: forall s . Ivory (AllocEffects s) ()
    , taskcode_eventloop :: forall s . Ivory (AllocEffects s) ()
    }

