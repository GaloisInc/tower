
module Ivory.Tower.Tower
  ( Tower
  , towerArtifact
  , towerDepends
  , towerModule
  , towerGroup
  ) where

import Ivory.Language
import Ivory.Tower.Types.Artifact
import Ivory.Tower.Types.SystemCode
import Ivory.Tower.Types.TaskCode
import Ivory.Tower.Monad.Tower

towerArtifact :: Artifact -> Tower p ()
towerArtifact = putArtifact

towerModule :: Module -> Tower p ()
towerModule = putModule

towerDepends :: Module -> Tower p ()
towerDepends m = do
  c <- getSystemCode
  setSystemCode $ \sys ->
    (c sys) { systemcode_tasks = map task_depend (systemcode_tasks (c sys))
            , systemcode_moddef = depend m >> (systemcode_moddef (c sys))
            }
  where
  task_depend tc = tc { taskcode_commprim = depend m >> taskcode_commprim tc }

towerGroup :: String -> Tower p a -> Tower p a
towerGroup = group
