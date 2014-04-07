
module Ivory.Tower.Compile.AADL.SystemDoc
  ( systemDoc
  ) where

import System.FilePath

import           Ivory.Compile.AADL.AST
import           Ivory.Compile.AADL.Identifier
import           Ivory.Compile.AADL.Monad
import           Ivory.Compile.AADL.Gen (mkType, typeImpl)

import           Ivory.Language
import           Ivory.Tower
import           Ivory.Tower.Types.Unique
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.AST.Directory as D

type CompileAADL = CompileM (Unique,[String])

systemDoc :: String -> [Module] -> AST.System p
          -> TypeCtxM (Document, [Warning])
systemDoc name ms sysast = runCompile ms virtMod $ do
  writeImport "SMACCM_SYS"
  tg <- threadGroup [] (AST.system_tasks sysast)
  writeThreadGroup tg
  where
  virtMod = package name $ do
    mapM_ depend ms


type ThreadGroup = [ThreadDef] -- XXX change later.

writeThreadGroup :: ThreadGroup -> CompileAADL ()
writeThreadGroup = mapM_ writeThreadDefinition

threadGroup :: [Unique] -> D.Dir Unique (AST.Task p) -> CompileAADL ThreadGroup
threadGroup path (D.Dir ts subdirs) = do
  tdefs   <- mapM threadDef ts
  subdefs <- mapM recur   subdirs
  return (concat (tdefs : subdefs))
  where
  recur (D.Subdir n d) = threadGroup (n:path) d


threadDef :: AST.Task p -> CompileAADL ThreadDef
threadDef t = do
  threadname <- introduceUnique (AST.task_name t) []
  features <- featuresDef threadname t (loopsource <.> "h")
  let props = [ ThreadProperty "Source_Text"
                  (PropList [PropString (usersource <.> "c")])
              , ThreadProperty "Priority"
                  (PropInteger (AST.task_priority t))
              , ThreadProperty "Source_Stack_Size"
                  (PropUnit (AST.task_stack_size t) "bytes")
              , ThreadProperty "SMACCM_SYS::Language"
                  (PropString "Ivory")
              ]

  return (ThreadDef threadname features props)
  where
  loopsource = "tower_task_loop_" ++ (showUnique (AST.task_name t))
  usersource = "tower_task_loop_" ++ (showUnique (AST.task_name t))

featuresDef :: String -> AST.Task p -> FilePath -> CompileAADL [ThreadFeature]
featuresDef scope taskast headername = do
  return []

-- Uniqueness managment -------------------------------------------------------

introduceUnique :: Unique -> [String] -> CompileAADL String
introduceUnique u scope = do
  m <- getIdentifierMap
  let mm = filter ((== scope) . snd . fst) m
  case elem up (map snd mm)  of
    False -> do
      setIdentifierMap (((u,scope),up):m)
      return up
    True -> do
      uniquenessWarning warning
      let up' = notUnique mm (showUnique u)
      setIdentifierMap (((u,scope),up'):m)
      return up'
  where
  up = unique_name u
  warning = "User provided identifier " ++ up
         ++ " is not unique in tower-aadl Assembly."
  notUnique m r = case elem r (map snd m) of
    True -> notUnique m (r ++ "_1")
    False -> r

uniqueIdentifier :: Unique -> String -> CompileAADL String
uniqueIdentifier u ctx = do
  m <- getIdentifierMap
  -- If this lookup fails, its because the CompileM code that should have used
  -- introduceUnique to create names was not implemented correctly.
  case lookup u (map withoutScope m) of
    Just n -> return n
    Nothing -> error ("Failed to find unique identifier " ++ (showUnique u)
                      ++ " in " ++ ctx ++ " context.\nName Map Dump\n" ++ (show m))
  where
  withoutScope ((k,_s),v) = (k,v)
