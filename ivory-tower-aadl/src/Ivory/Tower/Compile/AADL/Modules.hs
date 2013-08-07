
module Ivory.Tower.Compile.AADL.Modules (buildModules) where

buildModules :: Assembly -> [Module]
buildModules asm = ms
  where
  towerst = asm_towerst asm
  tasks   = asm_tasks   asm
  signals = asm_sigs    asm
  (sys_mdef, _) = asm_system asm

  node_genchannels = getNodeCodegen tasks ++ getNodeCodegen signals

  ms = [ tower_entry ] ++  tower_tasks ++ [ tower_commprim ]
    ++ towerst_modules towerst
    ++ concatMap (taskst_extern_mods . nodest_impl . an_nodest) tasks

  tower_commprim = package "tower_commprim" $ do
    -- Generated code: channels, dataports
    mapM_ cgen_mdef node_genchannels
    mapM_ cgen_mdef (towerst_dataportgen towerst)
    -- System code
    sys_mdef
    -- Dependencies
    mapM_ depend (towerst_depends towerst)

  systemDeps :: ModuleDef
  systemDeps = do
    depend tower_commprim
    mapM_ depend (towerst_depends towerst)

  nodeModules :: AssembledNode a -> [Module]
  nodeModules n = an_modules n systemDeps

  tower_tasks :: [Module]
  tower_tasks = concatMap nodeModules tasks ++ concatMap nodeModules signals

  tower_entry = package "tower" $ do
    -- System-wide code
    incl towerentry
    -- Dependencies
    mapM_ depend tower_tasks
    depend tower_commprim

  towerentry :: Def ('[]:->())
  towerentry = externProc "tower_entry" 


getNodeCodegen :: [AssembledNode a] -> [Codegen]
getNodeCodegen as = concatMap (nodest_codegen . an_nodest) as


