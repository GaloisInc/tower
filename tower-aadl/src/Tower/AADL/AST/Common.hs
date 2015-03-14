--
-- Pre-processing helpers for the AST to extract all of the channels associated
-- with a process.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST.Common
  ( ChanIds
  , getTxLabels
  , getRxLabels
  , filterEndpoints
  , threadsChannels
  , extractTypes
  , mapConnections
  , emptyConnections
  ) where

import           Tower.AADL.AST
import qualified Ivory.Language.Syntax.Type as I

import Prelude hiding (id)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.List (foldl')

--------------------------------------------------------------------------------

data ChanIds = ChanIds
  { chanTxLabels :: S.Set LocalId
  , chanRxLabels :: S.Set LocalId
  } deriving (Show, Eq)

instance Monoid ChanIds where
  mempty = ChanIds mempty mempty
  c0 `mappend` c1 =
    ChanIds (chanTxLabels c0 `mappend` chanTxLabels c1)
            (chanRxLabels c0 `mappend` chanRxLabels c1)

getTxLabels :: ChanIds -> [LocalId]
getTxLabels = S.toList . chanTxLabels

getRxLabels :: ChanIds -> [LocalId]
getRxLabels = S.toList . chanRxLabels

type Connections = M.Map ChanLabel ChanIds

-- Interface below hides the data structure.

mapConnections :: (ChanLabel -> ChanIds -> a) -> Connections -> [a]
mapConnections cs = M.elems . M.mapWithKey cs

emptyConnections :: Connections -> Bool
emptyConnections = M.null

-- | Remove connections that don't have both endpoints.
filterEndpoints :: Connections -> Connections
filterEndpoints = M.filter go
  where
  go c = not (S.null (chanTxLabels c) || S.null (chanRxLabels c))

-- Given a list of pairs of AADL threads and local variables, Create their
-- connections.
threadsChannels :: [(Thread, LocalId)] -> Connections
threadsChannels ls = foldl' go M.empty ls
  where
  go :: Connections -> (Thread, LocalId) -> Connections
  go cs (th, id) =
    (M.unionWith mappend) (threadChannels th id) cs

threadChannels :: Thread -> LocalId -> Connections
threadChannels th id = foldl' go M.empty (getThreadEndpoints th)
    where
    go :: Connections -> Endpoint -> Connections
    go cs = insertConnectionLabel id cs

data Endpoint =
    InputEp  Input
  | OutputEp Output
  deriving (Show, Eq)

endPointLabel :: Endpoint -> ChanLabel
endPointLabel ep = case ep of
  InputEp  rx -> inputLabel  rx
  OutputEp tx -> outputLabel tx

newChan :: LocalId -> Endpoint -> ChanIds
newChan l ep =
  case ep of
    InputEp{}  -> ChanIds S.empty (S.singleton l)
    OutputEp{} -> ChanIds (S.singleton l) S.empty

-- Add the id to the connections map, creating a new channel if needed.
insertConnectionLabel :: LocalId -> Connections -> Endpoint -> Connections
insertConnectionLabel l cs ep =
  M.insertWith mappend (endPointLabel ep) (newChan l ep) cs

getThreadEndpoints :: Thread -> [Endpoint]
getThreadEndpoints t =
  map go (threadFeatures t)
  where
  go f =
    case f of
      InputFeature  rx -> InputEp  rx
      OutputFeature tx -> OutputEp tx

-- Extract a unique instance of the channel types defined in the system.
extractTypes :: System -> [I.Type]
extractTypes sys = S.toList $ S.map getTy (S.fromList fs)
  where
  fs :: [Feature]
  fs = concatMap threadFeatures
     $ concatMap processComponents
     $ systemComponents sys
  getTy f = case f of
    InputFeature  rx -> inputType  rx
    OutputFeature tx -> outputType tx
