--
-- Pre-processing helpers for the AST to extract all of the channels associated
-- with a process.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST.Common
  ( ThdIds
  , getTxThds
  , getRxThds
  , filterEndpoints
  , threadsChannels
  , extractTypes
  , allConnections
  , connectedThreadsSize
  , emptyConnections
  , towerTime
  ) where


import           Prelude ()
import           Prelude.Compat hiding (id)

import           Tower.AADL.AST
import qualified Ivory.Language.Syntax.Type as I

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.List (foldl')

--------------------------------------------------------------------------------

type ThreadChans = (LocalId, ChanLabel)

-- Used in system composition. All the threads that are connected.
data ThdIds = ThdIds
  { chanTxThds :: S.Set ThreadChans
  , chanRxThds :: S.Set ThreadChans
  } deriving (Show, Eq)

instance Monoid ThdIds where
  mempty = ThdIds mempty mempty
  c0 `mappend` c1 =
    ThdIds (chanTxThds c0 `mappend` chanTxThds c1)
            (chanRxThds c0 `mappend` chanRxThds c1)

getTxThds :: ThdIds -> [ThreadChans]
getTxThds = S.toList . chanTxThds

getRxThds :: ThdIds -> [ThreadChans]
getRxThds = S.toList . chanRxThds

-- A mapping from channels to the sending and receiving threads on the channel.
type Connections = M.Map ChanId ThdIds

-- Interface below hides the data structure.

allConnections :: Connections -> [ThdIds]
allConnections = M.elems

connectedThreadsSize :: ThdIds -> Int
connectedThreadsSize thds =
  S.size (chanTxThds thds) * S.size (chanRxThds thds)

emptyConnections :: Connections -> Bool
emptyConnections = M.null

-- | Remove connections that don't have both endpoints.
filterEndpoints :: Connections -> Connections
filterEndpoints = M.filter go
  where
  go c = not (S.null (chanTxThds c) || S.null (chanRxThds c))

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
    go cs = insertConnectionId id cs

data Endpoint =
    InputEp  Input
--  | SignalEp SignalInfo
  | OutputEp Output
  deriving (Show, Eq)

endPointId :: Endpoint -> ChanId
endPointId ep = case ep of
  InputEp  rx -> inputId  rx
  OutputEp tx -> outputId tx
--  SignalEp s  -> SignalChanId $ fromIntegral (signalInfoNumber s)

newChan :: LocalId -> Endpoint -> ThdIds
newChan l ep =
  case ep of
    InputEp  c -> ThdIds S.empty (S.singleton (l, inputLabel c))
    OutputEp c -> ThdIds (S.singleton (l, outputLabel c)) S.empty
    -- TODO JED: This is probably wrong
--    SignalEp c -> ThdIds S.empty (S.singleton (l, signalInfoName c))

-- Add the id to the connections map, creating a new channel if needed.
insertConnectionId :: LocalId -> Connections -> Endpoint -> Connections
insertConnectionId l cs ep =
  M.insertWith mappend (endPointId ep) (newChan l ep) cs

getThreadEndpoints :: Thread -> [Endpoint]
getThreadEndpoints t =
  concatMap go (threadFeatures t)
  where
  go f =
    case f of
      InputFeature  rx -> [InputEp  rx]
      OutputFeature tx -> [OutputEp tx]
      SignalFeature _  -> []

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
    -- TODO JED: Wouldn't this be so much nicer if the Time module told us what
    -- type to put here?
    SignalFeature _  -> towerTime

towerTime :: I.Type
towerTime = I.TyInt I.Int64
