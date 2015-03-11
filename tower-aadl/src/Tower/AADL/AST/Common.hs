--
-- Pre-processing helpers for the AST to extract all of the channels associated
-- with a process.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST.Common where

import           Tower.AADL.AST
import qualified Ivory.Language.Syntax.Type as I

import Data.List (nubBy, nub)

--------------------------------------------------------------------------------

data Connection = Connection
  { connectionLabel    :: ChanLabel
  , connectionTxLabels :: [LocalId]
  , connectionRxLabels :: [LocalId]
  } deriving (Show, Eq)

sameChannel :: Channel -> Channel -> Bool
sameChannel c0 c1 = chanLabel c0 == chanLabel c1

-- | Remove connections that don't have both endpoints.
filterEndpoints :: [Connection] -> [Connection]
filterEndpoints = filter b
  where
  b c = not (null (connectionTxLabels c) || null (connectionRxLabels c))

threadChannels :: [(Thread, LocalId)] -> [Connection]
threadChannels ls = do
  c <- nubBy sameChannel (concatMap threadChans threads)
  let rx = chanThreadIn c threads
  let tx = chanThreadOut c threads
  return (Connection (chanLabel c) (getLabels tx) (getLabels rx))
  where
  threads = map fst ls
  getLabels = map go
    where
    go t = case lookup t ls of
             Nothing -> error $ "Impossible lookup in threadChannels."
             Just l  -> l

-- All channels in a thread.
threadChans :: Thread -> [Channel]
threadChans t = map go (threadFeatures t)
  where
  go f = case f of
    ChannelFeature c
      -> c

-- All input threds for a channel.
chanThreadIn :: Channel -> [Thread] -> [Thread]
chanThreadIn = filterChans Input

chanThreadOut :: Channel -> [Thread] -> [Thread]
chanThreadOut = filterChans Output

filterChans :: ChannelHandle -> Channel -> [Thread] -> [Thread]
filterChans h c ts =
  [ t
  | t  <- ts
  , c' <- filter (sameChannel c) (threadChans t)
  , chanHandle c' == h
  ]

-- Extract a unique instance of the types defined in the system.
extractTypes :: System -> [I.Type]
extractTypes sys =
    nub
  $ map go
  $ concatMap threadFeatures
  $ concatMap processComponents
  $ systemComponents sys
  where
  go cf = case cf of
            ChannelFeature c -> chanType c
