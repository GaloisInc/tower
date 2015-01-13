--
-- AST for the fragment of AADL we generate.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST where

import Data.List

import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Tower.AST.Comment    as C

--------------------------------------------------------------------------------

type Import = String

data System = System
  { systemName       :: !Name
  , systemComponents :: [Process]
  -- ^ For eChronos and seL4, there will be one process per system.
  , systemProperties :: [SystemProperty]
  } deriving (Show, Eq)

data Process = Process
  { processName        :: !Name
  , processComponents  :: [Thread]
  } deriving (Show, Eq)

data SystemProperty =
    SystemOS !String
  | SystemHW !String
  deriving (Show, Eq)

data Thread = Thread
  { threadName       :: !Name
  , threadFeatures   :: [Feature]
  , threadProperties :: [ThreadProperty]
  , threadComments   :: [C.Comment]
  } deriving (Show, Eq)

data Feature =
  ChannelFeature Channel
  deriving (Show, Eq)

-- Integer corresponds to Tower's SyncChan integer label.
data Channel = Channel
  { chanLabel     :: !ChanLabel
  , chanHandle    :: ChannelHandle
  , chanType      :: !I.Type
  -- ^ The Ivory AADL backend has converted an Ivory Type into an AADL type.
  , chanCallbacks :: SourceText
  } deriving (Show, Eq)

data ChannelHandle =
    Input
  -- ^ AADL semantics: input into channel.
  | Output
  -- ^ AADL semantics: output from channel.
  deriving (Show, Eq)

data SourceText =
    Prim [FuncSym]
  | User [(FilePath, FuncSym)]
  deriving (Show, Eq)

data ThreadProperty =
    DispatchProtocol DispatchProtocol
  | ThreadType !ThreadType
  | ExecTime !Integer !Integer
  -- ^ Min bound, max bound.
  | StackSize Integer
  | Priority Integer
  | PropertySourceText SourceText
  | SendEvents [(ChanLabel, Bound)]
  deriving (Show, Eq)

data DispatchProtocol =
    Periodic !Integer
  | Aperiodic
  deriving (Show, Eq)

data ThreadType =
    Passive
  | Active
  deriving (Show, Eq)

-- | An AADL variable.
type LocalId = String

-- | An AADL identifier.
type Name = String

-- | Channel label.
type ChanLabel = String

-- | Channel bound.
type Bound = Integer

-- | Function symbol.
type FuncSym = String

--------------------------------------------------------------------------------

-- -- | Takes a `System` and decomposes the `Thread`s, returning the list of
-- -- threads and the origional system with the threads extracted.
-- decomposeThreads :: System -> (System, [Thread])
-- decomposeThreads sys =
--   (sys', concat thds)
--   where
--   sys' = sys { systemName       = systemName sys
--              , systemComponents = comps
--              , systemProperties = systemProperties sys
--              }
--   (comps,thds) = unzip (map go (systemComponents sys))
--   go p = ( p { processName       = processName p
--              , processComponents = []
--              }
--          , processComponents p
--          )

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
