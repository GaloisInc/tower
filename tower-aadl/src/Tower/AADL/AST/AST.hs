--
-- AST for the fragment of AADL we generate.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST.AST where

import Tower.AADL.AST.TypeAST

--------------------------------------------------------------------------------

data Package = Packagek
  { packageName      :: String
  , packageImports   :: [String]
  , packageSystem    :: [System]
  } deriving (Show, Eq)

data Thread = Thread
  { threadName       :: String
  , threadFeatures   :: [Feature]
  , threadProperties :: [ThreadProperty]
  } deriving (Show, Eq)

data Feature =
  ChannelFeature Channel
  deriving (Show, Eq)

type ChanLabel = String

-- Integer corresponds to Tower's SyncChan integer label.
data Channel = Channel
  { chanLabel     :: ChanLabel
  , chanHandle    :: ChannelHandle
  , chanType      :: ChanType
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
    Prim [String]
  | User [(FilePath, String)]
  deriving (Show, Eq)

type Bound = Integer

data ThreadProperty =
    DispatchProtocol DispatchProtocol
  | ThreadType ThreadType
  | ExecTime Integer Integer
  -- ^ Min bound, max bound.
  | StackSize Integer
  | Priority Integer
  | PropertySourceText SourceText
  | SendEvents [(ChanLabel, Bound)]
  deriving (Show, Eq)

data DispatchProtocol =
    Periodic Integer
  | Aperiodic
  deriving (Show, Eq)

data ThreadType =
    Passive
  | Active
  deriving (Show, Eq)

data Process = Process
  { processName        :: String
  , processComponents  :: [Thread]
  } deriving (Show, Eq)

data System = System
  { systemName       :: String
  , systemComponents :: [Process]
  -- ^ For eChronos and seL4, there will be one process per system.
  , systemProperties :: [SystemProperty]
  } deriving (Show, Eq)

data SystemProperty =
    SystemOS String
  | SystemHW String
  deriving (Show, Eq)

-- | An AADL identifier.
type LocalId = String

--------------------------------------------------------------------------------
