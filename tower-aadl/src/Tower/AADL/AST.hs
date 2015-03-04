--
-- AST for the fragment of AADL we generate.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST where

import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Tower.AST.Comment    as C

--------------------------------------------------------------------------------

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
  -- ^ AADL semantics: input to handler. Associated with an entry source and a
  | Output
  -- ^ AADL semantics: output from of handler. Associated an output function.
  deriving (Show, Eq)

-- | Path to a .c file and a function symbol in the file.
type SourcePath = (FilePath, FuncSym)

data SourceText =
    Prim FuncSym
  | User [SourcePath]
  deriving (Show, Eq)

data ThreadProperty =
    DispatchProtocol DispatchProtocol
  | ThreadType !ThreadType
  | ExecTime !Integer !Integer
  -- ^ Min bound, max bound.
  | StackSize Integer
  | Priority Integer
  | PropertySourceText !SourcePath
  -- ^ Path to a .c file
  | SendEvents [(ChanLabel, Bound)]
  deriving (Show, Eq)

data DispatchProtocol =
    Periodic !Integer
  | Signal !SignalName !Address
  | Aperiodic
  deriving (Show, Eq)

data SignalInfo = SignalInfo
  { signalName     :: SignalName
  , signalNumber   :: SignalNumber
  , signalAddress  :: Address
  , signalDeadline :: Integer
  , signalInit     :: FuncSym
  } deriving (Show, Eq)

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

type SignalName = String

type Address = Integer

type SignalNumber = Integer

--------------------------------------------------------------------------------
