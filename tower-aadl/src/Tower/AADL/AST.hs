--
-- AST for the fragment of AADL we generate.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.AST where

import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Tower.AST.Comment    as C

import           Tower.AADL.Priorities

----------------------------------------

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
    InputFeature  Input
  | OutputFeature Output
  | SignalFeature SignalInfo
  deriving (Show, Eq, Ord)

-- | Init Channel
data Init = Init
  { initCallback :: [SourcePath]
  , initOutput   :: [(Output, Bound)]
  } deriving (Show, Eq, Ord)

-- | Input channels
data Input = Input
  { inputId          :: !ChanId
  , inputLabel       :: !ChanLabel
  , inputType        :: !I.Type
  , inputCallback    :: [SourcePath]
  , inputQueue       :: Maybe Integer
  , inputSendsEvents :: SendsEvents
  } deriving (Show, Eq, Ord)

-- | Output channels
data Output = Output
  { outputId      :: !ChanId
  , outputLabel   :: !ChanLabel
  , outputType    :: !I.Type
  , outputEmitter :: FuncSym
  } deriving (Show, Eq, Ord)

-- | Path to a .c file and a function symbol in the file.  If the funtion symbol
-- is generated (i.e., in external threads), no filepath is given.
type SourcePath = (FilePath, FuncSym)
type SendsEvents = [(ChanLabel, Bound)]
data SourceTexts = SourceTexts [FilePath]
  deriving (Show, Eq)

data ThreadProperty =
    DispatchProtocol DispatchProtocol
  | ThreadType !ThreadType
  | ExecTime (Integer, Integer)
  -- ^ Min bound, max bound.
  | StackSize Integer
  | Priority Priority
  | EntryPoint [FuncSym]
  | SourceText [FilePath]
  -- ^ Path to a .c file
  | SendEvents SendsEvents
  | External
  | InitProperty FuncSym
  deriving (Show, Eq)

data DispatchProtocol =
    Periodic !Integer
  | Signal !SignalName !Address
  | Aperiodic
  | Sporadic
  deriving (Show, Eq)

data SignalInfo = SignalInfo
  { signalName        :: SignalName
  , signalNumber      :: SignalNumber
  , signalCallback    :: [SourcePath]
  , signalSendsEvents :: SendsEvents
  } deriving (Show, Eq, Ord)

data ThreadType =
    Passive
  | Active
  deriving (Show, Eq)

-- | An AADL variable.
type LocalId = String

-- | An AADL identifier.
type Name = String

-- Unique through the system.
type ChanId = Integer

type ChanLabel = String

-- | Channel bound.
type Bound = Integer

-- | Function symbol.
type FuncSym = String

type SignalName = String

type Address = Integer

type SignalNumber = Int
