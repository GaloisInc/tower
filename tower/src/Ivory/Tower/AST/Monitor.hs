{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Tower.AST.Monitor where

import Data.Monoid ((<>))
#if MIN_VERSION_mainland_pretty(0,6,0)
import           Text.PrettyPrint.Mainland.Class
#endif
import Text.PrettyPrint.Mainland

import Ivory.Tower.Types.Unique
import Ivory.Tower.AST.Handler

data Monitor = Monitor
  { monitor_name     :: Unique
  , monitor_handlers :: [Handler]
  , monitor_external  :: MonitorExternal
  } deriving (Eq, Show, Ord)

monitorName :: Monitor -> String
monitorName = showUnique . monitor_name

data MonitorExternal =
    MonitorDefined
  | MonitorExternal
  deriving (Show, Read, Eq, Ord)

instance Pretty Monitor where
  ppr m@(Monitor{..}) = hang 2 $
        text (monitorName m) <+> parens (ppr monitor_external) <> colon
    </> hang 2 ("Handlers:" </> stack (map ppr monitor_handlers))

instance Pretty MonitorExternal where
  ppr MonitorDefined = "defined"
  ppr MonitorExternal = "external"
