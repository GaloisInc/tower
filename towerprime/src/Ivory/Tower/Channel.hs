{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Channel
  ( channel
  ) where


import Ivory.Language

import qualified Ivory.Tower.AST            as AST
import qualified Ivory.Tower.Types.OS       as OS
import           Ivory.Tower.Types.Channels
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Tower

channel :: forall area
         . (IvoryArea area, IvoryZero area)
        => Tower (ChannelSource area, ChannelSink area)
channel = do
  cid <- fresh
  os <- getOS
  let chan = AST.Chan { AST.chan_id = cid
                      , AST.chan_size = -1 -- XXX
                      , AST.chan_ityp = undefined  -- XXX
                      }
      code astsys = OS.gen_channel os astsys chan (Proxy :: Proxy area)
  putChan               chan
  putSysCommInitializer ((call_ . fst) `fmap` code)
  putSysModdef          (snd `fmap` code)
  return (ChannelSource chan, ChannelSink chan)

