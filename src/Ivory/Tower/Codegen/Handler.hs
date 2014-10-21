
module Ivory.Tower.Codegen.Handler
  ( emptyHandlerThreadCode
  , generateHandlerThreadCode
  ) where

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.ThreadCode

import Ivory.Tower.ToyObjLang

emptyHandlerThreadCode :: AST.Handler -> AST.Tower
                       -> [(AST.Thread, HandlerCode)]
emptyHandlerThreadCode handlerast towerast =
  [ (t, emptyHandlerCode)
  | t <- AST.handlerThreads towerast handlerast
  ]

generateHandlerThreadCode :: (AST.Tower -> [(AST.Thread, HandlerCode)])
                    -> AST.Tower -> [ThreadCode]
generateHandlerThreadCode thcs twr =
  [ handlerCodeToThreadCode t hc
  | (t, hc) <- thcs twr
  ]

userHandlerCode :: HandlerCode -> ModuleM ()
userHandlerCode hc = handlercode_callbacks hc >>
  foldl appenduser (return ()) (handlercode_emitters hc)
  where
  appenduser acc ec = acc >> emittercode_user ec

generatedHandlerCode :: HandlerCode -> ModuleM ()
generatedHandlerCode hc =
  foldl appendgen (return ()) (handlercode_emitters hc)
  -- XXX MAKE RUNNER FUNCTION
  where
  appendgen acc ec = acc >> emittercode_gen ec

handlerCodeToThreadCode :: AST.Thread -> HandlerCode -> ThreadCode
handlerCodeToThreadCode t hc = insertUserThreadCode (userHandlerCode hc)
                             $ insertGenThreadCode (generatedHandlerCode hc)
                             $ emptyThreadCode t

