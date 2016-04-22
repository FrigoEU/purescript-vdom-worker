module Example.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (either, Either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readProp)
import Example.WEvents (allWEvents)
import Prelude (Unit, const, bind)
import VirtualDOM (DOM, applyPatch, fromString, appendToBody, createElement, props, div)
import VirtualDOM.Worker (makeDOMHandlersForWEvents)
import WebWorker (OwnsWW, mkWorker, onmessageFromWorker)

main :: forall eff. Eff ( dom :: DOM , ownsww :: OwnsWW , exception :: EXCEPTION | eff ) Unit
main = do 
  let initial = div (props []) []
  let node = createElement initial
  appendToBody node
  worker <- mkWorker "worker.js"
  let mdh = makeDOMHandlersForWEvents worker allWEvents
  let handleMessageFromWorker = 
      (\fn -> let serializedPatches = (either (const []) 
                                              fromString 
                                              (readProp "data" fn :: Either ForeignError String))
               in applyPatch node serializedPatches mdh)
  onmessageFromWorker worker handleMessageFromWorker



