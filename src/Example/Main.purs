module Example.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (either)
import Data.Foreign.Class (read)
import Example.WEvents (allWEvents)
import Prelude (Unit, const, bind)
import VirtualDOM (DOM, applyPatch, fromString, appendToBody, createElement, props, div)
import VirtualDOM.Worker (makeDOMHandlersForWEvents)
import WebWorker (MessageEvent(MessageEvent), OwnsWW, mkWorker, onmessageFromWorker)

main :: forall eff. Eff ( dom :: DOM , ownsww :: OwnsWW , exception :: EXCEPTION | eff ) Unit
main = do 
  let initial = div (props []) []
  let node = createElement initial
  appendToBody node
  worker <- mkWorker "worker.js"
  let mdh = makeDOMHandlersForWEvents worker allWEvents
  let handleMessageFromWorker = 
      \(MessageEvent {data: d}) -> let serPs = either (const []) fromString (read d)
                                    in applyPatch node serPs mdh
  onmessageFromWorker worker handleMessageFromWorker
