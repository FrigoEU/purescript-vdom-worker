module Example.Worker where

import VirtualDOM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, writeRef, newRef, readRef)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Example.Component (renderExampleComponent)
import Example.Model (State, Action(Add))
import Prelude (unit, const, return, Unit, (+), bind, ($))
import VirtualDOM.Worker (mkWorkerFunctionsForWEvents)
import WebWorker (MessageEvent(MessageEvent), IsWW, onmessage, postMessage)

main :: Eff (ref :: REF, isww :: IsWW) Unit
main = do
  {functionSerializer, handler} <- mkWorkerFunctionsForWEvents
  let initial = div (props []) []
  stateRef <- newRef 0
  treeRef <- newRef initial
  let dispatch action = do
        s <- readRef stateRef 
        let newS = update action s
        oldTree <- readRef treeRef
        let newTree = renderExampleComponent dispatch newS
        let patches = diff oldTree newTree
        let serializedPatches = serializePatch functionSerializer patches
        writeRef stateRef newS
        writeRef treeRef newTree
        postMessage (toForeign $ toString serializedPatches)
  onmessage (\(MessageEvent {data: d}) -> either (const $ return unit)
                                          handler
                                          (read d))
  dispatch (Add 0)


update :: Action -> State -> State
update (Add n) s = n + s
