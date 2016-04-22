module Example.Worker where

import VirtualDOM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, writeRef, newRef, readRef)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp)
import Example.Component (renderExampleComponent)
import Example.Framework (Action(Add))
import Prelude (unit, const, return, Unit, (+), bind, ($))
import VirtualDOM.Worker (mkWorkerFunctionsForWEvents)
import WebWorker (IsWW, onmessage, postMessage)

main :: Eff (ref :: REF, isww :: IsWW) Unit
main = do
  let initial = div (props []) []
  stateRef <- newRef 0
  treeRef <- newRef initial
  {functionSerializer, handler} <- mkWorkerFunctionsForWEvents
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
  onmessage (\fn -> either (const $ return unit)
                           handler
                           (readProp "data" fn)) -- TODO
  dispatch (Add 0)

type State = Int

update :: Action -> State -> State
update (Add n) s = n + s
