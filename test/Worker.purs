module Test.Worker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Prelude (Unit, show, unit, return, const, ($), bind, (+))
import Test.Main (ScreenXY(ScreenXY), screenXY)
import VirtualDOM (serializePatch, diff, VTree, vtext, toString, div, props)
import VirtualDOM.Worker (mkWorkerFunctionsForWEvents, on)
import WebWorker (IsWW, MessageEvent(MessageEvent), postMessage, onmessage)

data Action = Display Int
type State = Int
type AppEffects = (ref :: REF, isww :: IsWW)
type Dispatch = Action -> Eff AppEffects Unit

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
        let newTree = testComponent dispatch newS
        let patches = diff oldTree newTree
        let serializedPatches = serializePatch functionSerializer patches
        writeRef stateRef newS
        writeRef treeRef newTree
        postMessage (toForeign $ toString serializedPatches)
  onmessage (\(MessageEvent {data: d}) -> either (const $ return unit)
                                          handler
                                          (read d))
  dispatch $ Display 5

testComponent :: Dispatch -> Int -> VTree
testComponent dispatch n = div ps children
  where 
    ps = props [on screenXY (\(ScreenXY {x, y}) -> dispatch (Display $ x + y))]
    children = [vtext $ show n]

update :: Action -> State -> State
update (Display n) _ = n
