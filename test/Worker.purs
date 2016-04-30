module Test.Worker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Data.StrMap (empty)
import Prelude (Unit, show, ($), (+), bind)
import Test.Main (wEventsChannel, patchesChannel, ScreenXY(ScreenXY), screenXY)
import VirtualDOM (serializePatch, diff, VTree, vtext, div, props)
import VirtualDOM.Worker (mkWorkerFunctionsForWEvents, on)
import WebWorker (IsWW, onmessageC, registerChannel, postMessageC)

data Action = Display Int
type State = Int
type AppEffects = (ref :: REF, isww :: IsWW, console :: CONSOLE)
type Dispatch = Action -> Eff AppEffects Unit

main :: Eff AppEffects Unit
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
        postMessageC patchesChannel serializedPatches
  let chs = registerChannel empty wEventsChannel handler
  onmessageC chs
  dispatch $ Display 5

testComponent :: Dispatch -> Int -> VTree
testComponent dispatch n = div ps children
  where 
    ps = props [on screenXY (\(ScreenXY {x, y}) -> dispatch (Display $ x + y))]
    children = [vtext $ show n]

update :: Action -> State -> State
update (Display n) _ = n
