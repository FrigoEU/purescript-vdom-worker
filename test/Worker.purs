module Test.Worker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import Control.Monad.Except (except)
import Data.Foreign (toForeign, F, Foreign, readInt)
import Data.Foreign.Index (readProp)
import Data.StrMap (empty)
import Prelude (class Semigroup, Unit, bind, const, pure, show, unit, ($), (+), (<<<), discard, (>=>), (>>>))
import Simple.JSON (readJSON, writeJSON)
import VirtualDOM (SerializedVPatches, VTree, decodePatches, diff, encodePatches, serializePatch, vtext)
import VirtualDOM.HTML (div)
import VirtualDOM.SEvent (SEvent(SEvent), SHook(..), hook, on)
import WebWorker (IsWW)
import WebWorker.Channel (Channel(Channel), onmessageC, registerChannel, postMessageC)

data Action = Clicked {val :: ScreenXY}
encodeAction :: Action -> String
encodeAction (Clicked obj) =
  writeJSON { tag: "Clicked" , contents: obj}
decodeAction :: String -> F Action
decodeAction str = except $ do
  obj :: {tag :: String, contents :: {val :: ScreenXY}} <- readJSON str
  pure (Clicked obj.contents)

type State = Int
type AppEffects = (ref :: REF, isww :: IsWW, console :: CONSOLE)
type Dispatch = Action -> Eff AppEffects Unit

main :: Eff AppEffects Unit
main = do
  let initial = div [] []
  stateRef <- newRef 0
  treeRef <- newRef initial
  let dispatch action = do
        s <- readRef stateRef
        let newS = update action s
        oldTree <- readRef treeRef
        let newTree = testComponent newS
        let patches = diff oldTree newTree
        let serializedPatches = serializePatch patches
        writeRef stateRef newS
        writeRef treeRef newTree
        postMessageC patchesChannel serializedPatches
  onmessageC (registerChannel empty actionsChannel dispatch)
  dispatch (Clicked {val: {x: 2, y: 3}})

testComponent :: Int -> VTree Action
testComponent n = div ps children
  where
    ps = [on encodeAction clickXY Clicked {val: {x: 0, y: 0}}, hook addRedClass]
    children = [vtext $ show n]

update :: Action -> State -> State
update (Clicked {val: {x, y}}) _ = x + y

type ScreenXY = {x :: Int, y :: Int}

extractXY :: Foreign -> F ScreenXY
extractXY obj = do
  x <- (readProp "screenX" >=> readInt) obj
  y <- (readProp "screenY" >=> readInt) obj
  pure {x, y}

clickXY :: forall e. SEvent e ScreenXY
clickXY = SEvent { event: "onclick"
                 , id: "clickXY"
                 , handle: pure <<< extractXY <<< toForeign}

patchesChannel :: Channel (SerializedVPatches Action)
patchesChannel = Channel
  { name: "serializedvpatchesyo"
  , encode: encodePatches
  , decode: decodePatches
  } 

actionsChannel :: Channel Action
actionsChannel = Channel
  { name: "weventsmessagesfam"
  , encode: encodeAction
  , decode: decodeAction
  }

addRedClass :: SHook
addRedClass = SHook { id: "addRedClassHook" }
