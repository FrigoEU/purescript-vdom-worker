module Test.Worker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, writeRef, readRef, newRef)
import DOM (DOM)
import DOM.HTML.Types (htmlElementToElement)
import DOM.Node.Element (setAttribute)
import Data.Argonaut.Decode (class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, gEncodeJson)
import Data.Foreign (toForeign, F, Foreign)
import Data.Foreign.Class (readProp)
import Data.Function.Uncurried (mkFn1)
import Data.Generic (class Generic)
import Data.Monoid (class Monoid, mempty)
import Data.StrMap (empty)
import Prelude (class Semigroup, Unit, bind, const, pure, show, unit, ($), (+), (<<<))
import VirtualDOM (SerializedVPatches, VTree, vtext, serializePatch, diff)
import VirtualDOM.HTML (div)
import VirtualDOM.SEvent (SEvent(SEvent), SHook(..), hook, on)
import WebWorker (IsWW)
import WebWorker.Channel (Channel(Channel), onmessageC, registerChannel, postMessageC)

data Action = Clicked {val :: ScreenXY}
derive instance genericAction :: Generic Action
instance encodeJsonAction :: EncodeJson Action where encodeJson = gEncodeJson
instance decodeJsonAction :: DecodeJson Action where decodeJson = gDecodeJson

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
  dispatch (Clicked {val: ScreenXY {x: 2, y: 3}})

testComponent :: Int -> VTree Action
testComponent n = div ps children
  where
    ps = [on clickXY Clicked {val: mempty}, hook addRedClass]
    children = [vtext $ show n]

update :: Action -> State -> State
update (Clicked {val: ScreenXY {x, y}}) _ = x + y

newtype ScreenXY = ScreenXY {x :: Int, y :: Int}
derive instance genericScreenXY :: Generic ScreenXY
instance semigroupScreenXY :: Semigroup ScreenXY where
  append (ScreenXY {x: x1, y: y1}) (ScreenXY {x: x2, y: y2}) =
    ScreenXY {x: x1 + x2, y: y1 + y2}
instance monoidScreenXY :: Monoid ScreenXY where mempty = ScreenXY {x: 0, y: 0}
instance encodeJsonScreenXY :: EncodeJson ScreenXY where encodeJson = gEncodeJson
instance decodeJsonScreenXY :: DecodeJson ScreenXY where decodeJson = gDecodeJson

extractXY :: Foreign -> F ScreenXY
extractXY obj = do
  x <- readProp "screenX" obj
  y <- readProp "screenY" obj
  pure $ ScreenXY {x, y}

clickXY :: forall e. SEvent e ScreenXY
clickXY = SEvent { event: "onclick"
                 , id: "clickXY"
                 , handle: pure <<< extractXY <<< toForeign}

patchesChannel :: Channel (SerializedVPatches Action)
patchesChannel = Channel "serializedvpatchesyo"

actionsChannel :: Channel Action
actionsChannel = Channel "weventsmessagesfam"

addRedClass :: SHook
addRedClass = SHook { id: "addRedClassHook" }
