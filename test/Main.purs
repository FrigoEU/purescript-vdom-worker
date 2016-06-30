module Test.Main where

import Control.Monad.Aff (later')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut.Decode (decodeJson, class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode (gEncodeJson, class EncodeJson)
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (readProp)
import Data.Generic (class Generic)
import Data.StrMap (empty)
import Prelude (Unit, show, ($), (+), return, bind, (==), pure, (>>>))
import Test.Unit (TIMER, runTest, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import VirtualDOM (VTree, DOM, SerializedVPatches, Node, div, vtext, props, applyPatch, appendToBody, createElement)
import VirtualDOM.Worker (WEventMessage, WEvent(WEvent), on, mkDeserializeHandlersForWEvents, registerWEventHandler)
import WebWorker (OwnsWW, mkWorker, terminateWorker)
import WebWorker.Channel (onmessageFromWorkerC, registerChannel, Channel(Channel))

main :: forall eff. Eff ( dom :: DOM , ownsww :: OwnsWW , exception :: EXCEPTION, console :: CONSOLE, timer :: TIMER, avar :: AVAR, testOutput :: TESTOUTPUT | eff ) Unit
main = runTest do
  testWithWorker
  {-- testWithoutWorker --}

testWithWorker =
  test "make worker, click button, check that text = screenX + screenY" do
    let initial = div (props []) []
    node <- liftEff $ createElement initial
    liftEff $ appendToBody node
    ww <- liftEff $ mkWorker "testworker.js"
    let allWEvents = registerWEventHandler empty clickXY (extractXY >>> pure)
    let mdh = mkDeserializeHandlersForWEvents ww wEventsChannel allWEvents
    let chs = registerChannel empty patchesChannel (\serPs -> applyPatch node serPs mdh)
    liftEff $ onmessageFromWorkerC ww chs
    later' 500 do 
      inner <- liftEff $ innerHTML node 
      assert "first rendered with 5" (inner == "5")
      liftEff $ sendClick node 100 23
      later' 50 do
        innr <- liftEff $ innerHTML node 
        assert "second rendered with 123" (innr == "123")
        liftEff (terminateWorker ww)

{-- testWithoutWorker = --}
{--   test "without worker" do --}
{--     let initial = div (props []) [] --}
{--     node <- liftEff $ createElement initial --}
{--     stateRef <- liftEff $ newRef 0 --}
{--     treeRef <- liftEff $ newRef initial --}
{--     liftEff (appendToBody node) --}
{--     {1-- let localDOMHandlers = makeLocalDOMHandlersForWEvents allWEvents --1} --}
{--     let dispatch action = do --}
{--           s <- readRef stateRef --} 
{--           let newS = update action s --}
{--           oldTree <- readRef treeRef --}
{--           let newTree = testComponent dispatch newS --}
{--           let patches = diff oldTree newTree --}
{--           let serializedPatches = serializePatch localFunctionSerializer patches --}
{--           writeRef stateRef newS --}
{--           writeRef treeRef newTree --}
{--           applyPatch node serializedPatches ?localDOMHandlers --}
{--     liftEff (dispatch $ Display 5) --}
{--     later' 500 do --}
{--       inner <- liftEff $ innerHTML node --}
{--       assert "first rendered with 5" (inner == "5") --}
{--       liftEff $ sendClick node 100 23 --}
{--       later' 50 do --}
{--         innr <- liftEff $ innerHTML node --} 
{--         assert "second rendered with 123" (innr == "123") --}

foreign import innerHTML :: forall eff. Node -> Eff (dom :: DOM | eff) String
foreign import sendClick :: forall eff. Node -> Int -> Int -> Eff (dom :: DOM | eff) Unit

newtype ScreenXY = ScreenXY {x :: Int, y :: Int}
derive instance genericScreenXY :: Generic ScreenXY
instance encodeJsonScreenXY :: EncodeJson ScreenXY where encodeJson = gEncodeJson 
instance decodeJsonScreenXY :: DecodeJson ScreenXY where decodeJson = gDecodeJson

extractXY :: Foreign -> F ScreenXY
extractXY obj = do
  x <- readProp "screenX" obj
  y <- readProp "screenY" obj
  return $ ScreenXY {x, y}

clickXY :: WEvent ScreenXY
clickXY = WEvent {event: "click", tag: "clickXY", decodeJson: decodeJson}


patchesChannel :: Channel SerializedVPatches
patchesChannel = Channel "serializedvpatchesyo"

wEventsChannel :: Channel WEventMessage
wEventsChannel = Channel "weventsmessagesfam"

data Action = Display Int
type State = Int
type Dispatch = Action -> Eff AppEffects Unit
type AppEffects = (ref :: REF, dom :: DOM)

testComponent :: Dispatch -> Int -> VTree
testComponent dispatch n = div ps children
  where 
    ps = props [on clickXY (\(ScreenXY {x, y}) -> dispatch (Display $ x + y))]
    children = [vtext $ show n]

update :: Action -> State -> State
update (Display n) _ = n
