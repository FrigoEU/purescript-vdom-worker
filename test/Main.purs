module Test.Main where

import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut.Decode (decodeJson, class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode (gEncodeJson, class EncodeJson)
import Data.Either (either)
import Data.Foreign.Class (read, class IsForeign, readProp)
import Data.Generic (class Generic)
import Data.StrMap (empty)
import Prelude (Unit, id, return, const, bind, ($), (==))
import Test.Unit (test, runTest)
import Test.Unit.Assert (assert)
import VirtualDOM (DOM, Node, applyPatch, fromString, appendToBody, createElement, props, div)
import VirtualDOM.Worker (WEventHandlers, registerWEventHandler, makeDOMHandlersForWEvents, WEvent(WEvent))
import WebWorker (onmessageFromWorker, MessageEvent(MessageEvent), mkWorker)

main = runTest do
  test "make worker, click button, check that text = screenX + screenY" do
    let initial = div (props []) []
    let node = createElement initial
    liftEff $ appendToBody node
    ww <- liftEff $ mkWorker "testworker.js"
    let mdh = makeDOMHandlersForWEvents ww allWEvents
    let handleMessageFromWorker = 
        \(MessageEvent {data: d}) -> let serPs = either (const []) fromString (read d)
                                      in applyPatch node serPs mdh
    liftEff $ onmessageFromWorker ww handleMessageFromWorker
    later' 500 $ do 
      inner <- liftEff $ innerHTML node 
      assert "first rendered with 5" (inner == "5")
      liftEff $ sendClick node 100 23
      later' 50 $ do
        inner <- liftEff $ innerHTML node 
        assert "second rendered with 123" (inner == "123")


foreign import innerHTML :: forall eff. Node -> Eff (dom :: DOM | eff) String
foreign import sendClick :: forall eff. Node -> Int -> Int -> Eff (dom :: DOM | eff) Unit

newtype ScreenXY = ScreenXY {x :: Int, y :: Int}
instance isForeignScreenXY :: IsForeign ScreenXY where
  read obj = do
    x <- readProp "screenX" obj
    y <- readProp "screenY" obj
    return $ ScreenXY {x, y}

derive instance genericScreenXY :: Generic ScreenXY

instance encodeJsonScreenXY :: EncodeJson ScreenXY where
  encodeJson = gEncodeJson

instance decodeJsonScreenXY :: DecodeJson ScreenXY where
  decodeJson = gDecodeJson

screenXY :: WEvent ScreenXY
screenXY = WEvent {event: "click", tag: "screenxy", decodeJson: decodeJson}

allWEvents :: WEventHandlers
allWEvents = let reader fn = return $ either (\_ -> ScreenXY {x: 0, y: 0}) id $ read fn
              in registerWEventHandler empty screenXY reader
