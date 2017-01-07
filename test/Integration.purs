module Test.Integration where

import Control.Monad.Aff (later')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.Node.Types (Node)
import Data.Exists (mkExists)
import Data.StrMap (empty)
import Prelude (Unit, bind, (==), ($))
import Test.Unit (test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Worker (patchesChannel, actionsChannel, clickXY)
import VirtualDOM (applyPatch, appendToBody, createElement)
import VirtualDOM.HTML (div)
import VirtualDOM.SEvent (mkUIHandlers)
import WebWorker (OwnsWW, mkWorker, terminateWorker)
import WebWorker.Channel (onmessageFromWorkerC, registerChannel)

main :: forall eff. Eff ( dom :: DOM , ownsww :: OwnsWW , err :: EXCEPTION, console :: CONSOLE, timer :: TIMER, avar :: AVAR, testOutput :: TESTOUTPUT | eff ) Unit
main = runTest do
  testWithWorker
  {-- testWithoutWorker --}

testWithWorker =
  test "make worker, click button, check that text = screenX + screenY" do
    let initial = div [] []
    node <- liftEff $ createElement initial
    liftEff $ appendToBody node
    ww <- liftEff $ mkWorker "testworker.js"
    let allEvents = [mkExists clickXY]
    let mdh = mkUIHandlers allEvents ww actionsChannel
    let chs = registerChannel empty patchesChannel (\sps -> applyPatch node sps mdh)
    liftEff $ onmessageFromWorkerC ww chs
    later' 500 do
      inner <- liftEff $ innerHTML node
      assert "first rendered with 5" (inner == "5")
      liftEff $ sendClick node 100 23
      later' 50 do
        innr <- liftEff $ innerHTML node
        assert "second rendered with 123" (innr == "123")
        liftEff (terminateWorker ww)

foreign import innerHTML :: forall eff. Node -> Eff (dom :: DOM | eff) String
foreign import sendClick :: forall eff. Node -> Int -> Int -> Eff (dom :: DOM | eff) Unit