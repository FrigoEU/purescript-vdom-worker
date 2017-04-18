module Test.Integration where

import Control.Monad.Aff (delay)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.Node.Element (getAttribute, setAttribute)
import DOM.Node.Types (Node)
import DOM.HTML.Types (htmlElementToElement)
import Data.Exists (mkExists)
import Data.Function.Uncurried (mkFn1)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.StrMap (empty)
import Prelude (Unit, bind, ($), (<#>), (==), discard, const, pure, unit)
import Test.Unit (test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Worker (actionsChannel, addRedClass, clickXY, patchesChannel)
import VirtualDOM (applyPatch, appendToBody, createElement)
import VirtualDOM.HTML (div)
import VirtualDOM.SEvent (deserialize, registerHook)
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
    let allHooks = [registerHook
                      addRedClass
                      { hook: mkFn1 (\el -> setAttribute "class" "red" (htmlElementToElement el))
                      , unhook: mkFn1 (const (pure unit))
                      }]
    let mdh = deserialize allEvents allHooks ww actionsChannel
    let chs = registerChannel empty patchesChannel (\sps -> applyPatch node sps mdh)
    liftEff $ onmessageFromWorkerC ww chs

    delay (Milliseconds 500.0)
    inner <- liftEff $ innerHTML node
    class' <-liftEff $ getClass node <#> toMaybe
    assert "first rendered with 5" (inner == "5")
    assert "hook set class to red" (class' == Just "red")
    liftEff $ sendClick node 100 23

    delay (Milliseconds 50.0)
    innr <- liftEff $ innerHTML node
    assert "second rendered with 123" (innr == "123")
    liftEff (terminateWorker ww)

foreign import innerHTML :: forall eff. Node -> Eff (dom :: DOM | eff) String
foreign import getClass :: forall eff. Node -> Eff (dom :: DOM | eff) (Nullable String)
foreign import sendClick :: forall eff. Node -> Int -> Int -> Eff (dom :: DOM | eff) Unit
