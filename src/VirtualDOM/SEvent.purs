module VirtualDOM.SEvent (on, on', SEvent(..), SEventS, click, change, submit, magic, deserialize, deserialize', SHook(..), registerHook, RegisteredSHook(..), hook, input, mousedown, keydown, KeyboardEvent(..)) where

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Exception (EXCEPTION, throw, try)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import DOM.HTML.Types (HTMLElement)
import Data.Either (either, fromLeft, fromRight, isLeft)
import Data.Exists (Exists, runExists)
import Data.Foldable (find)
import Data.Foreign (F, readBoolean, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.List.NonEmpty (head)
import Data.Maybe (maybe)
import Data.String (Pattern(..), Replacement(..), drop, length, replace, take)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, show, unit, (#), ($), (*>), (<#>), (<$>), (<*>), (<<<), (<>), (==), (>>=), (>>>))
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (MainThreadProp, Prop, prop)
import WebWorker (WebWorker, OwnsWW)
import WebWorker.Channel (Channel(..), postMessageToWorkerC)

newtype SEvent e a = SEvent { event :: String
                            , id :: String
                            , handle :: Event -> Eff e (F a)}
type SEventS e = Exists (SEvent e)
newtype SHook = SHook { id :: String }
newtype RegisteredSHook e = RegisteredSHook { id :: String
                                            , hook :: HTMLElement -> (Eff e Unit)
                                            , unhook :: HTMLElement -> (Eff e Unit)
                                            }
registerHook :: forall e. SHook -> { hook :: HTMLElement -> Eff e Unit
                                   , unhook :: HTMLElement -> Eff e Unit} -> RegisteredSHook e
registerHook (SHook {id}) {hook, unhook} = RegisteredSHook {id, hook, unhook}

foreign import data Hook :: Type
foreign import makeHook :: forall obj e. { hook :: HTMLElement -> (Eff e Unit)
                                         , unhook :: HTMLElement -> (Eff e Unit) | obj}
                                         -> Hook

-- on makes a property (eg: onclick, onchange) based on the SEvent
-- the value of this property consists of three parts:
-- * "_vdom_as_json_": this is to signal to the vdom-as-json library that we want to do some custom logic on this prop/value at the UI thread side
-- * id of event: we'll use this to identify the event handler that we need to execute. These handlers need to be registred on the UI thread and passed to replaceOnMain since we can't transmit event handlers (= functions!) between threads
-- * JsonEncoded (partial) action: Here we require that the action constructor takes a record as only argument and this record has a val property with the same type as the value that the handler of the SEvent produces
on :: forall a e obj act.
      (act -> String) -- Json Encoding function, partially apply once in root of your app
      -> SEvent e a
      -> ({val :: a | obj} -> act)
      -> {val :: a | obj}
      -> Prop act
on encodeAction (SEvent {event, id}) constr rest =
  prop event ("_vdom_as_json_event_" <> id <> encodeAction (constr rest))

-- special case for actions that don't need any value returned from the UI thread
on' :: forall a e act.
      (act -> String) -- Json Encoding function, partially apply once in root of your app
       -> SEvent e a
       -> act
       -> Prop act
on' encodeAction (SEvent {event, id}) action =
  prop (event) ("_vdom_as_json_event_" <> id <> encodeAction action)

hook :: forall act. SHook -> Prop act
hook (SHook {id}) = prop ("purescript_vdom_worker_hook") ("_vdom_as_json_hook_" <> id)

-- The "magic" function just fills the "a" parameter into the "val" property of the action
-- (+ special case for actions without val property cf: on')
-- this function is (relatively) safe because on the worker thread the compiler ensured us through the signature of on/on'
foreign import magic :: forall a act. act -> a -> act

-- * deserializeEvent makes an event handler function on the UI thread based on the string we constructed in the "on" function
--   When everything's ok (the event is found and the action is decoded) it fills in the value into the action
--   and sends the action into the provided Channel
-- * The SEvents are wrapped in Exists so we can put them all in a single array and iterate over them
deserializeEvent :: forall act e.
                    (String -> F act) -- Json Decoding function
                    -> Array (SEventS (exception :: EXCEPTION | e)) -- registered events
                    -> (act -> Eff (exception :: EXCEPTION | e) Unit)
                    -> (String
                        -> Eff (exception :: EXCEPTION) (Event -> Eff (exception :: EXCEPTION | e) Unit))
deserializeEvent decodeAction events go =
  \str -> do
   -- ^ This string is the string made by on/on', without the "_vdom_as_json_event_" part
    let maybeSEventE = find (runExists (\(SEvent {id}) -> startsWith id str)) events
    sEventE <- maybe (throw ("No event handler found for " <> str <>". Add it to your deserialize invocation!")) pure maybeSEventE
    runExists (\(SEvent {id, handle}) -> do
      action <- either
                (head >>> show >>> (<>) ("Failed to parse action from " <> str <> ": ") >>> throw)
                pure
                (drop (length id) str # decodeAction # runExcept)
      pure \ev ->
        handle ev
          >>= runExcept
          >>> either
                (show >>> (<>) "Failed to extract value from event: " >>> throw)
                (magic action >>> go)) sEventE

deserializeHook :: forall e.
                   Array (RegisteredSHook (exception :: EXCEPTION | e))
                   -> (String -> Eff (exception :: EXCEPTION) Hook)
deserializeHook hooks =
  \str -> let foundSHook = find (\(RegisteredSHook {id}) -> startsWith id str) hooks
           in maybe (throw ("No hook found for " <> str))
                    (\(RegisteredSHook s) -> pure $ makeHook s)
                    foundSHook

deserialize :: forall act e.
               Array (Exists (SEvent (exception :: EXCEPTION, ownsww :: OwnsWW | e)))
               -> Array (RegisteredSHook (exception :: EXCEPTION, ownsww :: OwnsWW | e))
               -> WebWorker 
               -> Channel act
               -> (String -> MainThreadProp)
deserialize evs hooks ww chan@(Channel c) = deserialize' c.decode evs hooks (postMessageToWorkerC ww chan)

-- TODO refactor?
-- Lots of unsafeThrow / unsafePartial stuff here
deserialize' :: forall act e.
               (String -> F act) -- Json Decoding function
                -> Array (Exists (SEvent (exception :: EXCEPTION, ownsww :: OwnsWW | e)))
                -> Array (RegisteredSHook (exception :: EXCEPTION, ownsww :: OwnsWW | e))
                -> (act -> Eff ((exception :: EXCEPTION, ownsww :: OwnsWW | e)) Unit)
                -> (String -> MainThreadProp)
deserialize' decodeAction evs hooks dispatch = 
  \str -> let effMTP =
                if startsWith "event_" str
                  then (coerceEventHandler <<< mkEffFn1) <$> deserializeEvent decodeAction evs dispatch (replace (Pattern "event_") (Replacement "") str)
                  else coerceHook <$> deserializeHook hooks (replace (Pattern "hook_") (Replacement "") str)
           in runPure $ unsafePartial (try effMTP <#> (\a -> if isLeft a then unsafeThrow (show $ fromLeft a) else (fromRight a)))

coerceEventHandler :: forall e. (EffFn1 e Event Unit) -> MainThreadProp
coerceEventHandler = unsafeCoerce

coerceHook :: Hook -> MainThreadProp
coerceHook = unsafeCoerce

-- utility function
startsWith :: String -> String -> Boolean
startsWith short long = take (length short) long == short

-- Some example SEvents
click :: forall e. SEvent e Unit
click = SEvent { event: "onclick"
               , id: "__clickUnit"
               , handle: \_ -> pure (pure unit)}

mousedown :: forall e. SEvent e Unit
mousedown = SEvent { event: "onmousedown"
                   , id: "__mousedownUnit"
                   , handle: \_ -> pure (pure unit)}

change :: forall e. SEvent e String
change = SEvent { event: "onchange"
                , id: "__changeValue"
                , handle: \ev -> pure $ readProp "target" (toForeign ev) >>= readProp "value" >>= readString}

input :: forall e. SEvent e String
input = SEvent { event: "oninput"
               , id: "__changeInput"
               , handle: \ev -> pure $ readProp "target" (toForeign ev) >>= readProp "value" >>= readString}

keydown :: forall e. SEvent e KeyboardEvent
keydown = SEvent { event: "onkeydown"
                 , id: "__keyDownEvent"
                 , handle: \ev -> pure $ KeyboardEvent <$> 
                            ({key: _, ctrlKey: _, metaKey: _, altKey: _, shiftKey: _} <$>
                             (readProp "key" (toForeign ev) >>= readString) <*>
                             (readProp "ctrlKey" (toForeign ev) >>= readBoolean) <*>
                             (readProp "metaKey" (toForeign ev) >>= readBoolean) <*>
                             (readProp "altKey" (toForeign ev) >>= readBoolean) <*>
                             (readProp "shiftKey" (toForeign ev) >>= readBoolean))
                          }

submit :: forall e. SEvent (dom :: DOM | e) Unit
submit = SEvent { event: "onsubmit"
                , id: "__submitunit"
                , handle: \ev -> preventDefault ev *> pure (pure unit)}

data KeyboardEvent = KeyboardEvent { key :: String
                                   , ctrlKey :: Boolean
                                   , metaKey :: Boolean
                                   , altKey :: Boolean
                                   , shiftKey :: Boolean
                                   }
