module VirtualDOM.SEvent (on, on', mkUIHandlers, SEvent(..), SEventS, click, change, submit, magic, mkUIHandlers') where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (either, Either)
import Data.Exists (Exists, runExists)
import Data.Foldable (find)
import Data.Foreign (F, readString, toForeign)
import Data.Foreign.Class (readProp)
import Data.Maybe (maybe)
import Data.Monoid (class Monoid)
import Data.String (length, drop, take)
import Prelude ((>>>), const, (<<<), (*>), show, Unit, pure, unit, (>>=), ($), (==), (#), (<>))
import VirtualDOM (prop, Prop)
import WebWorker (WebWorker, OwnsWW)
import WebWorker.Channel (Channel, postMessageToWorkerC)

newtype SEvent e a = SEvent { event :: String
                            , id :: String
                            , handle :: Event -> Eff e (F a)}
type SEventS e = Exists (SEvent e)

-- on makes a property (eg: onclick, onchange) based on the SEvent
-- the value of this property consists of three parts:
-- * "_vdom_as_json_": this is to signal to the vdom-as-json library that we want to do some custom logic on this prop/value at the UI thread side
-- * id of event: we'll use this to identify the event handler that we need to execute. These handlers need to be registred on the UI thread and passed to mkUIHandlers since we can't transmit event handlers (= functions!) between threads
-- * JsonEncoded (partial) action: Here we require that the action constructor takes a record as only argument and this record has a val property with the same type as the value that the handler of the SEvent produces
on :: forall a e obj act.
      (Monoid a, EncodeJson act) =>
      SEvent e a
      -> ({val :: a | obj} -> act)
      -> {val :: a | obj}
      -> Prop act
on (SEvent {event, id}) constr rest =
  prop event ("_vdom_as_json_" <> id <> printJson (encodeJson (constr rest)))

-- special case for actions that don't need any value returned from the UI thread
on' :: forall a e act. (EncodeJson act) => SEvent e a -> act -> Prop act
on' (SEvent {event, id}) action =
  prop (event) ("_vdom_as_json_" <> id <> printJson (encodeJson action))

-- The "magic" function just fills the "a" parameter into the "val" property of the action
-- (+ special case for actions without val property cf: on')
-- this function is (relatively) safe because on the worker thread the compiler ensured us through the signature of on/on'
foreign import magic :: forall a act. act -> a -> act

-- * mkUIHandlers makes an event handler function on the UI thread based on the string we constructed in the "on" function
--   When everything's ok (the event is found and the action is decoded) it fills in the value into the action
--   and sends the action into the provided Channel
-- * The SEvents are wrapped in Exists so we can put them all in a single array and iterate over them
mkUIHandlers' :: forall act e. (EncodeJson act, DecodeJson act) =>
                Array (SEventS (err :: EXCEPTION | e))
                -> (act -> Eff (err :: EXCEPTION | e) Unit)
                -> (String
                    -> (Event -> Eff (err :: EXCEPTION | e) Unit))
mkUIHandlers' events go =
  \str ->
   -- ^ This string is the string made by on/on', without the "_vdom_as_json_" part
    let foundSEvent = find (runExists (\(SEvent {id}) -> startsWith id str)) events
                      -- ^ Trying to find the SEvent by id
     in maybe (const (throw ("No event handler found for " <> str)))
              (runExists (\(SEvent {id, handle}) ->
                  either
                    (\s _ -> throw ("Failed to parse action from " <> str <> ": " <> s))
                    (\action ev -> handle ev >>= runExcept >>> either
                                                  (throw <<< (<>) "Failed to extract value from event: " <<< show)
                                                  (go <<< magic action))
                                                  -- ^ everything ok, fill in val and send back
                    ((drop (length id) str # jsonParser >>= decodeJson) :: Either String act)))
                    -- ^ Parsing the action
              foundSEvent

mkUIHandlers :: forall act e. (EncodeJson act, DecodeJson act) =>
                Array (Exists (SEvent (err :: EXCEPTION, ownsww :: OwnsWW | e))) ->
                WebWorker -> Channel act ->
                (String -> (Event -> Eff (err :: EXCEPTION, ownsww :: OwnsWW | e) Unit))
mkUIHandlers evs ww chan = mkUIHandlers' evs (postMessageToWorkerC ww chan)

-- utility function
startsWith :: String -> String -> Boolean
startsWith short long = take (length short) long == short

-- Some example SEvents
click :: forall e. SEvent e Unit
click = SEvent { event: "onclick"
               , id: "__clickUnit"
               , handle: \_ -> pure (pure unit)}

change :: forall e. SEvent e String
change = SEvent { event: "onchange"
                , id: "__changeValue"
                , handle: \ev -> pure $ readProp "target" (toForeign ev) >>= readProp "value" >>= readString}

submit :: forall e. SEvent (dom :: DOM | e) Unit
submit = SEvent { event: "onsubmit"
                , id: "__submitunit"
                , handle: \ev -> preventDefault ev *> pure (pure unit)}
