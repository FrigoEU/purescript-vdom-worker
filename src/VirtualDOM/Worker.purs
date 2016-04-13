module VirtualDOM.Worker (
  WEvent, 
  on 
  ) where

import Control.Monad.Eff (Eff)
import Data.Array (index)
import Data.Maybe (maybe)
import Data.String (split)
import Data.StrMap (StrMap, insert, lookup)
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, (<>), ($))
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (DOM, PropSerializer)

-- a : can be anything, just has to be serializable
data WEvent a = WEvent { event :: String 
                       , tag :: String }

on :: forall eff a. WEvent a -> (a -> Eff eff Unit) -> Tuple String Prop
on (WEvent {event, tag}) handler = Tuple (event <> "@" <> tag) $ unsafeCoerce handler

serializeWEvent :: PropSerializer
serializeWEvent str = 
  let splitted = split "@" str
   in maybe {prop: str, val: str}
            (\prop -> maybe {prop: str, val: str} (\val ->{prop: prop, val: val}) (index splitted 1))
            (index splitted 0)

type WEvents = StrMap (Foreign -> Eff (dom :: DOM) Json)

registerWEventHandler :: forall eff a. (JsonEncode a) =>
                         WEvents 
                         -> WEvent a 
                         -> (Foreign -> Eff (dom :: DOM) a)
                         -> WEvents
registerWEventHandler wes (WEvent {tag}) f = insert tag (encodeJson <<< f) wes

mkReplaceWithEventHandler :: WEvents -> ReplaceWithEventHandler
mkReplaceWithEventHandler = lookup

foreign import data Prop :: * -- TODO

