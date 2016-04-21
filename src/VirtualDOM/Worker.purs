module VirtualDOM.Worker (
  WEvent, 
  on 
  ) where

import WebWorker
import Control.Bind (join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Array (index)
import Data.Foreign (toForeign, Foreign)
import Data.Maybe (maybe)
import Data.StrMap (StrMap, insert, lookup)
import Data.String (split)
import Data.String.Regex (Regex, match)
import Data.Tuple (Tuple(Tuple))
import Prelude (map, return, id, Unit, (<>), ($), (<<<), bind, (<$>), (>>=))
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (FunctionSerializer, MakeDOMHandlers, DOM)

foreign import data Prop :: * -- TODO
foreign import data MUT :: !
foreign import crossAndAtRegex :: Regex

-- a : can be anything, just has to have a JsonEncode instance
data WEvent a = WEvent { event :: String 
                       , tag :: String }

type WEventHandlers = StrMap (Foreign -> Eff (dom :: DOM) Json)

on :: forall eff a. WEvent a -> (a -> Eff eff Unit) -> Tuple String Prop
on (WEvent {event, tag}) handler = Tuple (event <> "@" <> tag) $ unsafeCoerce handler

registerWEventHandler :: forall eff a. (EncodeJson a, DecodeJson a) =>
                         WEventHandlers 
                         -> WEvent a 
                         -> (Foreign -> Eff (dom :: DOM) a)
                         -> WEventHandlers
registerWEventHandler wes (WEvent {tag}) f = insert tag ((map encodeJson <<< f)) wes

makeDOMHandlersForWEvents :: WebWorker -> WEventHandlers -> MakeDOMHandlers
makeDOMHandlersForWEvents ww weh fullstr = 
  maybe 
    (unsafeThrow $ "Couldn't find event handler for " <> fullstr) -- Wow, how suck, much crappy
    id
    (do matches <- match crossAndAtRegex fullstr
        mnr <- index matches 0
        nr <- mnr
        ms <- index matches 1
        s <- ms
        evh <- lookup s weh
        return ((postMessageToWW ww) <<< toForeign <<< {id: nr, data: _} <<< evh))

foreign import mkWorkerFunctionsForWEvents :: forall eff. 
                                Eff (mut :: MUT | eff) { functionSerializer :: FunctionSerializer, 
                                                         handler :: String -> Eff eff Unit}

