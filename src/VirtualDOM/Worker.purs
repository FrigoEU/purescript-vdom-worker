module VirtualDOM.Worker where

import WebWorker
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Array (index)
import Data.Foreign (toForeign, Foreign)
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.StrMap (StrMap, insert, lookup)
import Data.String.Regex (Regex, match)
import Prelude (show, class Show, unit, const, Unit, (<<<), return, bind, id, (<>), ($), map, (>>=))
import VirtualDOM (Prop, prop, FunctionSerializer, MakeDOMHandlers, DOM)

foreign import crossAndAtRegex :: Regex

-- a : can be anything, just has to have a JsonEncode & JsonDecode instance
data WEvent a = WEvent { event :: String 
                       , tag :: String }

type WEventHandlers = StrMap (Foreign -> Eff (dom :: DOM, ownsww :: OwnsWW) Json)

on :: forall eff a. WEvent a -> (a -> Eff eff Unit) -> Prop
on (WEvent {event, tag}) handler = prop ("on" <> event <> "@" <> tag) handler

registerWEventHandler :: forall a. (EncodeJson a, DecodeJson a) =>
                         WEventHandlers 
                         -> WEvent a 
                         -> (Foreign -> Eff (dom :: DOM, ownsww :: OwnsWW) a)
                         -> WEventHandlers
registerWEventHandler wes (WEvent {tag}) f = insert tag ((map encodeJson <<< f)) wes

makeDOMHandlersForWEvents :: WebWorker -> WEventHandlers -> MakeDOMHandlers
makeDOMHandlersForWEvents ww weh fullstr = 
  maybe 
    (const $ return unit) -- Wow, how suck, much crappy
    id
    (do matches <- match crossAndAtRegex fullstr
        msnr <- index matches 1
        snr <- msnr
        nr <- fromString snr
        ms <- index matches 2
        s <- ms
        evh <- lookup s weh
        return (\ev -> evh ev 
                 >>= \json -> postMessageToWW ww (toForeign $ show (Message {id: nr, data: json}))))

-- TODO
newtype Message = Message {id :: Int, data :: Json}
instance showMessage :: Show Message where
  show (Message {id: id, data: d}) = "{\"id\": " <> show id <> ", \"data\": " <> show d <> "}"

foreign import mkWorkerFunctionsForWEvents :: forall eff. 
                                Eff (ref :: REF | eff) { functionSerializer :: FunctionSerializer, 
                                                         handler :: String -> Eff eff Unit}

