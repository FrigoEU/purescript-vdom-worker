module VirtualDOM.Worker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Array (index)
import Data.Either (either, Either)
import Data.Foreign (Foreign)
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.StrMap (StrMap, insert, lookup)
import Data.String.Regex (Regex, match)
import Prelude (Unit, return, ($), bind, (>>=), id, unit, const, map, (<<<), (<>))
import Unsafe.Coerce (unsafeCoerce)
import VirtualDOM (Prop, prop, FunctionSerializer, MakeDOMHandlers, DOM)
import WebWorker (Channel, postMessageToWorkerC, WebWorker, OwnsWW)

foreign import crossAndAtRegex :: Regex

-- a : can be anything, just has to have a JsonEncode & JsonDecode instance
-- decodeJson property is here so we get the correct typeclass instance immediately. 
--   In the handler we don't have the type information anymore so the compiler can't know which DecodeJson instance to use
data WEvent a = WEvent { event :: String 
                       , tag :: String 
                       , decodeJson :: Json -> Either String a} 

type WEventHandlers = StrMap (Foreign -> Eff (dom :: DOM, ownsww :: OwnsWW) Json)

-- TODO: hier wordt telkens nieuwe functie gemaakt!!
on :: forall eff a. (DecodeJson a) => WEvent a -> (a -> Eff eff Unit) -> Prop
on (WEvent {event, tag, decodeJson}) handler = 
  prop ("on" <> event <> "@" <> tag) (\fn -> either (\_ -> return unit) handler (decodeJson fn))

registerWEventHandler :: forall a. (EncodeJson a, DecodeJson a) =>
                         WEventHandlers 
                         -> WEvent a 
                         -- TODO: Dit is denk ik beter ... -> Eff (...) F a
                         -> (Foreign -> Eff (dom :: DOM, ownsww :: OwnsWW) a)
                         -> WEventHandlers
registerWEventHandler wes (WEvent {tag}) f = insert tag ((map encodeJson <<< f)) wes

makeDOMHandlersForWEvents :: WebWorker -> Channel WEventMessage -> WEventHandlers -> MakeDOMHandlers
makeDOMHandlersForWEvents ww ch weh fullstr = 
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
                 >>= \json -> postMessageToWorkerC ww ch (WEventMessage {id: nr, data: json})))

-- TODO
newtype WEventMessage = WEventMessage {id :: Int, data :: Json}
instance encodeJsonWEventMessage :: EncodeJson WEventMessage where
  encodeJson = unsafeCoerce
instance decodeJsonWEvent :: DecodeJson WEventMessage where
  decodeJson j = do 
    strm <- decodeJson j
    id <- strm .? "id"
    d <- strm .? "data"
    return $ WEventMessage {id, data: d}

foreign import mkWorkerFunctionsForWEvents :: forall eff1 eff2. 
                                Eff (ref :: REF | eff1) 
                                    { functionSerializer :: FunctionSerializer
                                    , handler :: WEventMessage -> Eff (ref :: REF | eff2) Unit}
