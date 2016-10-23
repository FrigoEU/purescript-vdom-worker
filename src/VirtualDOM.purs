module VirtualDOM where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.Event.Event (Event)
import Data.Argonaut.Core (jsonSingletonObject, JObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Function.Uncurried (mkFn2, Fn2)
import Prelude (pure, Unit, bind)
import Unsafe.Coerce (unsafeCoerce)
import WebWorker (OwnsWW)

foreign import data VTree :: *
foreign import data VPatch :: *
foreign import data SerializedVPatches :: *
foreign import data Node :: *


foreign import vnode :: String -> Props -> Array VTree -> VTree
foreign import vtext :: String -> VTree
foreign import createElement :: forall eff. VTree -> Eff (dom :: DOM | eff) Node
foreign import appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
foreign import diff :: VTree -> VTree -> Array VPatch
foreign import applyPatch :: forall eff. Node -> SerializedVPatches -> DeserializeHandlers -> Eff (dom :: DOM | eff) Unit

type FunctionSerializer = forall a. Fn2 String a {prop :: String, id :: String}
type DeserializeHandlers = String -> (Event -> Eff (dom :: DOM, ownsww :: OwnsWW, err :: EXCEPTION) Unit)

dummyFunctionSerializer :: FunctionSerializer
dummyFunctionSerializer = mkFn2 (\_ _ -> {prop: "", id: ""})
foreign import serializePatchImpl :: FunctionSerializer -> Array VPatch -> SerializedVPatches
serializePatch :: Array VPatch -> SerializedVPatches
serializePatch = serializePatchImpl dummyFunctionSerializer


foreign import data Prop :: *
foreign import data Props :: *
foreign import prop :: forall a. String -> a -> Prop
foreign import props :: Array Prop -> Props

vn :: String -> Array Prop -> Array VTree -> VTree
vn tag ps = vnode tag (props ps)

instance encodeJsonSerializedVPatches :: EncodeJson SerializedVPatches where
  encodeJson obj = jsonSingletonObject "VirtualDOM.SerializedVPatches" (unsafeCoerce obj :: Json)

instance decodeJsonSerializedVPatches :: DecodeJson SerializedVPatches where
  decodeJson obj = do
    jobj :: JObject <- decodeJson obj
    servps :: Json <- jobj .? "VirtualDOM.SerializedVPatches"
    pure (unsafeCoerce servps :: SerializedVPatches)
