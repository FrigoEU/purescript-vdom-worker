module VirtualDOM where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Node.Types (Node)
import Data.Argonaut.Core (jsonSingletonObject, JObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Function.Uncurried (mkFn2, Fn2)
import Prelude (pure, Unit, bind)
import Unsafe.Coerce (unsafeCoerce)
import WebWorker (OwnsWW)

foreign import data VTree :: * -> *
foreign import data VPatch :: * -> *
foreign import data SerializedVPatches :: * -> *

foreign import vnode :: forall act. String -> (Props act) -> Array (VTree act) -> VTree act
foreign import vtext :: forall act. String -> VTree act
foreign import createElement :: forall eff act. VTree act -> Eff (dom :: DOM | eff) Node
foreign import appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
foreign import diff :: forall act. VTree act -> VTree act -> Array (VPatch act)
foreign import applyPatch :: forall eff act. Node -> SerializedVPatches act -> DeserializeHandlers (dom :: DOM | eff) -> Eff (dom :: DOM | eff) Unit

type FunctionSerializer = forall a. Fn2 String a {prop :: String, id :: String}
type DeserializeHandlers e = String -> (Event -> Eff e Unit)

dummyFunctionSerializer :: FunctionSerializer
dummyFunctionSerializer = mkFn2 (\_ _ -> {prop: "", id: ""})
foreign import serializePatchImpl :: forall act. FunctionSerializer -> Array (VPatch act) -> SerializedVPatches act
serializePatch :: forall act. Array (VPatch act) -> (SerializedVPatches act)
serializePatch = serializePatchImpl dummyFunctionSerializer


foreign import data Prop :: * -> *
foreign import data Props :: * -> *
foreign import prop :: forall a act. String -> a -> Prop act
foreign import props :: forall act. Array (Prop act) -> Props act

vn :: forall act. String -> Array (Prop act) -> Array (VTree act) -> (VTree act)
vn tag ps = vnode tag (props ps)

instance encodeJsonSerializedVPatches :: EncodeJson (SerializedVPatches act) where
  encodeJson obj = jsonSingletonObject "VirtualDOM.SerializedVPatches" (unsafeCoerce obj :: Json)

instance decodeJsonSerializedVPatches :: DecodeJson (SerializedVPatches act) where
  decodeJson obj = do
    jobj :: JObject <- decodeJson obj
    servps :: Json <- jobj .? "VirtualDOM.SerializedVPatches"
    pure (unsafeCoerce servps :: SerializedVPatches act)
