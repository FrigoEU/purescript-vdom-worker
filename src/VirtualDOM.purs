module VirtualDOM where

import Control.Monad.Eff (Eff)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (jsonSingletonObject, JObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Foreign (Foreign)
import Prelude (Unit, return, bind)
import Unsafe.Coerce (unsafeCoerce)
import WebWorker (OwnsWW)

foreign import data VTree :: *
foreign import data VPatch :: *
foreign import data SerializedVPatches :: *
foreign import data Node :: *
foreign import data FunctionSerializer :: *

foreign import data DOM :: !

foreign import vnode :: String -> Props -> Array VTree -> VTree
foreign import vtext :: String -> VTree
foreign import createElement :: forall eff. VTree -> Eff (dom :: DOM | eff) Node
foreign import appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
foreign import diff :: VTree -> VTree -> Array VPatch
foreign import applyPatch :: forall eff. Node -> SerializedVPatches -> MakeDOMHandlers -> Eff (dom :: DOM | eff) Unit

foreign import serializePatch :: FunctionSerializer -> Array VPatch -> SerializedVPatches

type MakeDOMHandlers = String -> (Foreign -> Eff (dom :: DOM, ownsww :: OwnsWW) Unit)

foreign import data Prop :: *
foreign import data Props :: *
foreign import prop :: forall a. String -> a -> Prop
foreign import props :: Array Prop -> Props

div :: Props -> Array VTree -> VTree
div = vnode "div"

checked :: Boolean -> Prop
checked = prop "checked"

width :: String -> Prop
width = prop "width"

height :: String -> Prop
height = prop "height"

border :: String -> Prop
border = prop "border"

instance encodeJsonSerializedVPatches :: EncodeJson SerializedVPatches where
  encodeJson obj = jsonSingletonObject "VirtualDOM.SerializedVPatches" (unsafeCoerce obj :: Json)

instance decodeJsonSerializedVPatches :: DecodeJson SerializedVPatches where
  decodeJson obj = do
    jobj :: JObject <- decodeJson obj
    servps :: Json <- jobj .? "VirtualDOM.SerializedVPatches"
    return (unsafeCoerce servps :: SerializedVPatches)
