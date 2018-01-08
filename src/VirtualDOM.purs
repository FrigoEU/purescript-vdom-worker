module VirtualDOM where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Node)
import Data.Foreign (Foreign, F, unsafeFromForeign)
import Data.Foreign.Index (readProp)
import Data.Function.Uncurried (mkFn2, Fn2)
import Prelude (pure, Unit, bind)

foreign import data VTree :: Type -> Type
foreign import data VPatch :: Type -> Type
foreign import data SerializedVPatches :: Type -> Type
foreign import data MainThreadProp :: Type

foreign import vnode :: forall act. String -> (Props act) -> Array (VTree act) -> VTree act
foreign import vtext :: forall act. String -> VTree act
foreign import createElement :: forall eff act. VTree act -> Eff (dom :: DOM | eff) Node
foreign import appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
foreign import diff :: forall act. VTree act -> VTree act -> Array (VPatch act)
foreign import applyPatch :: forall eff act. Node -> SerializedVPatches act -> Deserializer -> Eff (dom :: DOM | eff) Unit

type FunctionSerializer = forall a. Fn2 String a {prop :: String, id :: String}
type Deserializer = String -> MainThreadProp

dummyFunctionSerializer :: FunctionSerializer
dummyFunctionSerializer = mkFn2 (\_ _ -> {prop: "", id: ""})
foreign import serializePatchImpl :: forall act. FunctionSerializer -> Array (VPatch act) -> SerializedVPatches act
serializePatch :: forall act. Array (VPatch act) -> (SerializedVPatches act)
serializePatch = serializePatchImpl dummyFunctionSerializer


foreign import data Prop :: Type -> Type
foreign import data Props :: Type -> Type
foreign import prop :: forall a act. String -> a -> Prop act
foreign import props :: forall act. Array (Prop act) -> Props act

foreign import stringify :: forall act. {"VirtualDOM.SerializedVPatches" :: SerializedVPatches act} -> String
foreign import parse :: String -> Foreign

encodePatches :: forall act. SerializedVPatches act -> String
encodePatches obj = stringify {"VirtualDOM.SerializedVPatches": obj}
decodePatches :: forall act. String -> F (SerializedVPatches act)
decodePatches str = do
  (obj :: Foreign) <- readProp "VirtualDOM.SerializedVPatches" (parse str)
  pure (unsafeFromForeign obj)
