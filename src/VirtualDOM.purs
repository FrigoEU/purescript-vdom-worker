module VirtualDOM where

import WebWorker
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)
import Prelude (Unit)

foreign import data VTree :: *
foreign import data VPatch :: *
foreign import data SerializedVPatch :: *
foreign import data Node :: *
foreign import data FunctionSerializer :: *

foreign import data DOM :: !

foreign import vnode :: String -> Props -> Array VTree -> VTree
foreign import vtext :: String -> VTree
foreign import createElement :: VTree -> Node
foreign import appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
foreign import diff :: VTree -> VTree -> Array VPatch
foreign import applyPatch :: forall eff. Node -> Array SerializedVPatch -> MakeDOMHandlers -> Eff (dom :: DOM | eff) Unit

foreign import toString :: Array SerializedVPatch -> String
foreign import fromString :: String -> Array SerializedVPatch
foreign import serializePatch :: FunctionSerializer -> Array VPatch -> Array SerializedVPatch

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











