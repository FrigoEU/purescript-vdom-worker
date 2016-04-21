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

foreign import vnode :: forall props. String -> { | props} -> Array VTree -> VTree
foreign import vtext :: String -> VTree
foreign import createElement :: VTree -> Node
foreign import appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
foreign import diff :: VTree -> VTree -> Array VPatch
foreign import applyPatch :: forall eff. Node -> Array VPatch -> Eff (dom :: DOM | eff) Unit

foreign import toString :: Array SerializedVPatch -> String
foreign import fromString :: String -> Array SerializedVPatch
foreign import fromJson :: MakeDOMHandlers -> Array SerializedVPatch -> Array VPatch
foreign import serializePatch :: FunctionSerializer -> Array VPatch -> Array SerializedVPatch

type MakeDOMHandlers = String -> (Foreign -> Eff (dom :: DOM, ownsww :: OwnsWW) Unit)

