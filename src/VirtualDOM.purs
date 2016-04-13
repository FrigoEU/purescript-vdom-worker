module VirtualDOM where

import WebWorker
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Argonaut.Core (Json)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Prelude (Unit)

foreign import data VTree :: *
foreign import data VPatch :: *
foreign import data VPatchSerialized :: *
foreign import data Node :: *
foreign import data FunctionSerializer :: *
-- foreign import data MakeEventHandlerSendToWorker :: *

foreign import data DOM :: !
foreign import data MUT :: !

foreign import vnode :: forall props. String -> { | props} -> Array VTree -> VTree
foreign import vtext :: String -> VTree
foreign import createElement :: VTree -> Node
foreign import appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
foreign import diff :: VTree -> VTree -> Array VPatch
foreign import serializePatch :: FunctionSerializer -> Array VPatch -> Array VPatchSerialized
foreign import toString :: Array VPatchSerialized -> String
foreign import fromString :: String -> Array VPatchSerialized
foreign import fromJson :: MakeEventHandlerSendToWorker -> Array VPatchSerialized -> Array VPatch
foreign import applyPatch :: forall eff. Node -> Array VPatch -> Eff (dom :: DOM | eff) Unit
foreign import mkWorkerFunctions :: forall eff. 
                                PropSerializer -> 
                                Eff (mut :: MUT | eff) { functionSerializer :: FunctionSerializer, 
                                                         handler :: String -> Eff eff Unit}
-- foreign import exampleSerializeProp :: PropSerializer

type MakeEventHandlerSendToWorker = String -> (Foreign -> Eff (dom :: DOM, ownsww :: OwnsWW) Unit)

type PropSerializer = String -> {prop :: String, val :: String}
type ReplaceWithEventHandler = String -> Maybe (Foreign -> Eff (dom :: DOM) Json)

foreign import crossAndAtRegex :: Regex

mkMakeHandler :: WebWorker -> ReplaceWithEventHandler -> MakeEventHandlerSendToWorker
mkMakeHandler ww repl fullstr = 
  maybe 
    (unsafeThrow $ "Couldn't find event handler for " <> fullstr) -- Wow, how suck, much crappy
    id
    (do matches <- match crossAndAtRegex str
        nr <- index matches 0
        s <- index matches 1
        evh <- repl s
        return (postMessage ww <<< toForeign <<< evh))

