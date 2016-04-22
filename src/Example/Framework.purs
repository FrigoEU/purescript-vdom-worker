module Example.Framework where

import VirtualDOM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Prelude (Unit)
import WebWorker (IsWW)

data Action = Add Int
type AppEffects = (ref :: REF, isww :: IsWW)
type Dispatch = Action -> Eff AppEffects Unit
