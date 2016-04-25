module Example.WEvents where

import Data.Argonaut.Decode (decodeJson)
import Data.StrMap (empty)
import Prelude (Unit, unit, return)
import VirtualDOM.Worker (registerWEventHandler, WEventHandlers, WEvent(WEvent))

noopClick :: WEvent Unit
noopClick = WEvent { event: "click", tag: "noopclick", decodeJson: decodeJson }

allWEvents :: WEventHandlers
allWEvents = registerWEventHandler empty noopClick (\_ -> return unit)

