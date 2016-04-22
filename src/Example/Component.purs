module Example.Component (renderExampleComponent) where

import VirtualDOM
import Example.Framework (Action(Add), Dispatch)
import Example.WEvents (noopClick)
import Prelude (show, (<>), (+), ($))
import VirtualDOM.Worker (on)

renderExampleComponent :: Dispatch -> Int -> VTree
renderExampleComponent d c = div ps children
  where
    ps = props [ border "1px solid red"
               , width (show (100 + c) <> "px") 
               , height (show (100 + c) <> "px") 
               , on noopClick (\_ -> d $ Add 10)
               ]
    children = [vtext $ show c]
