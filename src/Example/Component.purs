module Example.Component (render) where

import VirtualDOM

render :: Int -> VTree
render c = vnode "div" props
  where
    props = { border: "1px solid red"
            , width: show (100 + c) <> "px" 
            , height: show (100 + c) <> "px" }
    children = [vtext $ show c]
