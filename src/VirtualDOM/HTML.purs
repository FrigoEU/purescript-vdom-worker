module VirtualDOM.HTML where

import VirtualDOM (Prop, vn, prop, VTree)

div :: Array Prop -> Array VTree -> VTree
div = vn "div"
span :: Array Prop -> Array VTree -> VTree
span = vn "span"
table :: Array Prop -> Array VTree -> VTree
table = vn "table"
tbody :: Array Prop -> Array VTree -> VTree
tbody = vn "tbody"
tr :: Array Prop -> Array VTree -> VTree
tr = vn "tr"
td :: Array Prop -> Array VTree -> VTree
td = vn "td"
th :: Array Prop -> Array VTree -> VTree
th = vn "th"
nav :: Array Prop -> Array VTree -> VTree
nav = vn "nav"
aside :: Array Prop -> Array VTree -> VTree
aside = vn "aside"
section :: Array Prop -> Array VTree -> VTree
section = vn "section"
p :: Array Prop -> Array VTree -> VTree
p = vn "p"
button :: Array Prop -> Array VTree -> VTree
button = vn "button"
img :: Array Prop -> Array VTree -> VTree
img = vn "img"
input :: Array Prop -> Array VTree -> VTree
input = vn "input"
i :: Array Prop -> Array VTree -> VTree
i = vn "i"
ul :: Array Prop -> Array VTree -> VTree
ul = vn "ul"
li :: Array Prop -> Array VTree -> VTree
li = vn "li"
a :: Array Prop -> Array VTree -> VTree
a = vn "a"
form :: Array Prop -> Array VTree -> VTree
form = vn "form"

src :: String -> Prop
src = prop "src"
cl :: String -> Prop
cl = prop "className"
style :: String -> Prop
style = prop "style"
name :: String -> Prop
name = prop "name"
typeP :: String -> Prop
typeP = prop "type"
id :: String -> Prop
id = prop "id"
rowspan :: Int -> Prop
rowspan = prop "rowspan"
colspan :: Int -> Prop
colspan = prop "colspan"
title :: String -> Prop
title = prop "title"
href :: String -> Prop
href = prop "href"
tabindex :: Int -> Prop
tabindex = prop "tabindex"
disabled :: Boolean -> Prop
disabled = prop "disabled"
selected :: Boolean -> Prop
selected = prop "selected"
checked :: Boolean -> Prop
checked = prop "checked"
value :: String -> Prop
value = prop "value"

width :: String -> Prop
width = prop "width"

height :: String -> Prop
height = prop "height"
