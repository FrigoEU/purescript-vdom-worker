module VirtualDOM.HTML where

import Data.StrMap (StrMap)
import VirtualDOM (Prop, vn, prop, VTree)

div :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
div = vn "div"
span :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
span = vn "span"
table :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
table = vn "table"
tbody :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
tbody = vn "tbody"
tr :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
tr = vn "tr"
td :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
td = vn "td"
th :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
th = vn "th"
nav :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
nav = vn "nav"
aside :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
aside = vn "aside"
section :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
section = vn "section"
p :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
p = vn "p"
button :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
button = vn "button"
img :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
img = vn "img"
input :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
input = vn "input"
i :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
i = vn "i"
ul :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
ul = vn "ul"
li :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
li = vn "li"
a :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
a = vn "a"
form :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
form = vn "form"
label :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
label = vn "label"
h1 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h1 = vn "h1"
h2 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h2 = vn "h2"
h3 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h3 = vn "h3"
h4 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h4 = vn "h4"
h5 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h5 = vn "h5"
h6 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h6 = vn "h6"

src :: forall act. String -> Prop act
src = prop "src"
cl :: forall act. String -> Prop act
cl = prop "className"
style :: forall act. StrMap String -> Prop act
style = prop "style"
name :: forall act. String -> Prop act
name = prop "name"
typeP :: forall act. String -> Prop act
typeP = prop "type"
id :: forall act. String -> Prop act
id = prop "id"
rowspan :: forall act. Int -> Prop act
rowspan = prop "rowspan"
colspan :: forall act. Int -> Prop act
colspan = prop "colspan"
title :: forall act. String -> Prop act
title = prop "title"
href :: forall act. String -> Prop act
href = prop "href"
tabindex :: forall act. Int -> Prop act
tabindex = prop "tabindex"
disabled :: forall act. Boolean -> Prop act
disabled = prop "disabled"
selected :: forall act. Boolean -> Prop act
selected = prop "selected"
checked :: forall act. Boolean -> Prop act
checked = prop "checked"
value :: forall act. String -> Prop act
value = prop "value"
placeholder :: forall act. String -> Prop act
placeholder = prop "placeholder"

width :: forall act. String -> Prop act
width = prop "width"

height :: forall act. String -> Prop act
height = prop "height"
