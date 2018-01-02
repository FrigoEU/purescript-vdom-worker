module VirtualDOM.HTML where

import Data.StrMap (StrMap)
import VirtualDOM (Prop, prop, VTree, vnode, props)

-- Note: Writing this in fully applied style (instead of point-free) so
-- uncurrying can work

div :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
div ps cs = vnode "div" (props ps) cs
span :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
span ps cs = vnode "span" (props ps) cs
table :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
table ps cs = vnode "table" (props ps) cs
tbody :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
tbody ps cs = vnode "tbody" (props ps) cs
tr :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
tr ps cs = vnode "tr" (props ps) cs
td :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
td ps cs = vnode "td" (props ps) cs
th :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
th ps cs = vnode "th" (props ps) cs
nav :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
nav ps cs = vnode "nav" (props ps) cs
aside :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
aside ps cs = vnode "aside" (props ps) cs
section :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
section ps cs = vnode "section" (props ps) cs
p :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
p ps cs = vnode "p" (props ps) cs
button :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
button ps cs = vnode "button" (props ps) cs
img :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
img ps cs = vnode "img" (props ps) cs
input :: forall act. Array (Prop act) -> VTree act
input ps = vnode "input" (props ps) []
select :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
select ps cs = vnode "select" (props ps) cs
option :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
option ps cs = vnode "option" (props ps) cs
i :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
i ps cs = vnode "i" (props ps) cs
ul :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
ul ps cs = vnode "ul" (props ps) cs
li :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
li ps cs = vnode "li" (props ps) cs
a :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
a ps cs = vnode "a" (props ps) cs
form :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
form ps cs = vnode "form" (props ps) cs
label :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
label ps cs = vnode "label" (props ps) cs
h1 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h1 ps cs = vnode "h1" (props ps) cs
h2 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h2 ps cs = vnode "h2" (props ps) cs
h3 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h3 ps cs = vnode "h3" (props ps) cs
h4 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h4 ps cs = vnode "h4" (props ps) cs
h5 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h5 ps cs = vnode "h5" (props ps) cs
h6 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h6 ps cs = vnode "h6" (props ps) cs

src :: forall act. String -> Prop act
src v = prop "src" v
cl :: forall act. String -> Prop act
cl v = prop "className" v
style :: forall act. StrMap String -> Prop act
style v = prop "style" v
name :: forall act. String -> Prop act
name v = prop "name" v
typeP :: forall act. String -> Prop act
typeP v = prop "type" v
id :: forall act. String -> Prop act
id v = prop "id" v
rowspan :: forall act. Int -> Prop act
rowspan v = prop "rowspan" v
colspan :: forall act. Int -> Prop act
colspan v = prop "colspan" v
title :: forall act. String -> Prop act
title v = prop "title" v
href :: forall act. String -> Prop act
href v = prop "href" v
tabindex :: forall act. Int -> Prop act
tabindex v = prop "tabindex" v
disabled :: forall act. Boolean -> Prop act
disabled v = prop "disabled" v
selected :: forall act. Boolean -> Prop act
selected v = prop "selected" v
checked :: forall act. Boolean -> Prop act
checked v = prop "checked" v
value :: forall act. String -> Prop act
value v = prop "value" v
placeholder :: forall act. String -> Prop act
placeholder v = prop "placeholder" v

width :: forall act. String -> Prop act
width v = prop "width" v

height :: forall act. String -> Prop act
height v = prop "height" v
