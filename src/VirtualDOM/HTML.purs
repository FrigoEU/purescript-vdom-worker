module VirtualDOM.HTML where

import Data.StrMap (StrMap)
import VirtualDOM (Prop, prop, VTree, vnode, props)

-- Note: Writing this in fully applied style (instead of point-free) so
-- uncurrying can work

div :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
div ps = vnode "div" (props ps)
span :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
span ps = vnode "span" (props ps)
table :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
table ps = vnode "table" (props ps)
tbody :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
tbody ps = vnode "tbody" (props ps)
tr :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
tr ps = vnode "tr" (props ps)
td :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
td ps = vnode "td" (props ps)
th :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
th ps = vnode "th" (props ps)
nav :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
nav ps = vnode "nav" (props ps)
aside :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
aside ps = vnode "aside" (props ps)
section :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
section ps = vnode "section" (props ps)
p :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
p ps = vnode "p" (props ps)
button :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
button ps = vnode "button" (props ps)
img :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
img ps = vnode "img" (props ps)
input :: forall act. Array (Prop act) -> VTree act
input ps = vnode "input" (props ps) []
select :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
select ps = vnode "select" (props ps)
option :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
option ps = vnode "option" (props ps)
i :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
i ps = vnode "i" (props ps)
ul :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
ul ps = vnode "ul" (props ps)
li :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
li ps = vnode "li" (props ps)
a :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
a ps = vnode "a" (props ps)
form :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
form ps = vnode "form" (props ps)
label :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
label ps = vnode "label" (props ps)
h1 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h1 ps = vnode "h1" (props ps)
h2 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h2 ps = vnode "h2" (props ps)
h3 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h3 ps = vnode "h3" (props ps)
h4 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h4 ps = vnode "h4" (props ps)
h5 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h5 ps = vnode "h5" (props ps)
h6 :: forall act. Array (Prop act) -> Array (VTree act) -> VTree act
h6 ps = vnode "h6" (props ps)

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
