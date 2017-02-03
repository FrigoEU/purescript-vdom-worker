module VirtualDOM.Lazy where

import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8, runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8)
import VirtualDOM (VTree)

foreign import lazyRef1Impl :: forall a. Fn2 (a -> VTree) a VTree
lazyRef1 :: forall a. (a -> VTree) -> a -> VTree
lazyRef1 = runFn2 lazyRef1Impl
foreign import lazyRef2Impl :: forall a b. Fn3 (a -> b -> VTree) a b VTree
lazyRef2 :: forall a b. (a -> b -> VTree) -> a -> b -> VTree
lazyRef2 = runFn3 lazyRef2Impl
foreign import lazyRef3Impl :: forall a b c. Fn4 (a -> b -> c -> VTree) a b c VTree
lazyRef3 :: forall a b c. (a -> b -> c -> VTree) -> a -> b -> c -> VTree
lazyRef3 = runFn4 lazyRef3Impl
foreign import lazyRef4Impl :: forall a b c d. Fn5 (a -> b -> c -> d -> VTree) a b c d VTree
lazyRef4 :: forall a b c d. (a -> b -> c -> d -> VTree) -> a -> b -> c -> d -> VTree
lazyRef4 = runFn5 lazyRef4Impl
foreign import lazyRef5Impl :: forall a b c d e. Fn6 (a -> b -> c -> d -> e -> VTree) a b c d e VTree
lazyRef5 :: forall a b c d e. (a -> b -> c -> d -> e -> VTree) -> a -> b -> c -> d -> e -> VTree
lazyRef5 = runFn6 lazyRef5Impl
foreign import lazyRef6Impl :: forall a b c d e f. Fn7 (a -> b -> c -> d -> e -> f -> VTree) a b c d e f VTree
lazyRef6 :: forall a b c d e f. (a -> b -> c -> d -> e -> f -> VTree) -> a -> b -> c -> d -> e -> f -> VTree
lazyRef6 = runFn7 lazyRef6Impl
foreign import lazyRef7Impl :: forall a b c d e f g. Fn8 (a -> b -> c -> d -> e -> f -> g -> VTree) a b c d e f g VTree
lazyRef7 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g -> VTree) -> a -> b -> c -> d -> e -> f -> g -> VTree
lazyRef7 = runFn8 lazyRef7Impl
