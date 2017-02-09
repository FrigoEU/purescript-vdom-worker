module VirtualDOM.Lazy where

import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8, runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8)
import VirtualDOM (VTree)

foreign import lazyRef1Impl :: forall a act. Fn2 (a -> VTree act) a (VTree act)
lazyRef1 :: forall a act. (a -> VTree act) -> a -> VTree act
lazyRef1 = runFn2 lazyRef1Impl
foreign import lazyRef2Impl :: forall a b act. Fn3 (a -> b -> VTree act) a b (VTree act)
lazyRef2 :: forall a b act. (a -> b -> VTree act) -> a -> b -> VTree act
lazyRef2 = runFn3 lazyRef2Impl
foreign import lazyRef3Impl :: forall a b c act. Fn4 (a -> b -> c -> VTree act) a b c (VTree act)
lazyRef3 :: forall a b c act. (a -> b -> c -> VTree act) -> a -> b -> c -> VTree act
lazyRef3 = runFn4 lazyRef3Impl
foreign import lazyRef4Impl :: forall a b c d act. Fn5 (a -> b -> c -> d -> VTree act) a b c d (VTree act)
lazyRef4 :: forall a b c d act. (a -> b -> c -> d -> VTree act) -> a -> b -> c -> d -> VTree act
lazyRef4 = runFn5 lazyRef4Impl
foreign import lazyRef5Impl :: forall a b c d e act. Fn6 (a -> b -> c -> d -> e -> VTree act) a b c d e (VTree act)
lazyRef5 :: forall a b c d e act. (a -> b -> c -> d -> e -> VTree act) -> a -> b -> c -> d -> e -> VTree act
lazyRef5 = runFn6 lazyRef5Impl
foreign import lazyRef6Impl :: forall a b c d e f act. Fn7 (a -> b -> c -> d -> e -> f -> VTree act) a b c d e f (VTree act)
lazyRef6 :: forall a b c d e f act. (a -> b -> c -> d -> e -> f -> VTree act) -> a -> b -> c -> d -> e -> f -> VTree act
lazyRef6 = runFn7 lazyRef6Impl
foreign import lazyRef7Impl :: forall a b c d e f g act. Fn8 (a -> b -> c -> d -> e -> f -> g -> VTree act) a b c d e f g (VTree act)
lazyRef7 :: forall a b c d e f g act. (a -> b -> c -> d -> e -> f -> g -> VTree act) -> a -> b -> c -> d -> e -> f -> g -> VTree act
lazyRef7 = runFn8 lazyRef7Impl
