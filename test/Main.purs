module Test.Main where

import Prelude (class Eq, bind, eq, ($))
import Test.Unit (test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)
import VirtualDOM.SEvent (magic)

main = runTest $ test "magic" do
  testMagicNoProps
  testMagicSimple
  testMagicSubAction
  testMagicRecursiveAction

data SubAction = SubAction1 {val :: String}
               | SubAction2 String SubAction
data Action = Action0
            | Action1 {val :: String}
            | Action2 SubAction
derive instance eqSubAction :: Eq SubAction
derive instance eqAction :: Eq Action

testMagicNoProps = assert
                  "Doesn't work with noPropsAction"
                  ((magic Action0 "abc") `eq` (Action0))

testMagicSimple = assert
                  "Doesn't work with simpleAction"
                  ((magic (Action1 {val: ""}) "abc") `eq` (Action1 {val: "abc"}))

testMagicSubAction = assert
                     "Doesn't work with SubAction"
                     ((magic (Action2 (SubAction1 {val: ""})) "abc") `eq` (Action2 (SubAction1 {val: "abc"})))

testMagicRecursiveAction = assert
                     "Doesn't work with RecursiveAction"
                     ((magic (Action2
                              (SubAction2 "bla"
                               (SubAction2 "bla"
                                (SubAction1 {val: ""})))) "abc")
                      `eq` (Action2
                              (SubAction2 "bla"
                               (SubAction2 "bla"
                                (SubAction1 {val: "abc"})))))
