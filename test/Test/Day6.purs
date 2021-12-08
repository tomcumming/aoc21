module Test.Day6 (testDay6) where

import Prelude

import Data.Array (length)
import Day6 (day6)
import Effect (Effect)
import Effect.Console (log)
import Test (showResult)

testDay6 :: Effect Unit
testDay6 = do
  let state = [ 3, 4, 3, 1, 2 ]
  log $ "Day 6: "
    <> showResult (length (day6 state) == 5934)
