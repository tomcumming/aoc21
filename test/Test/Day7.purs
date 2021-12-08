module Test.Day7 (testDay7, testDay7_2, testScore) where

import Prelude

import Data.Maybe (Maybe(..))
import Day7 (day7, day7_2, score)
import Effect (Effect)
import Effect.Console (log)
import Test (showResult)

testScore :: Effect Unit
testScore = do
  log $ "Test Score 2: "
    <> showResult (score [ 16, 1, 2, 0, 4, 2, 7, 1, 2, 14 ] 2 == 37)
  log $ "Test Score 3: "
    <> showResult (score [ 16, 1, 2, 0, 4, 2, 7, 1, 2, 14 ] 3 == 39)

testDay7 :: Effect Unit
testDay7 = do
  log $ "Day 7: "
    <> showResult (day7 [ 16, 1, 2, 0, 4, 2, 7, 1, 2, 14 ] == Just 37)

testDay7_2 :: Effect Unit
testDay7_2 = do
  log $ "Day 7: "
    <> showResult (day7_2 [ 16, 1, 2, 0, 4, 2, 7, 1, 2, 14 ] == Just 206)
