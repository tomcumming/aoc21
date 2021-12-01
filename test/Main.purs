module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Day1 (day1, day1_2)
import Effect (Effect)
import Effect.Class.Console (log)

showResult :: Boolean -> String
showResult result = if result then "OK" else "WRONG"

day1ExampleInput :: String
day1ExampleInput = """199
    200
    208
    210
    200
    207
    240
    269
    260
    263"""

testDay1 :: Effect Unit
testDay1 = do
  log $ "Day 1: " <> showResult (day1 day1ExampleInput == Just 7)
  log $ "Day 1 pt2: " <> showResult (day1_2 day1ExampleInput == Just 5)

main :: Effect Unit
main = do
  testDay1
