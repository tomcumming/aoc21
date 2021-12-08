module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Day1 (day1, day1_2)
import Day2 (day2, day2_2)
import Effect (Effect)
import Effect.Class.Console (log)
import Test (showResult)
import Test.Day3 (testDay3)
import Test.Day4 (testDay4)
import Test.Day5 (testDay5)
import Test.Day6 (testDay6)
import Test.Day7 (testDay7)
import Test.Day8 (testDay8)

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

day2ExampleInput :: String
day2ExampleInput = """forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2"""

testDay2 :: Effect Unit
testDay2 = do
  log $ "Day 2: " <> showResult (day2 day2ExampleInput == Just 150)
  log $ "Day 2 pt2: " <> showResult (day2_2 day2ExampleInput == Just 900)

main :: Effect Unit
main = do
  testDay1
  testDay2
  testDay3
  testDay4
  testDay5
  testDay6
  testDay7
  testDay8
