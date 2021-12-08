module Test.Day3 (testDay3, day3Tests) where

import Prelude

import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Day3 (day3, leastCommon, mostCommon)
import Effect (Effect)
import Effect.Console (log)
import Test (showResult)

day3ExampleInput :: String
day3ExampleInput =
  """00100
  11110
  10110
  10111
  10101
  01111
  00111
  11100
  10000
  11001
  00010
  01010"""

testMostCommon :: Effect Unit
testMostCommon = do
  let input = List.fromFoldable [ 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0 ]
  log $ "Most Common: " <> showResult (mostCommon input == 1)

testLeastCommon :: Effect Unit
testLeastCommon = do
  let input = List.fromFoldable [ 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0 ]
  log $ "Least Common: " <> showResult (leastCommon input == 0)

day3Tests :: Effect Unit
day3Tests = do
  testMostCommon
  testLeastCommon

testDay3 :: Effect Unit
testDay3 = do
  log $ "Day 3: " <> showResult (day3 day3ExampleInput == Just 198)
