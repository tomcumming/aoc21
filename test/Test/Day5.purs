module Test.Day5 (allDay5Tests, testDay5) where

import Prelude

import Data.Maybe (Maybe(..))
import Day5 (day5, parseLine)
import Effect (Effect)
import Effect.Console (log)
import Test (showResult)

testParseLine :: Effect Unit
testParseLine = do
  let expected = { from: { x: 1, y: 2 }, to: { x: 3, y: 4} }
  log $ "Parse Line: "
    <> showResult (parseLine " 1,2 -> 3,4 " == Just expected)

exampleInput :: String
exampleInput = """0,9 -> 5,9
  8,0 -> 0,8
  9,4 -> 3,4
  2,2 -> 2,1
  7,0 -> 7,4
  6,4 -> 2,0
  0,9 -> 2,9
  3,4 -> 1,4
  0,0 -> 8,8
  5,5 -> 8,2"""

testDay5 :: Effect Unit
testDay5 = do
  log $ "Day 5: "
    <> showResult (day5 exampleInput == Just 5)

allDay5Tests :: Effect Unit
allDay5Tests = do
  testParseLine
  testDay5
