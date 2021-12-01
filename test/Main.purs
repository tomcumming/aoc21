module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Day1 (day1)
import Effect (Effect)
import Effect.Class.Console (log)

showResult :: Boolean -> String
showResult result = if result then "OK" else "WRONG"

testDay1 :: Effect Unit
testDay1 = do
  let sample = """99
    200
    208
    210
    200
    207
    240
    269
    260
    263"""
  log $ "Day 1 example: " <> showResult (day1 sample == Just 7)

main :: Effect Unit
main = do
  testDay1
