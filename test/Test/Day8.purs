module Test.Day8 (allTests, testDay8) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Day8 (parseExample, solve, solveOne, solveSeven)
import Effect (Effect)
import Effect.Console (log)
import Test (justOrDie, showResult)

exampleStr :: String
exampleStr = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"

{-
a 0
b 1
c 2
d 3
e 4
f 5
g 6
-}

testParseExample :: Effect Unit
testParseExample = do
  let
    examples = map Set.fromFoldable
      [ [ 1, 4 ]
      , [ 2, 5, 1, 4, 6, 0, 3 ]
      , [ 2, 1, 3, 6, 4, 5 ]
      , [ 5, 6, 0, 4, 2, 3 ]
      , [ 2, 6, 4, 1 ]
      , [ 5, 3, 2, 6, 4 ]
      , [ 0, 6, 4, 1, 5, 3 ]
      , [ 5, 4, 2, 3, 1 ]
      , [ 5, 0, 1, 2, 3 ]
      , [ 4, 3, 1 ]
      ]
  let
    tests = map Set.fromFoldable
      [ [ 5, 3, 6, 0, 2, 1, 4 ] -- cefbgd gcbe
      , [ 2, 4, 5, 3, 1 ]
      , [ 2, 4, 5, 1, 6, 3 ]
      , [ 6, 2, 1, 4 ]
      ]
  log $ "Parse Example: "
    <> showResult (parseExample exampleStr == Just { examples, tests })

testSolveOne :: Effect Unit
testSolveOne = do
  let expected = Set.fromFoldable [ { c: 1, f: 4 }, { c: 4, f: 1 } ]
  { examples } <- justOrDie $ parseExample exampleStr
  log $ "Solve One: "
    <> showResult (Set.fromFoldable (solveOne examples) == expected)

testSolveSeven :: Effect Unit
testSolveSeven = do
  { examples } <- justOrDie $ parseExample exampleStr
  let solutions = Set.fromFoldable (solveOne examples >>= solveSeven examples)
  let expected = Set.fromFoldable [ { a: 3, c: 1, f: 4 }, { a: 3, c: 4, f: 1 } ]
  log $ "Solve Seven: "
    <> showResult (solutions == expected)

testDay8 :: Effect Unit
testDay8 = do
  { examples } <- justOrDie $ parseExample exampleStr
  let result = isJust (solve examples)
  log $ "Day 8: " <> showResult result

allTests :: Effect Unit
allTests = do
  testParseExample
  testSolveOne
  testSolveSeven
  testDay8
