module Test.Day9 where

import Prelude

import Data.Array as Array
import Data.Set as Set
import Data.Traversable (sequence, sum)
import Day9 (HeightMap, lowPoints, riskLevel)
import Effect (Effect)
import Effect.Console (log)
import Test (justOrDie, showResult)

example :: HeightMap
example =
  [ [ 2, 1, 9, 9, 9, 4, 3, 2, 1, 0 ]
  , [ 3, 9, 8, 7, 8, 9, 4, 9, 2, 1 ]
  , [ 9, 8, 5, 6, 7, 8, 9, 8, 9, 2 ]
  , [ 8, 7, 6, 7, 8, 9, 6, 7, 8, 9 ]
  , [ 9, 8, 9, 9, 9, 6, 5, 6, 7, 8 ]
  ]

testLowPoints :: Effect Unit
testLowPoints = do
  let ps = Set.fromFoldable (lowPoints example)
  let
    expected = Set.fromFoldable
      [ { x: 1, y: 0 }
      , { x: 9, y: 0 }
      , { x: 2, y: 2 }
      , { x: 6, y: 4 }
      ]
  log $ "Low points: "
    <> showResult (ps == expected)

testDay9 :: Effect Unit
testDay9 = do
  rls <- justOrDie
    $ sequence
    $ map (riskLevel example)
    $ Array.fromFoldable
    $ lowPoints example
  log $ "Day 9: "
    <> showResult (sum rls == 15)
