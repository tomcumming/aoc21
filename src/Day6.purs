module Day6 (day6) where

import Prelude

import Data.Array (concatMap)

step :: Array Int -> Array Int
step = concatMap go
  where
  go 0 = [ 6, 8 ]
  go n = [ n - 1 ]

simDays :: Array Int -> Int -> Array Int
simDays state n
  | n == 0 = state
  | otherwise = simDays (step state) (n - 1)

day6 :: Array Int -> Array Int
day6 state = simDays state 80
