module Day7 (score, score2, day7, day7_2) where

import Prelude

import Data.Array (foldl, range)
import Data.Foldable (maximum, minimum, minimumBy)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

score :: Array Int -> Int -> Int
score xs n = foldl (\a b -> a + abs (b - n)) 0 xs

score2 :: Array Int -> Int -> Int
score2 xs n = foldl (\a b -> let c = abs (b - n) in a + (c * (c + 1)) / 2) 0 xs

day7 :: Array Int -> Maybe Int
day7 xs = do
  min <- minimum xs
  max <- maximum xs
  n <- minimumBy (comparing (score xs)) (range min max)
  Just $ score xs n

day7_2 :: Array Int -> Maybe Int
day7_2 xs = do
  min <- minimum xs
  max <- maximum xs
  n <- minimumBy (comparing (score2 xs)) (range min max)
  Just $ score2 xs n
