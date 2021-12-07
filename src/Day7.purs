module Day7 (score, day7) where

import Prelude

import Data.Array (foldl, range)
import Data.Foldable (maximum, minimum, minimumBy)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

score :: Array Int -> Int -> Int
score xs n = foldl (\a b -> a + abs (b - n)) 0 xs

day7 :: Array Int -> Maybe Int
day7 xs = do
  min <- minimum xs
  max <- maximum xs
  n <- minimumBy (comparing (score xs)) (range min max)
  Just $ score xs n
