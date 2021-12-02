module Day1 (readLines, window, day1, day1_2) where

import Prelude

import Data.Array as Array
import Data.Int (fromString)
import Data.List.Lazy (List, Step(..), cons, filter, fromFoldable, length, nil, step, take)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.Traversable (sequence, sum)

readLines :: String -> List String
readLines = filter (_ /= "") <<< map trim <<< fromFoldable <<< split (Pattern "\n")

window :: forall a. Int -> List a -> List (List a)
window n xs = filter (\xs -> length xs >= n) $ go nil xs
  where
    go :: List a -> List a -> List (List a)
    go xs ys = case step ys of
      Nil -> nil
      Cons y ys ->
        let xs' = cons y xs
        in cons (take n xs') $ go xs' ys

depthIncreases :: List Int -> Maybe Boolean
depthIncreases xs = case Array.fromFoldable xs of
  [x, y] -> Just (x > y)
  _ -> Nothing

depthIncreases2 :: List (List Int) -> Maybe Boolean
depthIncreases2 xs = case Array.fromFoldable xs of
  [x, y] -> Just (sum x > sum y)
  _ -> Nothing

day1 :: String -> Maybe Int
day1 inputStr = do
  inputs <- sequence $ map fromString $ readLines inputStr
  increases <- sequence $ map depthIncreases $ window 2 inputs
  pure $ length $ filter (\x -> x) $ increases

day1_2 :: String -> Maybe Int
day1_2 inputStr = do
  inputs <- sequence $ map fromString $ readLines inputStr
  let triples = window 3 inputs
  let pairs = window 2 triples
  increases <- sequence $ map depthIncreases2 pairs
  pure $ length $ filter (\x -> x) $ increases
