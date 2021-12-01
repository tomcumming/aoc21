module Day1 (readLines, pairs, day1) where

import Prelude

import Data.Int (fromString)
import Data.List.Lazy (List, Step(..), cons, filter, fromFoldable, length, nil, step)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

readLines :: String -> List String
readLines = filter (_ /= "") <<< map trim <<< fromFoldable <<< split (Pattern "\n")

pairs :: forall a. List a -> List (Tuple a a)
pairs xs = go Nothing xs
  where
    go :: Maybe a -> List a -> List (Tuple a a)
    go h xs = case Tuple h (step xs) of
      Tuple _ Nil -> nil
      Tuple Nothing (Cons x xs) -> go (Just x) xs
      Tuple (Just l) (Cons x xs) -> cons (Tuple l x) $ go (Just x) xs

depthIncreases :: Tuple Int Int -> Boolean
depthIncreases (Tuple a b) = a < b

day1 :: String -> Maybe Int
day1 inputStr = do
  inputs <- sequence $ map fromString $ readLines inputStr
  pure $ length $ filter depthIncreases $ pairs inputs
