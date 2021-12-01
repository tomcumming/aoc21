module Day1 (readLines, window, day1) where

import Prelude

import Data.Int (fromString)
import Data.List.Lazy (List, Step(..), cons, filter, fromFoldable, length, nil, step, take)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.Traversable (sequence)

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
depthIncreases xs = case step xs of
  Cons x xs -> case step xs of
    Cons y _ -> Just (x > y)
    _ -> Nothing
  _ -> Nothing

day1 :: String -> Maybe Int
day1 inputStr = do
  inputs <- sequence $ map fromString $ readLines inputStr
  increases <- sequence $ map depthIncreases $ window 2 inputs
  pure $ length $ filter (\x -> x) $ increases
