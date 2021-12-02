module Day2 (day2, day2_2) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence)
import Day1 (readLines)

data Move = Vert Int | Horiz Int

parseMove :: String -> Maybe Move
parseMove line = case String.split (String.Pattern " ") line of
  ["forward", n] -> Horiz <$> Int.fromString n
  ["down", n] -> Vert <$> Int.fromString n
  ["up", n] -> Vert <<< negate <$> Int.fromString n
  _ -> Nothing

sumMoves :: forall m. Foldable m => m Move -> { vert :: Int, horiz :: Int }
sumMoves = foldl go { vert: 0, horiz: 0 }
  where
    go { vert, horiz } m = case m of
      Vert n -> { vert: vert + n, horiz }
      Horiz n -> { vert, horiz: horiz + n }

sumMoves2 :: forall m. Foldable m => m Move -> { vert :: Int, horiz :: Int, aim :: Int }
sumMoves2 = foldl go { vert: 0, horiz: 0, aim: 0 }
  where
    go { vert, horiz, aim } m = case m of
      Vert n -> { vert, horiz, aim: aim + n }
      Horiz n -> { vert: vert + n * aim, horiz: horiz + n, aim }

day2 :: String -> Maybe Int
day2 inputStr = do
  moves <- sequence $ map parseMove (readLines inputStr)
  let { vert, horiz } = sumMoves moves
  pure (vert * horiz)

day2_2 :: String -> Maybe Int
day2_2 inputStr = do
  moves <- sequence $ map parseMove (readLines inputStr)
  let { vert, horiz } = sumMoves2 moves
  pure (vert * horiz)
