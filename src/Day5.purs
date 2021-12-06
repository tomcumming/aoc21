module Day5 (Coord, Line, parseLine, covered, day5) where

import Prelude

import Data.Int (fromString)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String as String
import Data.Traversable (sequence)
import Day1 (readLines)

type Coord = { x :: Int, y :: Int }
type Direction = Coord
type Line = { from :: Coord, to :: Coord }
type Total = Map.Map Coord Int

parseCoord :: String -> Maybe Coord
parseCoord str = do
  let parts = String.split (String.Pattern ",") str
  cs <- sequence $ map (fromString <<< String.trim) parts
  case cs of
    [x, y] -> Just { x, y }
    _ -> Nothing

normal :: Int -> Int
normal n
  | n == 0 = 0
  | otherwise = n / abs n

direction :: Line -> Direction
direction { from, to } =
  { x: normal (to.x - from.x)
  , y: normal (to.y - from.y)
  }

isSimpleDirection :: Direction -> Boolean
isSimpleDirection { x, y } = x == 0 || y == 0

advance :: Direction -> Coord -> Coord
advance d c = { x: c.x + d.x, y: c.y + d.y }

parseLine :: String -> Maybe Line
parseLine str = do
  let parts = String.split (String.Pattern "->") str
  cs <- sequence $ map parseCoord parts
  case cs of
    [from, to] -> Just { from, to }
    _ -> Nothing

covered :: Line -> Total
covered line = go (direction line) line.from
  where
    go :: Direction -> Coord -> Total
    go d c
      | c == line.to = Map.singleton c 1
      | otherwise = Map.insert c 1 (go d (advance d c))

sumLines :: List.List Line -> Total
sumLines = List.foldl (Map.unionWith (+)) Map.empty
  <<< map covered

day5 :: String -> Maybe Int
day5 inputStr = do
  allLines <- sequence $ map parseLine (readLines inputStr)
  let lines = List.filter (\line -> isSimpleDirection (direction line)) allLines
  let total = sumLines lines
  Just $ Map.size $ Map.filter (_ > 1) total
