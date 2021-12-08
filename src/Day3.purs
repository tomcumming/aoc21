module Day3 (day3, fromBinary, mostCommon, leastCommon, parseLine) where

import Prelude

import Data.Int as Int
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence)
import Day1 (readLines)

parseLine :: String -> Maybe (List.List Int)
parseLine inStr = case String.uncons inStr of
  Just { head, tail } | head == String.codePointFromChar '0' ->
    List.cons 0 <$> (parseLine tail)
  Just { head, tail } | head == String.codePointFromChar '1' ->
    List.cons 1 <$> (parseLine tail)
  Just _ -> Nothing
  Nothing -> Just List.nil

mostCommon :: List.List Int -> Int
mostCommon xs =
  let
    { yes, no } = List.partition (_ == 1) xs
  in
    if List.length yes >= List.length no then 1 else 0

leastCommon :: List.List Int -> Int
leastCommon xs = if mostCommon xs == 1 then 0 else 1

fromBinary :: List.List Int -> Int
fromBinary xs = _.t
  $ List.foldl
      (\{ t, i } n -> { t: t + n * Int.pow 2 i, i: i + 1 })
      { t: 0, i: 0 }
      (List.reverse xs)

day3 :: String -> Maybe Int
day3 inputStr = do
  parsed <- sequence $ map parseLine (readLines inputStr)
  let txd = List.transpose parsed
  let gamma = fromBinary (map mostCommon txd)
  let epsilon = fromBinary (map leastCommon txd)
  Just $ gamma * epsilon
