module Day4 (bingo, boardSum, readWords, parseBoard, day4, Marked, Board) where

import Prelude

import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Traversable (sequence)
import Day1 (readLines)

type Board = List.List (List.List Int)
type Marked = Set.Set Int

readWords :: String -> List.List String
readWords = List.filter (not <<< String.null)
  <<< List.fromFoldable
  <<< String.split (String.Pattern " ")

readBingoNumbers :: String -> Maybe (List.List Int)
readBingoNumbers = sequence
  <<< map fromString
  <<< List.fromFoldable
  <<< String.split (String.Pattern ",")
  <<< String.trim

bingo :: Marked -> Board -> Boolean
bingo marked board = testWin board || testWin (List.transpose board)
  where
    testWin :: Board -> Boolean
    testWin = List.any (List.all $ \x -> Set.member x marked)

boardSum :: Marked -> Board -> Int
boardSum marked = sum <<< List.filter (\x -> not $ Set.member x marked) <<< List.concat

parseBoard :: String -> Maybe Board
parseBoard = sequence
  <<< map (sequence <<< map fromString)
  <<< map readWords
  <<< List.fromFoldable
  <<< readLines

day4 :: List.List String -> String -> Maybe Int
day4 boardStrs markedStr = do
  boards <- sequence $ map parseBoard boardStrs
  marks <- readBingoNumbers markedStr
  go Set.empty boards marks
  where
    go :: Marked -> List.List Board -> List.List Int -> Maybe Int
    go lastMarked boards marks = do
      { head, tail } <- List.uncons marks
      let marked = Set.insert head lastMarked
      case List.find (bingo marked) boards of
        Nothing -> go marked boards tail
        Just board -> Just $ boardSum marked board * head
