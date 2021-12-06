module Test.Day4 (allDay4Tests, testDay4) where

import Prelude

import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Day4 (Board, Marked, bingo, boardSum, day4, parseBoard)
import Effect (Effect)
import Effect.Console (log)
import Test (showResult)

exampleBoard1 :: Board
exampleBoard1 = List.fromFoldable $ map List.fromFoldable
  [ [22, 13, 17, 11, 0]
  , [8, 2, 23, 4, 24]
  , [21, 9, 14, 16, 7]
  , [6, 10, 3, 18, 5]
  , [1, 12, 20, 15, 19]
  ]

exampleBoard2 :: Board
exampleBoard2 = List.fromFoldable $ map List.fromFoldable
  [ [3, 15, 0, 2, 22]
  , [9, 18, 13, 17, 5]
  , [19, 8, 7, 25, 23]
  , [20, 11, 10, 24, 4]
  , [14, 21, 16, 12, 6]
  ]

exampleBoard3 :: Board
exampleBoard3 = List.fromFoldable $ map List.fromFoldable
  [ [14, 21, 17, 24, 4]
  , [10, 16, 15, 9, 19]
  , [18, 8, 23, 26, 20]
  , [22, 11, 13, 6, 5]
  , [2, 0, 12, 3, 7]
  ]

exampleBoard3Str :: String
exampleBoard3Str = """14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
  2  0 12  3  7"""

exampleBoardStrings :: List.List String
exampleBoardStrings = List.fromFoldable
  [ """22 13 17 11  0
        8  2 23  4 24
        21  9 14 16  7
        6 10  3 18  5
        1 12 20 15 19"""
  , """3 15  0  2 22
        9 18 13 17  5
        19  8  7 25 23
        20 11 10 24  4
        14 21 16 12  6"""
  , exampleBoard3Str
  ]

allExampleBingoNumbers :: String
allExampleBingoNumbers = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"

winningMarked :: Marked
winningMarked = Set.fromFoldable [7,4,9,5,11,17,23,2,0,14,21,24]

notWinningMarked :: Marked
notWinningMarked = Set.fromFoldable [7,4,9,5,11,17,23,2,0,14,21]

testBingo :: Effect Unit
testBingo = do
  log $ "Example 1 Not Bingo: "
    <> showResult (not (bingo winningMarked exampleBoard1))
  log $ "Example 2 Not Bingo: "
    <> showResult (not (bingo winningMarked exampleBoard2))
  log $ "Example 3 Bingo: "
    <> showResult (bingo winningMarked exampleBoard3)
  log $ "Example 3 Not Bingo: "
    <> showResult (not (bingo notWinningMarked exampleBoard3))

testBoardSum :: Effect Unit
testBoardSum = do
  log $ "Example Board Sum: "
    <> showResult (boardSum winningMarked exampleBoard3 == 188)

testParseBoard :: Effect Unit
testParseBoard = do
  log $ "Parse Board: "
    <> showResult (parseBoard exampleBoard3Str == Just exampleBoard3)

testDay4 :: Effect Unit
testDay4 = do
  log $ "Day 4 Example: "
    <> showResult (day4 exampleBoardStrings allExampleBingoNumbers == Just 4512)

allDay4Tests :: Effect Unit
allDay4Tests = do
  testBingo
  testBoardSum
  testParseBoard
  testDay4
