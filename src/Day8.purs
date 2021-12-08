module Day8 where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Enum (fromEnum)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Day4 (readWords)

type Index = Int
type Unknown = Array (Set.Set Index)
type Example = { examples :: Unknown, tests :: Unknown }

type Solved2 = { c :: Index, f :: Index }
type Solved3 = { a :: Index, c :: Index, f :: Index }
type Solved5 = { a :: Index, b :: Index, c :: Index, d :: Index, f :: Index }
type Solved6 = { a :: Index, b :: Index, c :: Index, d :: Index, f :: Index, g :: Index }
type Solved7 = { a :: Index, b :: Index, c :: Index, d :: Index, e :: Index, f :: Index, g :: Index }

readIndexs :: String -> Set.Set Index
readIndexs = Set.fromFoldable
  <<< map (_ - fromEnum (String.codePointFromChar 'a'))
  <<< map fromEnum
  <<< String.toCodePointArray

parseExample :: String -> Maybe Example
parseExample inputStr = do
  let ps = map (map readIndexs <<< readWords) $ String.split (String.Pattern "|") inputStr
  case ps of
    [ex, te] -> Just { examples: Array.fromFoldable ex, tests: Array.fromFoldable te }
    _ -> Nothing

solveOne :: Unknown -> List.List Solved2
solveOne inputs = do
  input <- List.fromFoldable inputs
  guard (Set.size input == 2)
  c <- List.fromFoldable input
  f <- List.fromFoldable input
  guard (c /= f)
  pure { c, f }

solveSeven :: Unknown -> Solved2 -> List.List Solved3
solveSeven inputs { c, f } = do
  input <- List.fromFoldable inputs
  guard (Set.size input == 3)
  guard (Set.subset (Set.fromFoldable [c, f]) input) -- always true
  a <- List.fromFoldable input
  guard (Array.notElem a [c, f])
  pure { a, c, f }

solveFour :: Unknown -> Solved3 -> List.List Solved5
solveFour inputs { a, c, f } = do
  input <- List.fromFoldable inputs
  guard (Set.size input == 4)
  guard (Set.subset (Set.fromFoldable [c, f]) input)
  b <- List.fromFoldable input
  guard (Array.notElem b [a, c, f])
  d <- List.fromFoldable input
  guard (Array.notElem d [a, b, c, f])
  pure { a, b, c, d, f }

solveThree :: Unknown -> Solved5 -> List.List Solved6
solveThree inputs { a, b, c, d, f } = do
  input <- List.fromFoldable inputs
  guard (Set.size input == 5)
  guard (Set.subset (Set.fromFoldable [a, c, d, f]) input)
  g <- List.fromFoldable input
  guard (Array.notElem g [a, b, c, d, f])
  pure { a, b, c, d, f, g }

solveTwo :: Unknown -> Solved6 -> List.List Solved7
solveTwo inputs { a, b, c, d, f, g } = do
  input <- List.fromFoldable inputs
  guard (Set.size input == 5)
  guard (Set.subset (Set.fromFoldable [a, c, d, g]) input)
  e <- List.fromFoldable input
  guard (Array.notElem e [a, b, c, d, f, g])
  pure { a, b, c, d, e, f, g }

solveFive :: Unknown -> Solved7 -> List.List Solved7
solveFive inputs { a, b, c, d, e, f, g } = do
  input <- List.fromFoldable inputs
  guard (Set.size input == 5)
  guard (input == Set.fromFoldable [a, b, d, f, g])
  pure { a, b, c, d, e, f, g }

solveSix :: Unknown -> Solved7 -> List.List Solved7
solveSix inputs { a, b, c, d, e, f, g } = do
  input <- List.fromFoldable inputs
  guard (Set.size input == 6)
  guard (input == Set.fromFoldable [a, b, d, e, f, g])
  pure { a, b, c, d, e, f, g }

solve' :: Unknown -> List.List Solved7
solve' examples = solveOne examples
  >>= solveSeven examples
  >>= solveFour examples
  >>= solveThree examples
  >>= solveTwo examples
  >>= solveFive examples
  >>= solveSix examples

solve :: Unknown -> Maybe Solved7
solve examples = case Array.fromFoldable (solve' examples) of
  [solution] -> Just solution
  _ -> Nothing

digit :: Solved7 -> Set.Set Index -> Maybe Int
digit { a, b, c, d, e, f, g } dig
  | dig == Set.fromFoldable [c, f] = Just 1
  | dig == Set.fromFoldable [a, c, d, e, g] = Just 2
  | dig == Set.fromFoldable [a, c, d, f, g] = Just 3
  | dig == Set.fromFoldable [b, c, d, f] = Just 4
  | dig == Set.fromFoldable [a, b, d, f, g] = Just 5
  | dig == Set.fromFoldable [a, b, d, e, f, g] = Just 6
  | dig == Set.fromFoldable [a, c, f] = Just 7
  | dig == Set.fromFoldable [a, b, c, d, e, f, g] = Just 8
  | dig == Set.fromFoldable [a, b, c, d, f, g] = Just 9
  | dig == Set.fromFoldable [a, b, c, e, f, g] = Just 0
  | otherwise = Nothing
