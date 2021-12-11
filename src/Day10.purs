module Day10 where

import Prelude

import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

data BraceClass
  = Round
  | Square
  | Curly
  | Angle

data BraceType
  = Open
  | Close

data Token = Token BraceClass BraceType

derive instance eqBraceClass :: Eq BraceClass

tokenMap :: Map.Map String Token
tokenMap = Map.fromFoldable
  [ Tuple "(" (Token Round Open)
  , Tuple ")" (Token Round Close)
  , Tuple "[" (Token Square Open)
  , Tuple "]" (Token Square Close)
  , Tuple "{" (Token Curly Open)
  , Tuple "}" (Token Curly Close)
  , Tuple "<" (Token Angle Open)
  , Tuple ">" (Token Angle Close)
  ]

parseTokens :: String -> Maybe (Array Token)
parseTokens = sequence
  <<< map (\ch -> Map.lookup ch tokenMap)
  <<< map (String.fromCodePointArray <<< pure)
  <<< String.toCodePointArray
  <<< String.trim

firstIncorrect :: List.List Token -> Maybe BraceClass
firstIncorrect = go List.nil
  where
  go :: List.List BraceClass -> List.List Token -> Maybe BraceClass
  go stack ts = case List.uncons ts of
    Nothing -> Nothing
    Just { head: Token c Open, tail } -> go (List.cons c stack) tail
    Just { head: Token c Close, tail } -> case List.uncons stack of
      Nothing -> Just c
      Just { head: shead, tail: stail } ->
        if shead == c then go stail tail else Just c

score :: BraceClass -> Int
score c = case c of
  Round -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137
