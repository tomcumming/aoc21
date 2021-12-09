module Day9 where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.List.Lazy as List
import Data.Maybe (Maybe)

type HeightMap = Array (Array Int)
type Coord = { x :: Int, y :: Int }

lookup :: HeightMap -> Coord -> Maybe Int
lookup hm { x, y } = hm Array.!! y >>= (_ Array.!! x)

lowPoints :: HeightMap -> List.List Coord
lowPoints hm = do
  width <- List.fromFoldable $ maximum $ map Array.length hm
  y <- List.range 0 (Array.length hm)
  x <- List.range 0 width

  height <- List.fromFoldable $ lookup hm { x, y }

  let above = lookup hm { x, y: (y - 1) }
  let below = lookup hm { x, y: (y + 1) }
  let left = lookup hm { x: (x - 1), y }
  let right = lookup hm { x: (x + 1), y }
  let neighbours = Array.catMaybes [ above, below, left, right ]

  guard (Array.all (_ > height) neighbours)

  pure { x, y }

riskLevel :: HeightMap -> Coord -> Maybe Int
riskLevel hm c = (_ + 1) <$> lookup hm c