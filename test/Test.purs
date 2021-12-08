module Test where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)

showResult :: Boolean -> String
showResult result = if result then "OK" else "WRONG"

justOrDie :: forall a. Maybe a -> Effect a
justOrDie x = case x of
  Just x -> pure x
  Nothing -> throw "Expected Just!"
