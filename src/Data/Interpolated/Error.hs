module Data.Interpolated.Error
  ( InterpolatedError (..)
  , interpolatedErrorMessage
  )
where

import Prelude

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Text (Text, unpack)
import qualified Data.Text as T

data InterpolatedError
  = UnexpectedVariables (Set Text) (NonEmpty Text)

interpolatedErrorMessage :: InterpolatedError -> String
interpolatedErrorMessage = \case
  UnexpectedVariables have missing ->
    unpack $
      "Interpolation uses "
        <> theVariablesWhichAre missing
        <> " not available in the provided context ("
        <> T.intercalate ", " (toList have)
        <> ")"

theVariablesWhichAre :: NonEmpty Text -> Text
theVariablesWhichAre = \case
  (x :| []) -> "the variable " <> x <> ", which is"
  ne -> "the variables " <> commaAnd ne <> ", which are"

-- https://codegolf.stackexchange.com/a/37725
commaAnd :: NonEmpty Text -> Text
commaAnd = \case
  (x :| []) -> x
  (x :| [y]) -> x <> " and " <> y
  (x :| [y, z]) -> x <> ", " <> commaAnd (y <> "," :| [z])
  (x :| (y : ys)) -> x <> ", " <> commaAnd (y :| ys)
