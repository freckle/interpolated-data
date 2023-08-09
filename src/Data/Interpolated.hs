{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Interpolated
  ( InterpolationContext (..)
  , AsInterpolated (..)
  , Interpolated
  , interpolate
  )
where

import Prelude

import Control.Monad ((<=<))
import Data.Aeson
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Semigroup (Endo (..))
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

class InterpolationContext context where
  interpolationVariables :: Proxy context -> Set Text
  interpolationValues :: context -> [(Text, Text)]

class AsInterpolated a where
  interpolatedVariables :: a -> Set Text
  runInterpolation :: (Text -> Text) -> a -> a

instance AsInterpolated Text where
  interpolatedVariables = go
   where
    go x
      | T.null x = Set.empty
      | otherwise =
          x -- "...-{foo}-{bar}-..."
            & T.dropWhile (/= '{') -- "{foo}-{bar}-..."
            & T.drop 1 --  "foo}-{bar}-..."
            & T.breakOn "}" -- ("foo", "}-{bar}-...")
            & second (go . T.drop 1) -- ("foo", Set{bar,...})
            & \(var, rest) ->
              if T.null var
                then rest -- likely empty too
                else Set.insert var rest -- Set{foo,bar,...}
  runInterpolation = id

instance AsInterpolated String where
  interpolatedVariables = interpolatedVariables . pack
  runInterpolation f = unpack . f . pack

newtype Interpolated context a = Interpolated
  { unInterpolated :: a
  }
  deriving stock (Show, Functor)
  deriving newtype (Eq, Ord, Hashable, ToJSON)

instance
  (InterpolationContext context, AsInterpolated a, FromJSON a)
  => FromJSON (Interpolated context a)
  where
  parseJSON = either fail pure . toInterpolated <=< parseJSON

instance
  (InterpolationContext context, AsInterpolated a, FromJSON a)
  => FromJSONKey (Interpolated context a)
  where
  fromJSONKey = FromJSONKeyValue parseJSON

instance
  (InterpolationContext context, AsInterpolated a, IsString a)
  => IsString (Interpolated context a)
  where
  fromString = either error id . toInterpolated . fromString

toInterpolated
  :: forall context a
   . (InterpolationContext context, AsInterpolated a)
  => a
  -> Either String (Interpolated context a)
toInterpolated a =
  maybe (Right $ Interpolated a) (Left . errorMessage) $
    NE.nonEmpty $
      toList $
        want
          \\ have
 where
  have = interpolationVariables $ Proxy @context
  want = interpolatedVariables a
  errorMessage missing =
    unpack $
      "Interpolation uses "
        <> theVariablesWhichAre missing
        <> " not available in the provided context ("
        <> T.intercalate ", " (toList have)
        <> ")"

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

interpolate
  :: (InterpolationContext context, AsInterpolated a)
  => context
  -> Interpolated context a
  -> a
interpolate ic = runInterpolation go . unInterpolated
 where
  go :: Text -> Text
  go =
    appEndo $
      foldMap (Endo . uncurry replace) $
        interpolationValues ic

  replace :: Text -> Text -> Text -> Text
  replace k = T.replace ("{" <> k <> "}")
