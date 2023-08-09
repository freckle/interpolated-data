{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Interpolated
  ( InterpolationContext (..)
  , ToInterpolated (..)
  , InterpolatedBy
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

class ToInterpolated a where
  getVariables :: a -> Set Text
  runReplacement :: (Text -> Text) -> a -> a

instance ToInterpolated Text where
  getVariables = go
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
  runReplacement = id

instance ToInterpolated String where
  getVariables = getVariables . pack
  runReplacement f = unpack . f . pack

newtype InterpolatedBy a context = Interpolated
  { unInterpolated :: a
  }
  deriving stock (Show, Functor)
  deriving newtype (Eq, Ord, Hashable, ToJSON)

instance ToInterpolated a => ToInterpolated (a `InterpolatedBy` context) where
  getVariables = getVariables . unInterpolated
  runReplacement f = Interpolated . runReplacement f . unInterpolated

instance
  (InterpolationContext context, ToInterpolated a, FromJSON a)
  => FromJSON (a `InterpolatedBy` context)
  where
  parseJSON = either fail pure . toInterpolated <=< parseJSON

instance
  (InterpolationContext context, ToInterpolated a, FromJSON a)
  => FromJSONKey (a `InterpolatedBy` context)
  where
  fromJSONKey = FromJSONKeyValue parseJSON

instance
  (InterpolationContext context, ToInterpolated a, IsString a)
  => IsString (a `InterpolatedBy` context)
  where
  fromString = either error id . toInterpolated . fromString

toInterpolated
  :: forall context a
   . (InterpolationContext context, ToInterpolated a)
  => a
  -> Either String (a `InterpolatedBy` context)
toInterpolated a =
  maybe (Right $ Interpolated a) (Left . errorMessage) $
    NE.nonEmpty $
      toList $
        want
          \\ have
 where
  have = interpolationVariables $ Proxy @context
  want = getVariables a
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
  :: (InterpolationContext context, ToInterpolated a)
  => context
  -> a `InterpolatedBy` context
  -> a
interpolate ic = runReplacement go . unInterpolated
 where
  go :: Text -> Text
  go =
    appEndo $
      foldMap (Endo . uncurry replace) $
        interpolationValues ic

  replace :: Text -> Text -> Text -> Text
  replace k = T.replace ("{" <> k <> "}")
