{-# LANGUAGE DeriveFunctor #-}
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
import Data.Foldable (toList, traverse_)
import Data.Hashable (Hashable)
import Data.Interpolated.Error
import Data.Interpolated.InterpolationContext
import Data.Interpolated.ToInterpolated
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Semigroup (Endo (..))
import Data.Set ((\\))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T

newtype InterpolatedBy a context = Interpolated
  { unInterpolated :: a
  }
  deriving stock (Show, Functor)
  deriving newtype (Eq, Ord, Hashable, ToJSON)

instance ToInterpolated a => ToInterpolated (a `InterpolatedBy` context) where
  parseVariables = parseVariables . unInterpolated
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
toInterpolated a = do
  want <- parseVariables a
  let mMissing = NE.nonEmpty $ toList $ want \\ have
  Interpolated a <$ traverse_ (Left . errorMessage) mMissing
 where
  have = interpolationVariables $ Proxy @context
  errorMessage = interpolatedErrorMessage . UnexpectedVariables have

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
