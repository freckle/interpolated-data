{-# LANGUAGE FlexibleInstances #-}

module Data.Interpolated.ToInterpolated
  ( ToInterpolated (..)
  ) where

import Prelude

import Data.Interpolated.Parser
import Data.Set (Set)
import Data.Text (Text, pack, unpack)

class ToInterpolated a where
  parseVariables :: a -> Either String (Set Text)
  runReplacement :: (Text -> Text) -> a -> a

instance ToInterpolated Text where
  parseVariables = parseInterpolatedText
  runReplacement = id

instance ToInterpolated String where
  parseVariables = parseVariables . pack
  runReplacement f = unpack . f . pack
