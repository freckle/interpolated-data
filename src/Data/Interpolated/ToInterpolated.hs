{-# LANGUAGE FlexibleInstances #-}

module Data.Interpolated.ToInterpolated
  ( ToInterpolated (..)
  ) where

import Prelude

import Data.Interpolated.Parser
import Data.Set (Set)
import Data.Text (Text, pack, unpack)

class ToInterpolated a where
  getVariables :: a -> Set Text
  runReplacement :: (Text -> Text) -> a -> a

instance ToInterpolated Text where
  getVariables = parseInterpolatedText
  runReplacement = id

instance ToInterpolated String where
  getVariables = getVariables . pack
  runReplacement f = unpack . f . pack
