module Data.Interpolated.InterpolationContext
  ( InterpolationContext (..)
  ) where

import Data.Proxy (Proxy)
import Data.Set (Set)
import Data.Text (Text)

class InterpolationContext context where
  interpolationVariables :: Proxy context -> Set Text
  interpolationValues :: context -> [(Text, Text)]
