module Data.Interpolated.Parser
  ( parseInterpolatedText
  ) where

import Prelude

import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

parseInterpolatedText :: Text -> Either String (Set Text)
parseInterpolatedText = Right . go
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
