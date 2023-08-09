# interpolated-data

[![Hackage](https://img.shields.io/hackage/v/interpolated-data.svg?style=flat)](https://hackage.haskell.org/package/interpolated-data)
[![Stackage Nightly](http://stackage.org/package/interpolated-data/badge/nightly)](http://stackage.org/nightly/package/interpolated-data)
[![Stackage LTS](http://stackage.org/package/interpolated-data/badge/lts)](http://stackage.org/lts/package/interpolated-data)
[![CI](https://github.com/freckle/interpolated-data/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/interpolated-data/actions/workflows/ci.yml)

:warning: **Work in progress**

<!--
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Prelude

import Data.Aeson (FromJSON)
import Data.Bifunctor (first)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Text.Markdown.Unlit ()
```
-->

There are [many, many interpolation libraries][hackage-search] on Hackage, but
they are exclusively:

1. For building interpolated strings at compile-time, through quasi-quotation
2. For building only string (or string-like) types

This library is different. It aims to better support cases where:

1. The interpolated data may be provided at runtime, such as from a
   configuration file or web request.
2. The interpolated data is structured, such as a record of fields of
   interpolated data
3. You can state statically in the types what interpolation keys (called a
   "context") will be available, so it we validate the user-input

Let's build a motivating example.

```haskell
import Data.Interpolated
```

Let's say you're building some deployment tooling. Applications can describe
some settings in a configuration file. Deploys occur for some App in an
Environment, and it we want to support interpolating those (and only those)
runtime values into such settings.

An example configuration would look like:

```yaml
stackName: "{env}-{app}"

ecrRepository:
  registry: "my-registry"
  name: "apps/{app}"

dockerfile: "./{app}.dockerfile"
```

The `stackName` key supports the `env` and `app` interpolations, while the
fields of `ecrRepository` support only `app` (deployment images are not
env-specific, of course).

To eventually supply values for these interpolations we have to define types
that are instances of `InterpolationContext`. These can be derived
generically\*, but we will define them by hand here, so you can see how it
works.

<!--
```haskell
newtype AppName = AppName
  { unAppName :: Text
  }
  deriving newtype IsString

newtype Environment = Environment
  { unEnvironment :: Text
  }
  deriving newtype IsString
```
-->

```haskell
data AppEnvContext = AppEnvContext
  { app :: AppName
  , env :: Environment
  }

instance InterpolationContext AppEnvContext where
  -- interpolationVariables :: Proxy context -> Set Text
  interpolationVariables _ = Set.fromList ["app", "env"]

  -- interpolationValues :: context -> [(Text, Text)]
  interpolationValues AppEnvContext {..} =
    [ ("app", unAppName app)
    , ("env", unEnvironment env)
    ]

newtype AppContext = AppContext
  { app :: AppName
  }

instance InterpolationContext AppContext where
  interpolationVariables _ = Set.singleton "app"
  interpolationValues AppContext {..} = [("app", unAppName app)]
```

Next, we need to make types "interpolate-able" by giving them instances of
`AsInterpolated`. There is exists instances for string-like texts, so any
newtypes can derive this instance:

```haskell
newtype StackName = StackName Text
  deriving stock Show
  deriving newtype (FromJSON, AsInterpolated)

newtype EcrRegistry = EcrRegistry Text
  deriving stock Show
  deriving newtype (FromJSON, AsInterpolated)

newtype Dockerfile = Dockerfile FilePath
  deriving stock Show
  deriving newtype (FromJSON, AsInterpolated)
```

Our `EcrRepository` can also be interpolated, which would work as you might
imagine, by interpolating each field. Such records could also derive
`AsInterpolated` generically\*, but again we'll define it by hand so you can see
how it works:

```haskell
data EcrRepository = EcrRepository
  { registry :: EcrRegistry
  , name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON
```

```haskell
instance AsInterpolated EcrRepository where
  -- interpolatedVariables :: a -> Set Text
  interpolatedVariables EcrRepository {..} = mconcat
    [ interpolatedVariables registry
    , interpolatedVariables name
    ]

  -- runInterpolation :: (Text -> Text) -> a -> a
  runInterpolation f er = er
    { registry = runInterpolation f $ registry er
    , name = runInterpolation f $ name er
    }
```

When we wrap an interpolate-able type (one with `AsInterpolated`) with the
concrete `Interpolated` constructor, we must say what interpolated values it
supports by providing a phantom variable for the `context` too.

When we construct such a value, we will verify it only uses interpolations
provided by the `context`. Conveniently, construction via generic `FromJSON`
will include this validation:

```haskell
data Settings = Settings
  { stackName :: Interpolated AppEnvContext StackName
  , repository :: Interpolated AppContext EcrRepository
  , dockerfile :: Maybe (Interpolated AppContext Dockerfile)
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON
```

When we parse some Yaml, values containing an invalid variable will throw an
informative error:

```haskell
example1 :: IO (Either String Settings)
example1 =
  first Yaml.prettyPrintParseException
    <$> Yaml.decodeFileEither "files/invalid.yaml"
```

```console
λ> either putStrLn print =<< example1
Error in $.stackName: Interpolation uses the variable region, which is not available in the provided context (app, env)
```

Even valid values can't use the wrong `context` without that being a type-error:

```hs
example2 :: IO StackName
example2 = do
  Settings {..} <- Yaml.decodeFileThrow "files/valid.yaml"
  pure $ interpolate (AppContext {app="my-app"}) stackName
```

```
    • Couldn't match type ‘AppEnvContext’ with ‘AppContext’
      Expected: Interpolated AppContext StackName
        Actual: Interpolated AppEnvContext StackName
    • In the second argument of ‘interpolate’, namely ‘stackName’
      In the second argument of ‘($)’, namely
        ‘interpolate (AppContext {app = "my-app"}) stackName’
      In a stmt of a 'do' block:
        pure $ interpolate (AppContext {app = "my-app"}) stackName
    |
199 |   pure $ interpolate (AppContext {app="my-app"}) stackName
    |
```

But providing the right context works as expected:

```haskell
example3 :: IO StackName
example3 = do
  Settings {..} <- Yaml.decodeFileThrow "files/valid.yaml"
  pure $ interpolate (AppEnvContext {app="my-app", env="prod"}) stackName
```

```haskell
example4 :: IO EcrRepository
example4 = do
  Settings {..} <- Yaml.decodeFileThrow "files/valid.yaml"
  pure $ interpolate (AppContext {app="my-app"}) repository
```

```console
λ> print =<< example3
StackName "my-app-prod"
```

```console
λ> print =<< example4
EcrRepository {registry = EcrRegistry "hub.docker.io", name = "apps/my-app"}
```

\*These generic deriving features are coming soon ;)

<!--
```haskell
main :: IO ()
main = do
  either putStrLn print =<< example1
  print =<< example3
  print =<< example4
```
-->

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
