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
{-# LANGUAGE TypeOperators #-}

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

[hackage-search]: https://hackage.haskell.org/packages/search?terms=interpolate

1. For building interpolated strings at compile-time, through quasi-quotation
2. For building only string (or string-like) types

This library is different. It aims to better support cases where:

1. The interpolated data may be provided at runtime, such as from a
   configuration file or web request
2. The interpolated data is structured, such as a record of fields of
   interpolated data
3. You can state statically in the types what interpolation keys (called a
   "context") will be available, so we can validate the runtime input

Let's build a motivating example.

```haskell
import Data.Interpolated
```

Let's say you're building some deployment tooling. Applications can describe
some settings about how they're deployed in a configuration file. Deploys occur
for some *App* and in an *Environment*. Therefore, we want to support
interpolating those (and only those) runtime values into such settings.

An example configuration would look like:

```yaml
stackName: "{env}-{app}"

repository:
  registry: "my-registry"
  name: "apps/{app}"

dockerfile: "./{app}.dockerfile"
```

The `stackName` key supports the `env` and `app` interpolations, while the
fields of `repository` support only `app` (deployment images are reused from
`env` to `env`, of course). One key feature of our library is making that safe
through a combination of compile- and runtime validations.

To eventually supply values for these interpolations, we have to define types
that are instances of `InterpolationContext`:

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
  interpolationVariables _ = Set.fromList ["app", "env"]
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
`ToInterpolated`. There exists instances for string-like types, so any newtypes
can derive this instance:

```haskell
newtype StackName = StackName Text
  deriving stock Show
  deriving newtype (FromJSON, ToInterpolated)

newtype EcrRegistry = EcrRegistry Text
  deriving stock Show
  deriving newtype (FromJSON, ToInterpolated)

newtype Dockerfile = Dockerfile FilePath
  deriving stock Show
  deriving newtype (FromJSON, ToInterpolated)
```

Our `EcrRepository` can also be interpolated, which would work as you might
imagine (interpolating each of its fields). Such records could also derive
`ToInterpolated` generically\*, but again we'll define it by hand so you can see
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
instance ToInterpolated EcrRepository where
  getVariables EcrRepository {..} = mconcat
    [ getVariables registry
    , getVariables name
    ]

  runReplacement f er = er
    { registry = runReplacement f $ registry er
    , name = runReplacement f $ name er
    }
```

When we wrap an interpolate-able type (one with `ToInterpolated`) with the
concrete `InterpolatedBy` constructor, we must say what interpolated values it
wants by providing a phantom variable for the `context` too.

When we construct such a value, we will verify it only uses interpolations
provided by the `context`. Conveniently, construction via generic `FromJSON`
will include this validation:

```haskell
data Settings = Settings
  { stackName :: StackName `InterpolatedBy` AppEnvContext
  , repository :: EcrRepository `InterpolatedBy` AppContext
  , dockerfile :: Maybe (Dockerfile `InterpolatedBy` AppContext)
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON
```

Therefore, when we parse some Yaml, values containing an invalid variable will
throw an informative error:

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
      Expected: InterpolatedBy StackName AppContext
        Actual: InterpolatedBy StackName AppEnvContext
    • In the second argument of ‘interpolate’, namely ‘stackName’
      In the second argument of ‘($)’, namely
        ‘interpolate (AppContext {app = "my-app"}) stackName’
      In a stmt of a 'do' block:
        pure $ interpolate (AppContext {app = "my-app"}) stackName
    |
201 |   pure $ interpolate (AppContext {app="my-app"}) stackName
    |
```

But providing the right context works as expected:

```haskell
example3 :: IO StackName
example3 = do
  Settings {..} <- Yaml.decodeFileThrow "files/valid.yaml"
  pure $ interpolate (AppEnvContext {app="my-app", env="prod"}) stackName
```

```console
λ> print =<< example3
StackName "my-app-prod"
```

```haskell
example4 :: IO EcrRepository
example4 = do
  Settings {..} <- Yaml.decodeFileThrow "files/valid.yaml"
  pure $ interpolate (AppContext {app="my-app"}) repository
```

```console
λ> print =<< example4
EcrRepository {registry = EcrRegistry "hub.docker.io", name = "apps/my-app"}
```

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
