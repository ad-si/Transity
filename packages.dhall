let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

let overrides = {=}

let additions =
      { yaml-next =
        { dependencies =
          [ "argonaut-codecs"
          , "argonaut-core"
          , "console"
          , "effect"
          , "foreign"
          , "functions"
          , "ordered-collections"
          , "psci-support"
          , "spec"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/archaeron/purescript-yaml-next"
        , version = "v3.0.0"
        }
      }

in  upstream // overrides // additions
