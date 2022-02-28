let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220224/packages.dhall sha256:67cc3d4f0e8fb72bb1413ba94ddd72a3ceb0783eb725e3b22ad7568b3b581163

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
