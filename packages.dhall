let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220513/packages.dhall
        sha256:1ed784f37ae6131d99acd542d058d5ce39954ccaacc3adba5cc7cf1549d2bffa

let overrides = {=}

let additions = {
      , format =
        { dependencies =
          [ "control"
          , "effect"
          , "integers"
          , "maybe"
          , "numbers"
          , "prelude"
          , "strings"
          , "test-unit"
          , "unfoldable"
          ]
        , repo = "https://github.com/feramhq/purescript-format"
        , version = "3b0fc6c74f439fc3ad7fc7c88fc6f73b083f82e2"
        }
      , yaml-next =
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
        , version = "cd4833a32458e06ebb4338c3e00f98723c681891"
        }
      }

in  upstream // overrides // additions
