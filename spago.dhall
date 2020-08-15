{ name = "transity"
, dependencies =
    [ "ansi"
    , "argonaut-codecs"
    , "bigints"
    , "console"
    , "debug"
    , "effect"
    , "either"
    , "format"
    , "formatters"
    , "js-date"
    , "node-buffer"
    , "node-fs"
    , "node-process"
    , "ordered-collections"
    , "prelude"
    , "psci-support"
    , "rationals"
    , "result"
    , "spec"
    , "strings"
    , "yaml-next"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "AGPL-3.0-or-later"
, repository = "https://github.com/feramhq/transity"
}
