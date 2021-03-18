let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210317/packages.dhall sha256:e2e744972f9b60188dcf07f41418661b505c9ee2e9f91e57e67daefad3a5ae09

let overrides =
      { yaml-next = ../purescript-yaml-next/spago.dhall as Location}

let additions = {=}

in  upstream // overrides // additions
