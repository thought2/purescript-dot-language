{ name = "dot-language"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "prelude"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "undefined"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "examples/**/*.purs" ]
}
