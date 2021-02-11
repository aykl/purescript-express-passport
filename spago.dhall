{ name = "express-passport"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "maybe"
  , "either"
  , "express"
  , "spec"
  , "argonaut-codecs"
  , "foreign"
  , "newtype"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
