{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "cek"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "identity"
  , "integers"
  , "maybe"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
