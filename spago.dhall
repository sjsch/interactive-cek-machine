{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "cek"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "identity"
  , "integers"
  , "maybe"
  , "parsing"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "tuples"
  , "unordered-collections"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
