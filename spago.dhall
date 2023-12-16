{ name = "ps-0.0.1-hello-world"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-streams"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
