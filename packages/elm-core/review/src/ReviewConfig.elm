module ReviewConfig exposing (config)

import NoUnused.Variables
import NoUnoptimizedRecursion
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    ]
