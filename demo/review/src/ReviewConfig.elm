module ReviewConfig exposing (config)

{-| elm-review configuration.

Includes JsonRender.CatalogSync to keep component modules in sync
with the catalog defined in catalog.js. After changing the catalog, run:

    npm run catalog

to regenerate CatalogData.elm, then:

    npx elm-review --fix

to create/update component modules.

-}

import CatalogData
import Docs.ReviewAtDocs
import JsonRender.CatalogSync
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ JsonRender.CatalogSync.rule
        { schemaJson = CatalogData.schemaJson
        , componentsNamespace = "Components"
        }
    ]
