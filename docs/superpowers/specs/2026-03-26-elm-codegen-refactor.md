# ElmCodeGen Refactor: String Concatenation to AST-Based Generation

## Problem

`packages/elm-review/src/JsonRender/Internal/ElmCodeGen.elm` generates Elm source code via string concatenation with manual indentation (e.g., `"                        |> Resolve.required ..."`). This is error-prone — one wrong space count produces invalid Elm that elm-review's fix silently generates, causing fix loops.

## Solution

Replace string concatenation with [`the-sett/elm-syntax-dsl`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/):

- **`Elm.CodeGen`** — DSL for building elm-syntax AST nodes (type aliases, function declarations, expressions, patterns)
- **`Elm.Pretty`** — renders AST to properly formatted Elm source, elm-format compatible

## Scope

Refactor all 8 public functions in `ElmCodeGen.elm`:

| Function | Generates |
|---|---|
| `propsTypeAlias` | `type alias CardProps = { title : String, ... }` |
| `propsDecoder` | `propsDecoder = Resolve.succeed CardProps \|> ...` |
| `bindingsTypeAlias` | `type alias CardBindings = { ... }` |
| `bindingsDecoder` | `bindingsDecoder = Bind.succeed CardBindings \|> ...` |
| `componentModule` | Full component module (imports, types, decoders, view stub) |
| `actionsModule` | Action type + `decodeAction` function with nested case matching |
| `registryModule` | Registry record with components Dict + functions |
| `functionsModule` | Params types, Functions record, `()` stubs, `toFunctionDict` |

Plus the private helpers: `actionParamsType`, `actionType`, `decodeActionFunction`, `functionParamsType`, `functionRecordType`, `functionRecordValue`, `toFunctionDict`, `toFunctionDictEntry`, `nestedParamDecoding`.

## Example

Before (string concatenation):
```elm
"type alias " ++ componentName ++ "Props =\n" ++ body
```

After (AST-based):
```elm
aliasDecl Nothing (componentName ++ "Props") [] (recordAnn fields)
    |> Elm.Pretty.prettyDeclaration 120
    |> Pretty.pretty 120
```

Pipeline expressions that caused the indentation bug:
```elm
-- Before: manual space counting
"                        |> Resolve.required \"" ++ pName ++ "\" " ++ extractor

-- After: structural
pipe (apply [ fqFun [ "Resolve" ] "succeed", val capitalName ])
    [ apply [ fqFun [ "Resolve" ] "required", string pName, val extractor ] ]
```

## Dependencies

Add to `packages/elm-review/elm.json`:
```json
"the-sett/elm-syntax-dsl": "6.0.0 <= v < 7.0.0"
```

This transitively depends on `the-sett/elm-pretty-printer` for rendering.

## Approach

1. Add dependency
2. Create new implementations function-by-function, starting with the simplest (`propsTypeAlias`)
3. Each function: write the AST version, verify the output matches the string version via tests, swap
4. Delete all manual indentation helpers (`indent`, `nestedParamDecoding` string builder)
5. Tests may need updating if formatting differs slightly (elm-format canonical style vs hand-crafted strings)

Behavior does not change — only the implementation. All existing CatalogSync tests validate the output.
