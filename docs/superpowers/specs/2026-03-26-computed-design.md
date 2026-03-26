# `$computed` Expression Design

## Overview

`$computed` adds registered transformation functions to the expression system. An LLM writes `{ "$computed": "lineTotal", "args": { ... } }` in a spec; the Elm app resolves it by calling a developer-defined, type-safe function.

The catalog schema is the source of truth. elm-review generates typed Elm code from it. The developer fills in function bodies with full compiler guidance.

## Design Principle

Per the core spec, expressions compose uniformly. `$computed` args can contain any expression type (`$state`, `$item`, `$index`, `$template`, `$bindState`, `$bindItem`, `$cond`, nested `$computed`).

## Wire Format & Decoding

JSON wire format:

```json
{
  "$computed": "lineTotal",
  "args": {
    "price": { "$item": "price" },
    "quantity": { "$item": "quantity" }
  }
}
```

New `PropValue` variant:

```elm
type PropValue
    = ...existing variants...
    | ComputedExpr String (Dict String PropValue)
```

- `"$computed"` key triggers the decoder, value is the function name (String)
- `"args"` decoded as `Dict String PropValue` (recursive — args can contain any expression)
- Missing `"args"` defaults to `Dict.empty` (zero-arg functions are valid)

## Resolution

### FunctionDict

Defined in `JsonRender.Resolve`:

```elm
type alias FunctionDict =
    Dict String (Dict String ResolvedValue -> ResolvedValue)
```

Threaded as the first parameter through `resolvePropValue`, `resolveProps`, `resolveActionParams`, and `resolveActionParamValue`. The `Visibility` module also needs it since visibility conditions can contain `$computed` expressions.

### RError Variant

`ResolvedValue` gains an error variant for visible error propagation:

```elm
type ResolvedValue
    = ...existing variants...
    | RError String
```

`RError` propagates through:

- **Pipeline decoders** (`string`, `int`, `float`, `bool`) — `RError err -> Err err`, surfacing in the existing red error box via `register`
- **`isResolvedTruthy`** — returns `Result String Bool`. `RError err -> Err err`.
- **`ConditionalExpr`** — if condition is `RError`, the whole expression becomes that `RError`
- **Visibility** — `Visibility.evaluate` returns `Result String Bool`. `Err err` renders the red error box in place of the element.

### ComputedExpr Resolution

```elm
ComputedExpr name args ->
    let
        resolvedArgs =
            Dict.map (\_ v -> resolvePropValue functions state repeatCtx v) args
    in
    case findError resolvedArgs of
        Just err ->
            err

        Nothing ->
            case Dict.get name functions of
                Just fn ->
                    fn resolvedArgs

                Nothing ->
                    RError ("Unknown function: " ++ name)
```

If any arg resolves to `RError`, propagate it without calling the function.

### Action Params

Per the core spec, `resolveActionParam` only special-cases bare `$item` and `$index`. `$computed` delegates to regular `resolvePropValue` — args always use value semantics regardless of context.

```elm
ComputedExpr name args ->
    resolvePropValue functions state repeatCtx (ComputedExpr name args)
```

## Registry Changes

`Registry` becomes a record:

```elm
type alias Registry msg =
    { components : Dict String (Component msg)
    , functions : Dict String (Dict String ResolvedValue -> ResolvedValue)
    }
```

`render` destructures `registry.components` for component lookup and passes `registry.functions` into the resolution pipeline.

## Schema & Catalog Definition

The schema gains a `functions` field:

```typescript
catalog: s.object({
  components: s.map({ ... }),
  actions: s.map({ ... }),
  functions: s.map({
    params: s.zod(),
    returnType: s.zod(),
    description: s.string(),
  }),
}),
```

Catalog usage:

```typescript
export const catalog = defineCatalog(schema, {
  components: { ... },
  actions: { ... },
  functions: {
    lineTotal: {
      params: z.object({ price: z.number(), quantity: z.number() }),
      returnType: z.number(),
      description: "Multiply price by quantity",
    },
    formatCurrency: {
      params: z.object({ amount: z.number(), locale: z.string().optional() }),
      returnType: z.string(),
      description: "Format a number as currency",
    },
  },
});
```

`$computed` is added to `defaultRules` so the LLM knows the expression is available.

## elm-review Code Generation

CatalogSync generates a `Functions.elm` module with:

### Params type aliases

One per function:

```elm
type alias LineTotalParams =
    { price : Float
    , quantity : Float
    }
```

### Functions record type

```elm
type alias Functions =
    { lineTotal : LineTotalParams -> Float
    , formatCurrency : FormatCurrencyParams -> String
    }
```

### Functions value with () placeholders

Compiler-driven development — type errors guide the developer:

```elm
functions : Functions
functions =
    { lineTotal = ()
    , formatCurrency = ()
    }
```

### toFunctionDict converter

Converts the typed record to the runtime Dict. Uses `Resolve.succeed/required/optional` pipeline for arg validation with per-arg error messages:

```elm
toFunctionDict : Functions -> Dict String (Dict String ResolvedValue -> ResolvedValue)
toFunctionDict fns =
    Dict.fromList
        [ ( "lineTotal"
          , \args ->
                let
                    result =
                        Resolve.succeed LineTotalParams
                            |> Resolve.required "price" Resolve.float
                            |> Resolve.required "quantity" Resolve.float
                in
                case result args of
                    Ok params ->
                        RFloat (fns.lineTotal params)

                    Err err ->
                        RError ("lineTotal: " ++ err)
          )
        , ( "formatCurrency"
          , \args ->
                let
                    result =
                        Resolve.succeed FormatCurrencyParams
                            |> Resolve.required "amount" Resolve.float
                            |> Resolve.optional "locale" Resolve.string Nothing
                in
                case result args of
                    Ok params ->
                        RString (fns.formatCurrency params)

                    Err err ->
                        RError ("formatCurrency: " ++ err)
          )
        ]
```

### Registry.elm update

```elm
registry : Registry
registry =
    { components = Dict.fromList [ ... ]
    , functions = Functions.toFunctionDict Functions.functions
    }
```

## Known Limitations

- **`$computed` in visibility `eq`/`neq` values** — `Visibility.evaluate` uses `propValueMatchesJson` which compares PropValues literally, not by resolving them. A `$computed` (or `$state`) in an `eq` value won't be resolved. This is a pre-existing limitation — no expressions are evaluated in visibility comparison values today. Can be addressed in a future task if needed.

## Out of Scope

- **Memoization/caching** — functions called fresh each render, same as all expression resolution
- **Async functions** — functions are pure and synchronous
- **Function-level error handling** — arg validation in the generated wrapper catches type mismatches before the user's function is called
