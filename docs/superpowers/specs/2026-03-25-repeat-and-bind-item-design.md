# Repeat & $bindItem Design Spec

## Overview

Add `repeat` field to elements and `$bindItem` expression to the Elm renderer, enabling iteration over state arrays with two-way binding to individual array items. This brings the Elm renderer to parity with the React and Vue renderers for list rendering.

## Decisions

- **Index-based paths** for `$bindItem` setters (e.g., `SetState "/todos/1/completed" value`)
- **Keyed wrapper** with `Html.Keyed.node "div"` using inline `display: contents` style — one wrapper div per repeat, no per-iteration wrappers
- **Nested repeats** supported — inner context shadows outer
- **Empty string** supported for both `$item ""` and `$bindItem ""` — returns/binds to whole item
- **`key` is optional** (`Maybe String`) — falls back to `String.fromInt index`, matching upstream React/Vue behavior
- **Bottom-up implementation** — types/decoding first, then resolution, then rendering

## Wire Format

### `repeat` on elements

```json
{
  "type": "Stack",
  "props": { "direction": "vertical" },
  "children": ["todoItem"],
  "repeat": {
    "statePath": "/todos",
    "key": "id"
  }
}
```

- `statePath` (required): JSON Pointer to a state array
- `key` (optional): field name on each item for stable keyed rendering

### `$bindItem` expression

```json
{ "$bindItem": "completed" }
```

- Value is a field name on the current repeat item (or empty string for the whole item)
- Resolves to the current value (read) and provides a setter (write) via bindings
- Setter targets `${basePath}/${field}` (e.g., `/todos/1/completed`)

## Types & Decoding

### `Spec.elm`

New `Repeat` type and updated `Element`:

```elm
type alias Repeat =
    { statePath : String
    , key : Maybe String
    }

type alias Element =
    { type_ : String
    , props : Dict String PropValue
    , children : List String
    , visible : Maybe VisibilityCondition
    , repeat : Maybe Repeat
    }
```

JSON decoder for `repeat`:

```elm
repeatDecoder : Decoder Repeat
repeatDecoder =
    Decode.succeed Repeat
        |> required "statePath" Decode.string
        |> optional "key" (Decode.map Just Decode.string) Nothing
```

Element decoder gains `|> optional "repeat" (Decode.map Just repeatDecoder) Nothing`.

`Repeat` must be added to the module's `exposing` list so tests can construct `repeat = Just { statePath = "/todos", key = Nothing }`.

### `Internal/PropValue.elm`

New variant:

```elm
type PropValue
    = ...existing...
    | BindItemExpr String
```

New decoder entry in the `oneOf` chain:

```elm
Decode.field "$bindItem" Decode.string |> Decode.map BindItemExpr
```

## Expression Resolution

### `Resolve.elm`

`RepeatContext` gains `basePath`:

```elm
type alias RepeatContext =
    { item : Value
    , index : Int
    , basePath : String
    }
```

Both `ItemExpr` and `BindItemExpr` must handle empty string (return whole item). Update the existing `ItemExpr` branch and add `BindItemExpr` with identical read logic:

```elm
ItemExpr field ->
    case repeatCtx of
        Just ctx ->
            if field == "" then
                jsonValueToResolved ctx.item
            else
                case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                    Ok val -> jsonValueToResolved val
                    Err _ -> RNull
        Nothing ->
            RNull

BindItemExpr field ->
    -- Same read logic as ItemExpr; the difference is in binding extraction
    case repeatCtx of
        Just ctx ->
            if field == "" then
                jsonValueToResolved ctx.item
            else
                case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                    Ok val -> jsonValueToResolved val
                    Err _ -> RNull
        Nothing ->
            RNull
```

## Rendering

### How repeat works

The element with `repeat` renders its own component normally. `repeat` only changes how **children** are produced: instead of rendering each child once, all children are rendered once per array item.

### Keying strategy

All repeated children are laid flat into a single `Html.Keyed.node "div" [ style "display" "contents" ]`:

- **Single child**: key is the item key directly (from `repeat.key` field or index)
- **Multiple children**: key is `${childElementId}-${iterationIndex}-${itemKey}`

### `Render.elm` changes

Extract `renderChildren` helper from the existing inline code:

```elm
renderChildren : Registry action -> Value -> Maybe RepeatContext -> Spec -> List String -> List (Html (Msg action))
renderChildren registry state repeatCtx spec childIds =
    List.filterMap
        (\id ->
            Dict.get id spec.elements
                |> Maybe.map (renderElement registry state repeatCtx spec)
        )
        childIds
```

Add `decodeList` helper:

```elm
decodeList : Value -> Maybe (List Value)
decodeList value =
    Decode.decodeValue (Decode.list Decode.value) value |> Result.toMaybe
```

`renderElementInner` branches on `element.repeat`:

```elm
renderElementInner registry state repeatCtx spec element =
    case Dict.get element.type_ registry of
        Just (Component componentFn) ->
            let
                resolved = Resolve.resolveProps state repeatCtx element.props
                bindings = extractBindings repeatCtx element.props
                children =
                    case element.repeat of
                        Just repeat ->
                            renderRepeatedChildren registry state spec element repeat
                        Nothing ->
                            renderChildren registry state repeatCtx spec element.children
            in
            componentFn { props = resolved, bindings = bindings, children = children, emit = \action -> CustomAction action }
        Nothing ->
            Html.text ""
```

`renderRepeatedChildren` iterates the state array and produces keyed children:

```elm
renderRepeatedChildren registry state spec element repeat =
    case State.get repeat.statePath state |> Maybe.andThen decodeList of
        Just items ->
            let
                singleChild = List.length element.children == 1
                keyedChildren =
                    List.indexedMap
                        (\i item ->
                            let
                                ctx = Just
                                    { item = item
                                    , index = i
                                    , basePath = repeat.statePath ++ "/" ++ String.fromInt i
                                    }
                                itemKey = getItemKey repeat.key i item
                            in
                            if singleChild then
                                List.filterMap
                                    (\id ->
                                        Dict.get id spec.elements
                                            |> Maybe.map (\el -> ( itemKey, renderElement registry state ctx spec el ))
                                    )
                                    element.children
                            else
                                List.filterMap
                                    (\id ->
                                        Dict.get id spec.elements
                                            |> Maybe.map (\el -> ( id ++ "-" ++ String.fromInt i ++ "-" ++ itemKey, renderElement registry state ctx spec el ))
                                    )
                                    element.children
                        )
                        items
                        |> List.concat
            in
            [ Html.Keyed.node "div" [ Html.Attributes.style "display" "contents" ] keyedChildren ]

        Nothing ->
            []
```

`getItemKey` extracts the key field from the item or falls back to index:

```elm
getItemKey : Maybe String -> Int -> Value -> String
getItemKey maybeKey index item =
    case maybeKey of
        Just keyField ->
            case Decode.decodeValue (Decode.field keyField Decode.string) item of
                Ok k -> k
                Err _ ->
                    case Decode.decodeValue (Decode.field keyField Decode.int) item of
                        Ok k -> String.fromInt k
                        Err _ -> String.fromInt index
        Nothing ->
            String.fromInt index
```

### Bindings — `extractBindings`

Signature changes to accept `Maybe RepeatContext`:

```elm
extractBindings : Maybe RepeatContext -> Dict String PropValue -> Dict String (Value -> Msg action)
extractBindings repeatCtx props =
    Dict.foldl
        (\key propValue acc ->
            case propValue of
                BindStateExpr path ->
                    Dict.insert key (\val -> SetState path val) acc

                BindItemExpr field ->
                    case repeatCtx of
                        Just ctx ->
                            let
                                path =
                                    if field == "" then
                                        ctx.basePath
                                    else
                                        ctx.basePath ++ "/" ++ field
                            in
                            Dict.insert key (\val -> SetState path val) acc

                        Nothing ->
                            acc

                _ ->
                    acc
        )
        Dict.empty
        props
```

### Nested repeats

Inner repeat context replaces outer — the `repeatCtx` parameter passed to `renderRepeatedChildren` is not forwarded. Each iteration creates a fresh `Just { item, index, basePath }` context that shadows any outer repeat.

## Schema Changes

### `schema.ts`

Add `repeat` to element definition. Note: `key` should be optional in the schema if `defineSchema` supports it; otherwise document the mismatch (Elm decoder treats it as optional):

```typescript
elements: s.record(
  s.object({
    type: s.ref("catalog.components"),
    props: s.propsOf("catalog.components"),
    children: s.array(s.string()),
    visible: s.any(),
    repeat: s.object({
      statePath: s.string(),
      key: s.string(),  // optional — omit for index-based keying
    }),
  }),
),
```

Update `defaultRules` to document `repeat` and `$bindItem`. Remove the "No repeat field — not yet supported" comment.

## Testing Strategy

### Elm unit tests

**`SpecTest.elm`:**
- Decode element with `repeat` field (statePath + key)
- Decode element with `repeat` field (statePath only, no key)
- Decode `$bindItem` expression

**`ResolveTest.elm`:**
- `$bindItem "field"` resolves to field value from repeat context item
- `$bindItem ""` resolves to whole item
- `$bindItem` outside repeat context resolves to `RNull`

**`RenderTest.elm`:**
- Repeat renders children N times for N-item array
- Repeat with multiple children renders all children per iteration
- Repeat with empty array renders nothing
- Repeat with missing state path renders nothing
- `$item` and `$index` resolve correctly within repeat
- `$bindItem` produces `SetState` with correct index-based path
- `$bindItem ""` produces `SetState` targeting whole item
- Nested repeats: inner context shadows outer

### Playwright e2e tests

**Fixtures:**
- Todo list: Stack with repeat over `/todos`, child uses `$item "title"` and `$bindItem "completed"`
- Multi-child repeat: repeat with two different child element types
- Nested repeat: outer list with inner sub-items

**Tests:**
- Correct number of rendered items
- `$item` displays correct field values
- `$bindItem` checkbox toggle mutates state at correct path
- Add/remove items from array, verify DOM updates
- Nested repeat renders correctly

## Migration Notes

Adding `basePath` to `RepeatContext` and `repeat` to `Element` will break existing test code that constructs these records directly:

- **`ResolveTest.elm`**: All `RepeatContext` records need `basePath` added (e.g., `{ item = item, index = 2, basePath = "/items/2" }`)
- **`RenderTest.elm`**: All `Element` records need `repeat = Nothing` added
- **`VisibilityTest.elm`**: Any `RepeatContext` records need `basePath` added
- **`Visibility.elm`**: Has its own `RepeatContext` type alias — must gain `basePath` to stay in sync with `Resolve.RepeatContext`. This is a structural-only change; `Visibility.evaluate` does not read `basePath` (or `item`/`index`). Consider consolidating into a shared internal module if drift becomes a risk.
- **`Bind.elm`**: Docstring for `bindable` references `$bindState` only — update to mention `$bindItem` as well.

## Known Limitations

- **`$template` in repeat contexts**: Template expressions (`$template`) resolve `${/path}` against global state only, not repeat item fields. This matches upstream behavior — the core `resolvePropValue` also resolves templates against `ctx.stateModel` only.
- **`getItemKey` type coverage**: Only tries `string` and `int` decoders for the key field value. Float, bool, or nested object keys fall back to index.
- **`jsonValueToResolved` lists/objects**: Previously resolved to `RNull` for arrays and objects. **Fixed** — `jsonValueToResolved` now recursively decodes lists to `RList` and objects to `RObject`.

## Files Changed

| File | Change |
|---|---|
| `packages/elm-core/src/JsonRender/Spec.elm` | Add `Repeat` type, update `Element`, update decoder |
| `packages/elm-core/src/JsonRender/Internal/PropValue.elm` | Add `BindItemExpr` variant + decoder |
| `packages/elm-core/src/JsonRender/Resolve.elm` | Add `basePath` to `RepeatContext`, handle `BindItemExpr` resolution, fix `$item ""` |
| `packages/elm-core/src/JsonRender/Render.elm` | Extract `renderChildren`/`decodeList` helpers, add `renderRepeatedChildren`, update `extractBindings` signature, keyed rendering |
| `packages/elm-core/src/JsonRender/Visibility.elm` | Update `RepeatContext` alias to match (add `basePath`) |
| `packages/elm-core/src/JsonRender/Bind.elm` | Update docstring |
| `packages/js-bridge/src/schema.ts` | Add `repeat` to element spec, update rules |
| `packages/elm-core/tests/SpecTest.elm` | New decode tests |
| `packages/elm-core/tests/ResolveTest.elm` | New resolution tests + update existing `RepeatContext` records |
| `packages/elm-core/tests/RenderTest.elm` | New render pipeline tests + add `repeat = Nothing` to existing `Element` records |
| `packages/elm-core/tests/VisibilityTest.elm` | Update existing `RepeatContext` records |
| `demo/test/fixtures/repeat/` | New test fixtures |
| `demo/test/specs/repeat.spec.ts` | New Playwright spec |
