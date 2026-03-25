# Repeat & $bindItem Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `repeat` field to elements and `$bindItem` expression so the Elm renderer can iterate over state arrays with two-way binding to individual items.

**Architecture:** Bottom-up — types/decoding first, then expression resolution, then rendering. Each layer is independently testable. The repeat field on an element causes its children to render once per array item; a keyed wrapper div with `display: contents` handles VDOM diffing.

**Tech Stack:** Elm 0.19, elm-test, elm-json-decode-pipeline, Html.Keyed, Playwright

**Spec:** `docs/superpowers/specs/2026-03-25-repeat-and-bind-item-design.md`

**Test command:** `cd packages/elm-core && npx elm-test` (run with sandbox disabled — elm-test needs `/tmp` socket access)

**E2E test command:** `cd demo && npm run test:e2e`

---

## File Map

| File | Action | Responsibility |
|---|---|---|
| `packages/elm-core/src/JsonRender/Internal/PropValue.elm` | Modify | Add `BindItemExpr` variant + decoder |
| `packages/elm-core/src/JsonRender/Spec.elm` | Modify | Add `Repeat` type, update `Element`, update decoder, update `exposing` |
| `packages/elm-core/src/JsonRender/Resolve.elm` | Modify | Add `basePath` to `RepeatContext`, handle `BindItemExpr` + `$item ""` |
| `packages/elm-core/src/JsonRender/Visibility.elm` | Modify | Add `basePath` to `RepeatContext` (structural sync only) |
| `packages/elm-core/src/JsonRender/Render.elm` | Modify | Add repeat iteration, keyed rendering, update `extractBindings` |
| `packages/elm-core/src/JsonRender/Bind.elm` | Modify | Update docstring |
| `packages/js-bridge/src/schema.ts` | Modify | Add `repeat` to element spec, update rules |
| `packages/elm-core/tests/SpecTest.elm` | Modify | Add decode tests for `repeat` and `$bindItem` |
| `packages/elm-core/tests/ResolveTest.elm` | Modify | Fix existing `RepeatContext` records, add `$bindItem` + `$item ""` tests |
| `packages/elm-core/tests/VisibilityTest.elm` | No change | Has no `RepeatContext` records (confirmed by reading the file) |
| `packages/elm-core/tests/RenderTest.elm` | Modify | Fix existing `Element` records, add repeat rendering tests |
| `demo/test/fixtures/repeat/` | Create | JSON fixtures for e2e tests |
| `demo/test/specs/repeat.spec.ts` | Create | Playwright e2e tests |

---

### Task 1: Add `BindItemExpr` to PropValue

**Files:**
- Modify: `packages/elm-core/src/JsonRender/Internal/PropValue.elm`
- Modify: `packages/elm-core/tests/SpecTest.elm`

- [ ] **Step 1: Write failing test for `$bindItem` decoding**

Add to `packages/elm-core/tests/SpecTest.elm`, inside the `describe "JsonRender.Spec"` list, before the closing `]`:

```elm
        , test "decodes $bindItem expression" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "t",
                          "elements": {
                            "t": {
                              "type": "Input",
                              "props": { "checked": { "$bindItem": "completed" } },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "t" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just (BindItemExpr "completed"))
                                    (Dict.get "checked" el.props)

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: Compilation error — `BindItemExpr` is not a constructor of `PropValue`.

- [ ] **Step 3: Add `BindItemExpr` variant and decoder**

In `packages/elm-core/src/JsonRender/Internal/PropValue.elm`, add the variant to the type:

```elm
type PropValue
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue
    | ListValue (List PropValue)
    | ObjectValue (Dict String PropValue)
    | StateExpr String
    | ItemExpr String
    | IndexExpr
    | TemplateExpr String
    | BindStateExpr String
    | BindItemExpr String
```

Add the decoder entry in the `oneOf` chain, after `BindStateExpr`:

```elm
        , Decode.field "$bindItem" Decode.string |> Decode.map BindItemExpr
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: All tests pass (the new `BindItemExpr` test + all existing tests).

- [ ] **Step 5: Commit**

```bash
git add packages/elm-core/src/JsonRender/Internal/PropValue.elm packages/elm-core/tests/SpecTest.elm
git commit -m "feat: add BindItemExpr variant to PropValue"
```

---

### Task 2: Add `Repeat` type and update `Element` decoder

**Files:**
- Modify: `packages/elm-core/src/JsonRender/Spec.elm`
- Modify: `packages/elm-core/tests/SpecTest.elm`

- [ ] **Step 1: Write failing tests for `repeat` decoding**

Add to `packages/elm-core/tests/SpecTest.elm`, inside the `describe "JsonRender.Spec"` list:

```elm
        , test "decodes element with repeat field (statePath + key)" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "list",
                          "elements": {
                            "list": {
                              "type": "Stack",
                              "props": { "direction": "vertical" },
                              "children": ["item"],
                              "repeat": { "statePath": "/todos", "key": "id" }
                            },
                            "item": {
                              "type": "Text",
                              "props": { "content": { "$item": "title" } },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "list" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just { statePath = "/todos", key = Just "id" })
                                    el.repeat

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes element with repeat field (statePath only, no key)" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "list",
                          "elements": {
                            "list": {
                              "type": "Stack",
                              "props": {},
                              "children": [],
                              "repeat": { "statePath": "/items" }
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "list" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just { statePath = "/items", key = Nothing })
                                    el.repeat

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "element without repeat decodes as Nothing" <|
            \_ ->
                case Decode.decodeString Spec.decoder specJson of
                    Ok spec ->
                        case Dict.get "card-1" spec.elements of
                            Just el ->
                                Expect.equal Nothing el.repeat

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
```

Also add `import JsonRender.Spec exposing (Repeat)` to the imports (or just use the record literal — Elm structural typing means we don't need to import the alias to compare records). Actually, since the test uses `el.repeat` which returns `Maybe Repeat`, we just compare with record literals, so no import needed.

- [ ] **Step 2: Run test to verify it fails**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: Compilation errors — `Element` has no field `repeat`.

- [ ] **Step 3: Add `Repeat` type, update `Element`, update decoder**

In `packages/elm-core/src/JsonRender/Spec.elm`:

Update the `exposing` clause:

```elm
module JsonRender.Spec exposing
    ( Element
    , Repeat
    , Spec
    , decoder
    , propValueDecoder
    )
```

Add the `Repeat` type after `Element`:

```elm
type alias Repeat =
    { statePath : String
    , key : Maybe String
    }
```

Update `Element` to add the `repeat` field:

```elm
type alias Element =
    { type_ : String
    , props : Dict String PropValue
    , children : List String
    , visible : Maybe VisibilityCondition
    , repeat : Maybe Repeat
    }
```

Add the `repeatDecoder` and update `elementDecoder`:

```elm
elementDecoder : Decoder Element
elementDecoder =
    Decode.succeed Element
        |> required "type" Decode.string
        |> required "props" (Decode.dict PropValue.decoder)
        |> required "children" (Decode.list Decode.string)
        |> optional "visible" (Decode.map Just JsonRender.Visibility.decoder) Nothing
        |> optional "repeat" (Decode.map Just repeatDecoder) Nothing


repeatDecoder : Decoder Repeat
repeatDecoder =
    Decode.succeed Repeat
        |> required "statePath" Decode.string
        |> optional "key" (Decode.map Just Decode.string) Nothing
```

- [ ] **Step 4: Fix existing `RenderTest.elm` Element records**

Adding `repeat` to `Element` breaks all directly-constructed Element records in `RenderTest.elm`. Add `repeat = Nothing` to every Element record. There are 7 occurrences — each one that looks like:

```elm
, visible = Nothing
}
```

becomes:

```elm
, visible = Nothing
, repeat = Nothing
}
```

The full list of elements to update (search for `visible = Nothing` or `visible = Just` in RenderTest.elm):
1. Line 68: "renders a single element" — Text element
2. Line 85: "renders nested elements" — Card element
3. Line 92: "renders nested elements" — Text inner element
4. Line 111: "renders nothing for unknown component" — Unknown element
5. Line 135: "respects visibility condition" — Text element with `visible = Just`
6. Line 191: "$bindState and provides setter binding" — Input element

- [ ] **Step 5: Run tests to verify they pass**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: All tests pass including the 3 new repeat decoding tests.

- [ ] **Step 6: Commit**

```bash
git add packages/elm-core/src/JsonRender/Spec.elm packages/elm-core/tests/SpecTest.elm packages/elm-core/tests/RenderTest.elm
git commit -m "feat: add Repeat type and repeat field to Element"
```

---

### Task 3: Update `RepeatContext` and add `$bindItem`/`$item ""` resolution

**Files:**
- Modify: `packages/elm-core/src/JsonRender/Resolve.elm`
- Modify: `packages/elm-core/src/JsonRender/Visibility.elm`
- Modify: `packages/elm-core/tests/ResolveTest.elm`

- [ ] **Step 1: Write failing tests for `$bindItem` resolution and `$item ""`**

In `packages/elm-core/tests/ResolveTest.elm`, first update the two existing `RepeatContext` records to add `basePath`. Change:

```elm
Just { item = item, index = 2 }
```
to:
```elm
Just { item = item, index = 2, basePath = "/items/2" }
```

And change:
```elm
Just { item = Encode.null, index = 5 }
```
to:
```elm
Just { item = Encode.null, index = 5, basePath = "/items/5" }
```

And in the `jsonValueToResolved handles complex types` section, change:
```elm
Just { item = item, index = 0 }
```
to:
```elm
Just { item = item, index = 0, basePath = "/items/0" }
```

Then add new tests inside the `describe "resolveProps"` list:

```elm
            , test "resolves $bindItem expression (read direction)" <|
                \_ ->
                    let
                        item =
                            Encode.object [ ( "title", Encode.string "Buy milk" ) ]

                        ctx =
                            Just { item = item, index = 0, basePath = "/todos/0" }

                        props =
                            Dict.fromList [ ( "label", BindItemExpr "title" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RString "Buy milk"))
                        (Dict.get "label" resolved)
            , test "resolves $bindItem with empty string to whole item" <|
                \_ ->
                    let
                        item =
                            Encode.string "hello"

                        ctx =
                            Just { item = item, index = 0, basePath = "/items/0" }

                        props =
                            Dict.fromList [ ( "value", BindItemExpr "" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RString "hello"))
                        (Dict.get "value" resolved)
            , test "resolves $bindItem outside repeat context to RNull" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "value", BindItemExpr "title" ) ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.equal
                        (Just RNull)
                        (Dict.get "value" resolved)
            , test "resolves $item with empty string to whole item" <|
                \_ ->
                    let
                        item =
                            Encode.object [ ( "name", Encode.string "Alice" ) ]

                        ctx =
                            Just { item = item, index = 0, basePath = "/items/0" }

                        props =
                            Dict.fromList [ ( "data", ItemExpr "" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RObject (Dict.fromList [ ( "name", RString "Alice" ) ])))
                        (Dict.get "data" resolved)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: Compilation errors — `RepeatContext` records have wrong shape (missing `basePath`), `BindItemExpr` not handled in `resolvePropValue`.

- [ ] **Step 3: Update `RepeatContext` in Resolve.elm and Visibility.elm, add resolution logic**

In `packages/elm-core/src/JsonRender/Resolve.elm`, update the `RepeatContext` type:

```elm
type alias RepeatContext =
    { item : Value
    , index : Int
    , basePath : String
    }
```

Update the `ItemExpr` branch to handle empty string:

```elm
        ItemExpr field ->
            case repeatCtx of
                Just ctx ->
                    if field == "" then
                        jsonValueToResolved ctx.item

                    else
                        case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                            Ok val ->
                                jsonValueToResolved val

                            Err _ ->
                                RNull

                Nothing ->
                    RNull
```

Add the `BindItemExpr` branch after `BindStateExpr`:

```elm
        BindItemExpr field ->
            case repeatCtx of
                Just ctx ->
                    if field == "" then
                        jsonValueToResolved ctx.item

                    else
                        case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                            Ok val ->
                                jsonValueToResolved val

                            Err _ ->
                                RNull

                Nothing ->
                    RNull
```

In `packages/elm-core/src/JsonRender/Visibility.elm`, update the `RepeatContext` type to match:

```elm
type alias RepeatContext =
    { item : Value
    , index : Int
    , basePath : String
    }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: All tests pass.

- [ ] **Step 5: Commit**

```bash
git add packages/elm-core/src/JsonRender/Resolve.elm packages/elm-core/src/JsonRender/Visibility.elm packages/elm-core/tests/ResolveTest.elm
git commit -m "feat: add basePath to RepeatContext, handle BindItemExpr and \$item empty string"
```

---

### Task 4: Add repeat rendering to Render.elm

**Files:**
- Modify: `packages/elm-core/src/JsonRender/Render.elm`
- Modify: `packages/elm-core/tests/RenderTest.elm`

- [ ] **Step 1: Write failing test — repeat renders children N times**

Add to `packages/elm-core/tests/RenderTest.elm`, inside the `describe "JsonRender.Render"` list:

```elm
        , describe "repeat rendering"
            [ test "repeat renders children once per array item" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "list",
                              "elements": {
                                "list": {
                                  "type": "Card",
                                  "props": { "title": "Todos" },
                                  "children": ["item"],
                                  "repeat": { "statePath": "/todos", "key": "id" }
                                },
                                "item": {
                                  "type": "Text",
                                  "props": { "content": { "$item": "title" } },
                                  "children": []
                                }
                              }
                            }
                            """

                        todoState =
                            Encode.object
                                [ ( "todos"
                                  , Encode.list identity
                                        [ Encode.object [ ( "id", Encode.string "1" ), ( "title", Encode.string "Buy milk" ) ]
                                        , Encode.object [ ( "id", Encode.string "2" ), ( "title", Encode.string "Walk dog" ) ]
                                        ]
                                  )
                                ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry todoState spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Buy milk", Selector.text "Walk dog" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: FAIL — repeat rendering not implemented, children render with no repeat context so `$item` resolves to `RNull` and Text component gets empty string or fails.

- [ ] **Step 3: Implement repeat rendering in Render.elm**

In `packages/elm-core/src/JsonRender/Render.elm`, add new imports:

```elm
import Html.Attributes
import Html.Keyed
import Json.Decode as Decode
import JsonRender.Spec exposing (Element, Repeat, Spec)
import JsonRender.State as State
```

(Replace the existing `import JsonRender.Spec exposing (Element, Spec)` line with the one that includes `Repeat`.)

Add helper functions after `extractBindings`:

```elm
renderChildren : Registry action -> Value -> Maybe RepeatContext -> Spec -> List String -> List (Html (Msg action))
renderChildren registry state repeatCtx spec childIds =
    List.filterMap
        (\id ->
            Dict.get id spec.elements
                |> Maybe.map (renderElement registry state repeatCtx spec)
        )
        childIds


decodeList : Value -> Maybe (List Value)
decodeList value =
    Decode.decodeValue (Decode.list Decode.value) value |> Result.toMaybe


getItemKey : Maybe String -> Int -> Value -> String
getItemKey maybeKey index item =
    case maybeKey of
        Just keyField ->
            case Decode.decodeValue (Decode.field keyField Decode.string) item of
                Ok k ->
                    k

                Err _ ->
                    case Decode.decodeValue (Decode.field keyField Decode.int) item of
                        Ok k ->
                            String.fromInt k

                        Err _ ->
                            String.fromInt index

        Nothing ->
            String.fromInt index


renderRepeatedChildren : Registry action -> Value -> Spec -> Element -> Repeat -> List (Html (Msg action))
renderRepeatedChildren registry state spec element repeat =
    case State.get repeat.statePath state |> Maybe.andThen decodeList of
        Just items ->
            let
                singleChild =
                    List.length element.children == 1

                keyedChildren =
                    List.indexedMap
                        (\i item ->
                            let
                                ctx =
                                    Just
                                        { item = item
                                        , index = i
                                        , basePath = repeat.statePath ++ "/" ++ String.fromInt i
                                        }

                                itemKey =
                                    getItemKey repeat.key i item
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

Update `extractBindings` to accept `Maybe RepeatContext`:

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

Update `renderElementInner` to use the new helpers:

```elm
renderElementInner : Registry action -> Value -> Maybe RepeatContext -> Spec -> Element -> Html (Msg action)
renderElementInner registry state repeatCtx spec element =
    case Dict.get element.type_ registry of
        Just (Component componentFn) ->
            let
                resolved =
                    Resolve.resolveProps state repeatCtx element.props

                bindings =
                    extractBindings repeatCtx element.props

                children =
                    case element.repeat of
                        Just repeat ->
                            renderRepeatedChildren registry state spec element repeat

                        Nothing ->
                            renderChildren registry state repeatCtx spec element.children
            in
            componentFn
                { props = resolved
                , bindings = bindings
                , children = children
                , emit = \action -> CustomAction action
                }

        Nothing ->
            Html.text ""
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: All tests pass including the new repeat test.

- [ ] **Step 5: Commit**

```bash
git add packages/elm-core/src/JsonRender/Render.elm packages/elm-core/tests/RenderTest.elm
git commit -m "feat: add repeat rendering with keyed children"
```

---

### Task 5: Additional repeat rendering tests

**Files:**
- Modify: `packages/elm-core/tests/RenderTest.elm`

- [ ] **Step 1: Write tests for edge cases and `$bindItem` bindings**

Add inside the `describe "repeat rendering"` list in `RenderTest.elm`:

```elm
            , test "repeat with empty array renders no children" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "list",
                              "elements": {
                                "list": {
                                  "type": "Card",
                                  "props": { "title": "Empty" },
                                  "children": ["item"],
                                  "repeat": { "statePath": "/todos" }
                                },
                                "item": {
                                  "type": "Text",
                                  "props": { "content": { "$item": "title" } },
                                  "children": []
                                }
                              }
                            }
                            """

                        emptyState =
                            Encode.object [ ( "todos", Encode.list identity [] ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry emptyState spec
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has [ Selector.text "Empty" ]
                                    , Query.hasNot [ Selector.tag "span" ]
                                    ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "repeat with missing state path renders no children" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "list",
                              "elements": {
                                "list": {
                                  "type": "Card",
                                  "props": { "title": "Missing" },
                                  "children": ["item"],
                                  "repeat": { "statePath": "/nonexistent" }
                                },
                                "item": {
                                  "type": "Text",
                                  "props": { "content": "should not appear" },
                                  "children": []
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry Encode.null spec
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has [ Selector.text "Missing" ]
                                    , Query.hasNot [ Selector.text "should not appear" ]
                                    ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "$index resolves correctly within repeat" <|
                \_ ->
                    let
                        indexRegistry : Render.Registry TestAction
                        indexRegistry =
                            Dict.fromList
                                [ ( "Card"
                                  , Render.register
                                        (\props ->
                                            Resolve.succeed identity
                                                |> Resolve.required "title" Resolve.string
                                                |> (\d -> d props)
                                        )
                                        (\_ -> ())
                                        (\ctx ->
                                            div [ class "card" ]
                                                [ text ctx.props
                                                , div [] ctx.children
                                                ]
                                        )
                                  )
                                , ( "Text"
                                  , Render.register
                                        (\props ->
                                            Resolve.succeed identity
                                                |> Resolve.required "content" Resolve.int
                                                |> (\d -> d props)
                                        )
                                        (\_ -> ())
                                        (\ctx -> text (String.fromInt ctx.props))
                                  )
                                ]

                        json =
                            """
                            {
                              "root": "list",
                              "elements": {
                                "list": {
                                  "type": "Card",
                                  "props": { "title": "Indexed" },
                                  "children": ["item"],
                                  "repeat": { "statePath": "/items" }
                                },
                                "item": {
                                  "type": "Text",
                                  "props": { "content": { "$index": true } },
                                  "children": []
                                }
                              }
                            }
                            """

                        itemState =
                            Encode.object
                                [ ( "items"
                                  , Encode.list Encode.string [ "a", "b", "c" ]
                                  )
                                ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            indexRegistry
                                |> (\r -> Render.render r itemState spec)
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "0", Selector.text "1", Selector.text "2" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "$bindItem produces SetState with index-based path" <|
                \_ ->
                    let
                        bindRegistry : Render.Registry TestAction
                        bindRegistry =
                            Dict.fromList
                                [ ( "Card"
                                  , Render.register
                                        (\props ->
                                            Resolve.succeed identity
                                                |> Resolve.required "title" Resolve.string
                                                |> (\d -> d props)
                                        )
                                        (\_ -> ())
                                        (\ctx ->
                                            div [ class "card" ]
                                                [ text ctx.props
                                                , div [] ctx.children
                                                ]
                                        )
                                  )
                                , ( "Input"
                                  , Render.register
                                        (\props ->
                                            Resolve.succeed identity
                                                |> Resolve.required "value" Resolve.string
                                                |> (\d -> d props)
                                        )
                                        (\bindings ->
                                            { value = Dict.get "value" bindings }
                                        )
                                        (\ctx ->
                                            div []
                                                [ text ctx.props
                                                , case ctx.bindings.value of
                                                    Just _ ->
                                                        text "[bound]"

                                                    Nothing ->
                                                        text "[unbound]"
                                                ]
                                        )
                                  )
                                ]

                        json =
                            """
                            {
                              "root": "list",
                              "elements": {
                                "list": {
                                  "type": "Card",
                                  "props": { "title": "Todos" },
                                  "children": ["item"],
                                  "repeat": { "statePath": "/todos", "key": "id" }
                                },
                                "item": {
                                  "type": "Input",
                                  "props": { "value": { "$bindItem": "name" } },
                                  "children": []
                                }
                              }
                            }
                            """

                        todoState =
                            Encode.object
                                [ ( "todos"
                                  , Encode.list identity
                                        [ Encode.object [ ( "id", Encode.string "1" ), ( "name", Encode.string "Alice" ) ]
                                        ]
                                  )
                                ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render bindRegistry todoState spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Alice", Selector.text "[bound]" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
```

- [ ] **Step 2: Run tests to verify they pass**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: All tests pass (these exercise already-implemented functionality from Task 4).

- [ ] **Step 3: Write and run test for nested repeats**

Add inside the `describe "repeat rendering"` list:

```elm
            , test "nested repeats shadow outer context" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "outer",
                              "elements": {
                                "outer": {
                                  "type": "Card",
                                  "props": { "title": "Groups" },
                                  "children": ["inner"],
                                  "repeat": { "statePath": "/groups" }
                                },
                                "inner": {
                                  "type": "Card",
                                  "props": { "title": { "$item": "name" } },
                                  "children": ["leaf"],
                                  "repeat": { "statePath": "/items" }
                                },
                                "leaf": {
                                  "type": "Text",
                                  "props": { "content": { "$item": "label" } },
                                  "children": []
                                }
                              }
                            }
                            """

                        nestedState =
                            Encode.object
                                [ ( "groups"
                                  , Encode.list identity
                                        [ Encode.object [ ( "name", Encode.string "Group A" ) ]
                                        ]
                                  )
                                , ( "items"
                                  , Encode.list identity
                                        [ Encode.object [ ( "label", Encode.string "Item X" ) ]
                                        , Encode.object [ ( "label", Encode.string "Item Y" ) ]
                                        ]
                                  )
                                ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry nestedState spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Item X", Selector.text "Item Y" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
```

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: All tests pass.

- [ ] **Step 4: Commit**

```bash
git add packages/elm-core/tests/RenderTest.elm
git commit -m "test: add repeat edge case and nested repeat tests"
```

---

### Task 6: Update Bind.elm docstring and schema.ts

**Files:**
- Modify: `packages/elm-core/src/JsonRender/Bind.elm`
- Modify: `packages/js-bridge/src/schema.ts`

- [ ] **Step 1: Update Bind.elm docstring**

In `packages/elm-core/src/JsonRender/Bind.elm`, change the `bindable` docstring from:

```elm
{-| Extract a binding setter for the given prop name.

Returns `Just setter` if the prop had a `$bindState` expression,
`Nothing` otherwise.

-}
```

to:

```elm
{-| Extract a binding setter for the given prop name.

Returns `Just setter` if the prop had a `$bindState` or `$bindItem` expression,
`Nothing` otherwise.

-}
```

- [ ] **Step 2: Update schema.ts**

In `packages/js-bridge/src/schema.ts`:

Add `repeat` to the element object (after the `visible` field):

```typescript
          /** Visibility condition */
          visible: s.any(),
          /** Repeat over a state array: { statePath: "/path", key?: "fieldName" } */
          repeat: s.any(),
```

Update the doc comment at the top — remove the line `* - No \`repeat\` field — not yet supported (backlog)` and update the expressions line:

```typescript
 * - Expressions: $state, $item, $index, $template (read-only), $bindState, $bindItem (two-way binding)
```

Update the expression support rule in `defaultRules`:

```typescript
      'Dynamic props: use { "$state": "/path" } to read from state, { "$bindState": "/path" } for two-way binding (read and write), { "$item": "field" } to read from repeat item (use "" for whole item), { "$index": true } for repeat index, { "$bindItem": "field" } for two-way binding to repeat item field, { "$template": "Hello ${/name}" } for string interpolation.',
```

Add a new rule for repeat:

```typescript
      'Use "repeat": { "statePath": "/arrayPath", "key": "id" } on an element to render its children once per array item. Props on child elements can use $item and $bindItem to access item fields. The "key" field is optional but recommended for stable rendering when items change.',
```

- [ ] **Step 3: Verify demo compiles**

Run: `cd demo && npx elm make src/Main.elm --output=/dev/null && npx elm make src/TestHarness.elm --output=/dev/null`
Expected: Both compile successfully.

- [ ] **Step 4: Commit**

```bash
git add packages/elm-core/src/JsonRender/Bind.elm packages/js-bridge/src/schema.ts
git commit -m "feat: update schema and docs for repeat and \$bindItem"
```

---

### Task 7: Playwright e2e tests

**Files:**
- Create: `demo/test/fixtures/repeat/todo-list.json`
- Create: `demo/test/fixtures/repeat/multi-child.json`
- Create: `demo/test/specs/repeat.spec.ts`

- [ ] **Step 1: Create todo-list fixture**

Create `demo/test/fixtures/repeat/todo-list.json`:

```json
{
  "root": "list",
  "elements": {
    "list": {
      "type": "Stack",
      "props": { "direction": "vertical" },
      "children": ["item"],
      "repeat": { "statePath": "/todos", "key": "id" }
    },
    "item": {
      "type": "Text",
      "props": { "content": { "$item": "title" } },
      "children": []
    }
  }
}
```

- [ ] **Step 2: Create multi-child fixture**

Create `demo/test/fixtures/repeat/multi-child.json`:

```json
{
  "root": "list",
  "elements": {
    "list": {
      "type": "Stack",
      "props": { "direction": "vertical" },
      "children": ["title", "badge"],
      "repeat": { "statePath": "/items", "key": "id" }
    },
    "title": {
      "type": "Text",
      "props": { "content": { "$item": "name" } },
      "children": []
    },
    "badge": {
      "type": "Badge",
      "props": { "text": { "$item": "status" } },
      "children": []
    }
  }
}
```

- [ ] **Step 3: Create Playwright spec**

Create `demo/test/specs/repeat.spec.ts`:

```typescript
import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Repeat", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("renders children once per array item", async ({ page }) => {
    await setState(page, {
      todos: [
        { id: "1", title: "Buy milk" },
        { id: "2", title: "Walk dog" },
        { id: "3", title: "Write code" },
      ],
    })
    await sendSpec(page, "repeat/todo-list.json")

    const items = page.locator(".jr-text")
    await expect(items).toHaveCount(3)
    await expect(items.nth(0)).toHaveText("Buy milk")
    await expect(items.nth(1)).toHaveText("Walk dog")
    await expect(items.nth(2)).toHaveText("Write code")
  })

  test("renders nothing for empty array", async ({ page }) => {
    await setState(page, { todos: [] })
    await sendSpec(page, "repeat/todo-list.json")

    await expect(page.locator(".jr-text")).toHaveCount(0)
  })

  test("renders multiple children per iteration", async ({ page }) => {
    await setState(page, {
      items: [
        { id: "1", name: "Alice", status: "active" },
        { id: "2", name: "Bob", status: "inactive" },
      ],
    })
    await sendSpec(page, "repeat/multi-child.json")

    const texts = page.locator(".jr-text")
    await expect(texts).toHaveCount(2)
    await expect(texts.nth(0)).toHaveText("Alice")
    await expect(texts.nth(1)).toHaveText("Bob")

    const badges = page.locator(".jr-badge")
    await expect(badges).toHaveCount(2)
    await expect(badges.nth(0)).toHaveText("active")
    await expect(badges.nth(1)).toHaveText("inactive")
  })

  test("updates DOM when state array changes", async ({ page }) => {
    await setState(page, {
      todos: [{ id: "1", title: "Buy milk" }],
    })
    await sendSpec(page, "repeat/todo-list.json")
    await expect(page.locator(".jr-text")).toHaveCount(1)

    await setState(page, {
      todos: [
        { id: "1", title: "Buy milk" },
        { id: "2", title: "Walk dog" },
      ],
    })
    // State change triggers re-render since TestHarness re-renders on state change
    await expect(page.locator(".jr-text")).toHaveCount(2)
  })
})
```

- [ ] **Step 4: Run e2e tests**

Run: `cd demo && npm run test:e2e` (sandbox disabled — needs to launch browser)
Expected: All repeat tests pass alongside existing tests.

- [ ] **Step 5: Commit**

```bash
git add demo/test/fixtures/repeat/ demo/test/specs/repeat.spec.ts
git commit -m "test: add Playwright e2e tests for repeat rendering"
```

---

### Task 8: Final verification

- [ ] **Step 1: Run all Elm tests**

Run: `cd packages/elm-core && npx elm-test` (sandbox disabled)
Expected: All tests pass (should be 70+ tests).

- [ ] **Step 2: Run all Playwright tests**

Run: `cd demo && npm run test:e2e` (sandbox disabled)
Expected: All tests pass (should be 14+ tests).

- [ ] **Step 3: Compile demo apps**

Run: `cd demo && npx elm make src/Main.elm --output=/dev/null && npx elm make src/TestHarness.elm --output=/dev/null`
Expected: Both compile cleanly.

- [ ] **Step 4: Final commit if any cleanup needed**

Only if there were fixes during verification. Otherwise, skip.
