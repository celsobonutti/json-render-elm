# `on` Field & `emit` Revamp Design Spec

## Overview

Add an `on` field to elements and revamp `emit` so that JSON specs can express interactive behavior (click -> add todo, click -> toggle state) without host app wiring. Components become fully `msg`-polymorphic and action-agnostic. This brings the Elm renderer to parity with React/Vue/Svelte renderers for event handling.

## Decisions

- **Keep `CustomAction action`** — host app extensibility preserved, `action` type parameter stays on `Msg`
- **`EventHandler` sum type** — `SingleAction | ChainedActions` avoids recursive `update` calls for the common single-action case
- **Custom action decoder** — host provides `String -> Dict String Value -> Result String action`; failure produces `ActionError String`
- **Resolve at update time** — action params resolved in `update`, not `view`; chained actions see prior mutations
- **Components fully `msg`-polymorphic** — no knowledge of `Msg`, `Action`, or the action system
- **Thin `emit`, heavy `update`** — `emit` captures raw bindings + repeat context; `update` resolves params and dispatches
- **CatalogSync generates action decoders** — elm-review rule generates `Action` type + `decodeAction` from catalog; same spec, parallel agent team
- **Todo tests migrate to UI-driven interactions** — existing Playwright tests use `on` instead of manual `setState`

## Wire Format

The `on` field is a new optional top-level field on elements (like `visible`, `repeat`):

```json
{
  "type": "Button",
  "props": { "label": "Add Todo" },
  "children": [],
  "on": {
    "press": { "action": "pushState", "params": { "path": "/todos", "value": { "title": "New" } } }
  }
}
```

### Shape

- `on` is optional on elements. Internal type: `Dict String EventHandler`.
- Keys are event names (arbitrary strings: `"press"`, `"change"`, `"submit"`, etc.)
- Values are either a single action binding object or an array of action bindings (chained)
- Each `ActionBinding` has:
  - `action` (String) — built-in name (`"setState"`, `"pushState"`, `"removeState"`) or custom action name
  - `params` (`Dict String PropValue`) — can contain expressions (`$state`, `$item`, `$template`, etc.) resolved at update time

### Single vs chained

Single action (most common):
```json
"on": {
  "press": { "action": "setState", "params": { "path": "/loading", "value": true } }
}
```

Chained actions (array):
```json
"on": {
  "press": [
    { "action": "setState", "params": { "path": "/loading", "value": true } },
    { "action": "pushState", "params": { "path": "/todos", "value": { "$state": "/newTodo" } } },
    { "action": "setState", "params": { "path": "/newTodo", "value": "" } }
  ]
}
```

The decoder normalizes into a sum type:

```elm
type EventHandler
    = SingleAction ActionBinding
    | ChainedActions (List ActionBinding)

-- Decoder:
Decode.oneOf
    [ Decode.list actionBindingDecoder |> Decode.map ChainedActions
    , actionBindingDecoder |> Decode.map SingleAction
    ]
```

## Elm Types & Decoding

### New types in `Spec.elm`

```elm
type alias ActionBinding =
    { action : String
    , params : Dict String PropValue
    }

type EventHandler
    = SingleAction ActionBinding
    | ChainedActions (List ActionBinding)
```

`Element` gains `on : Dict String EventHandler` (default: empty dict).

Element decoder gains:
```elm
|> optional "on" (Decode.dict eventHandlerDecoder) Dict.empty
```

### New `Msg` variants in `Actions.elm`

```elm
type Msg action
    = SetState String Value
    | PushState String Value
    | RemoveState String
    | CustomAction action
    | ActionError String
    | ExecuteAction ActionBinding (Maybe RepeatContext)
    | ExecuteChain (List ActionBinding) (Maybe RepeatContext)
    | SpecReceived Value
```

### `ActionConfig` record

```elm
type alias ActionConfig action =
    { handleAction : action -> Model -> ( Model, Cmd (Msg action) )
    , decodeAction : String -> Dict String Value -> Result String action
    }

update : ActionConfig action -> Msg action -> Model -> ( Model, Cmd (Msg action) )
```

### `ComponentContext` becomes `msg`-polymorphic

```elm
type alias ComponentContext props bindings msg =
    { props : props
    , bindings : bindings
    , children : List (Html msg)
    , emit : String -> msg
    }

type alias RawComponentContext msg =
    { props : Dict String ResolvedValue
    , bindings : Dict String (Value -> msg)
    , children : List (Html msg)
    , emit : String -> msg
    }

type Component msg
    = Component (RawComponentContext msg -> Html msg)

type alias Registry msg =
    Dict String (Component msg)
```

Components no longer import `Actions` or know about the `action` type. The renderer instantiates `msg` as `Msg action` at the call site.

### Bindings go polymorphic

Binding types in components gain a `msg` parameter:

```elm
-- Before
type alias InputBindings =
    { value : Maybe (Value -> Msg Action) }

-- After
type alias InputBindings msg =
    { value : Maybe (Value -> msg) }
```

`Bind.succeed` and `Bind.bindable` signatures update to work with the generic `msg` type.

## Emit Construction in `Render.elm`

The renderer builds `emit` per-element by closing over the element's `on` dict and current repeat context:

```elm
buildEmit : Dict String EventHandler -> Maybe RepeatContext -> (String -> Msg action)
buildEmit onHandlers repeatCtx eventName =
    case Dict.get eventName onHandlers of
        Just (SingleAction binding) ->
            ExecuteAction binding repeatCtx

        Just (ChainedActions bindings) ->
            ExecuteChain bindings repeatCtx

        Nothing ->
            ActionError ("No handler for event: " ++ eventName)
```

In `renderElementInner`, replaces `\action -> CustomAction action`:

```elm
componentFn
    { props = resolved
    , bindings = bindings
    , children = children
    , emit = buildEmit element.on repeatCtx
    }
```

Key properties:
- `emit` captures repeat context at render time (needed for `$item` resolution in `update`)
- Emitting an event with no `on` handler produces `ActionError`
- Components with no `on` handlers still work — every emit produces `ActionError`, which the host can ignore

## Action Resolution & Execution in `update`

### Resolution

Action params are `Dict String PropValue`. Resolution is a two-step process:

1. **Resolve expressions** — `Resolve.resolveProps` resolves `$state`, `$item`, `$template`, etc. against current state and the captured `RepeatContext`, producing `Dict String ResolvedValue`.
2. **Convert to Values** — a new `Resolve.resolvedToValue : ResolvedValue -> Value` function converts back to raw JSON `Value` types for consumption by built-in actions and custom action decoders.

`resolvedToValue` is the inverse of the existing `jsonValueToResolved`:

```elm
resolvedToValue : ResolvedValue -> Value
resolvedToValue rv =
    case rv of
        RString s -> Encode.string s
        RInt i -> Encode.int i
        RFloat f -> Encode.float f
        RBool b -> Encode.bool b
        RNull -> Encode.null
        RList items -> Encode.list resolvedToValue items
        RObject dict -> Encode.object (Dict.toList (Dict.map (\_ v -> resolvedToValue v) dict))
```

A convenience function `resolveActionParams` combines both steps:

```elm
resolveActionParams : Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String Value
resolveActionParams state repeatCtx params =
    resolveProps state repeatCtx params
        |> Dict.map (\_ rv -> resolvedToValue rv)
```

### Built-in dispatch

After resolving params to `Dict String Value`, match on action name:
- `"setState"` -> extract `path` (String) and `value` (Value) from resolved params -> `SetState`
- `"pushState"` -> same shape -> `PushState`
- `"removeState"` -> extract `path` -> `RemoveState`

### Custom dispatch

If the action name isn't built-in, call the host-provided decoder with the resolved `Dict String Value`: `decodeAction : String -> Dict String Value -> Result String action`. On `Ok`, dispatch `CustomAction`. On `Err`, produce `ActionError`.

### Single action execution

`ExecuteAction` resolves params against current state, dispatches directly. No list iteration.

### Chained execution

`ExecuteChain` folds left over the list. Each action resolves against the model updated by the prior action:

```elm
ExecuteChain bindings repeatCtx ->
    List.foldl
        (\binding ( model_, cmds ) ->
            let
                ( newModel, cmd ) = executeOneAction actionConfig repeatCtx binding model_
            in
            ( newModel, cmd :: cmds )
        )
        ( model, [] )
        bindings
    |> (\( finalModel, cmds ) -> ( finalModel, Cmd.batch (List.reverse cmds) ))
```

## Component Migration

All demo components undergo the same mechanical transformation:

**Before (Button.elm):**
```elm
import Components.Actions exposing (Action(..))
import JsonRender.Actions exposing (Msg)

component : Component Action
view : ComponentContext ButtonProps () Action -> Html (Msg Action)
view ctx = button [ onClick (ctx.emit Press) ] [ text ctx.props.label ]
```

**After:**
```elm
import JsonRender.Render exposing (Component, ComponentContext, register)

component : Component msg
view : ComponentContext ButtonProps () msg -> Html msg
view ctx = button [ onClick (ctx.emit "press") ] [ text ctx.props.label ]
```

Changes per component:
- Remove `import Components.Actions`
- Remove `import JsonRender.Actions`
- `Component Action` -> `Component msg`
- `ComponentContext props bindings Action` -> `ComponentContext props bindings msg`
- `Html (Msg Action)` -> `Html msg`
- `ctx.emit SomeAction` -> `ctx.emit "eventName"`

`Components.Actions` module stays — host app still defines `Action` and its decoder. But no component imports it anymore. Only `Main.elm` and `TestHarness.elm` reference it.

`Registry` becomes `Registry msg` instead of `Registry Action`.

## Schema & JS Bridge Changes

### `schema.ts`

Add `on` to element spec:

```typescript
elements: s.record(
  s.object({
    type: s.ref("catalog.components"),
    props: s.propsOf("catalog.components"),
    children: s.array(s.string()),
    visible: s.any(),
    repeat: s.any(),
    on: s.any(),
  }),
),
```

Remove "No `on` field on elements" comment. Add `defaultRules` entry:

```
'Use "on" to wire component events to actions: "on": { "press": { "action": "setState", "params": { "path": "/x", "value": true } } }. Chain multiple actions with an array. Built-in actions: setState, pushState, removeState. Custom actions reference catalog action names.'
```

## Host App Integration

### `ActionConfig`

Both `Main.elm` and `TestHarness.elm` provide an `ActionConfig action`:

```elm
actionConfig : Actions.ActionConfig Action
actionConfig =
    { handleAction = handleAction
    , decodeAction = decodeAction
    }
```

### `decodeAction` (hand-written initially, generated by CatalogSync later)

```elm
decodeAction : String -> Dict String Value -> Result String Action
decodeAction name params =
    case name of
        "press" -> Ok Press
        "export" ->
            case Dict.get "format" params of
                Just fmt ->
                    case Decode.decodeValue Decode.string fmt of
                        Ok s -> Ok (Export { format = s })
                        Err _ -> Err "export: format must be a string"
                Nothing -> Err "export: missing format param"
        _ -> Err ("Unknown action: " ++ name)
```

### CatalogSync Action Generation (parallel agent team)

The elm-review CatalogSync rule is extended to generate `Components/Actions.elm` from the catalog:

1. **`Action` type** — one variant per catalog action, with params as record fields
2. **`decodeAction` function** — `String -> Dict String Value -> Result String Action`

Generated from the catalog's `actions` section:
```typescript
actions: {
    press: { params: z.object({}), description: "Generic button press" },
    export: { params: z.object({ format: z.string() }), description: "Export data in a given format" },
}
```

The host app still writes `handleAction` by hand — that's where business logic lives. The boilerplate type + decoder comes from the catalog for free.

This work parallels the core `on`/`emit` implementation and integrates at the end.

## Testing Strategy

### Elm unit tests

**`SpecTest.elm`:**
- Decode element with `on` field (single action)
- Decode element with `on` field (chained actions array)
- Decode element with `on` containing expression params (`$state`, `$item`)
- Decode element without `on` (defaults to empty dict)

**`ActionsTest.elm`:**
- `ExecuteAction` with `setState` resolves params and sets state
- `ExecuteAction` with `pushState` resolves and pushes
- `ExecuteAction` with `removeState` resolves and removes
- `ExecuteAction` with custom action name calls decoder, produces `CustomAction`
- `ExecuteAction` with unknown action name produces `ActionError`
- `ExecuteChain` executes sequentially — second action sees first action's mutations
- `ExecuteChain` with expression params (`$state`) resolves against updated state per step
- `ActionError` carries descriptive message

**`RenderTest.elm`:**
- Component with `on.press` -> clicking emits `ExecuteAction` with correct binding
- Component with chained `on.press` -> emits `ExecuteChain`
- Component emitting unhandled event name -> `ActionError`
- Component with no `on` field -> all emits produce `ActionError`
- `emit` captures repeat context when inside a repeat

### Playwright e2e tests

**New fixtures in `demo/test/fixtures/on/`:**
- Button with `on.press` -> `setState`
- Button with `on.press` -> `pushState` (add item to list)
- Chained actions: button press sets loading + pushes item + clears input
- Custom action: button press fires custom action, verify via `testActionOut` port
- Expression params: `on.press` with `$state` reference in params

**Migrate existing todo/repeat fixtures:**
- Update todo list fixture to include "Add Todo" button with `on.press` -> chained `pushState` + `setState`
- Add per-item "Remove" button with `on.press` -> `removeState`
- Playwright tests drive interactions through UI clicks instead of manual `setState` calls
- Tests verify the full `on` -> `emit` -> `update` -> DOM cycle

### Tests verify:
- Clicking button mutates state and DOM updates
- Chained actions execute in order with correct intermediate state
- Custom actions reach the host app via `testActionOut` port
- Expression params resolve to current values at execution time
- Todo add/remove works end-to-end through UI interactions

## Implementation Strategy

Two parallel workstreams via agent teams:

**Team 1: Core `on`/`emit`**
1. Types & decoding — `ActionBinding`, `EventHandler`, `on` field on `Element`
2. `Msg` changes — `ExecuteAction`, `ExecuteChain`, `ActionError`, `ActionConfig`
3. `Render.elm` — `buildEmit`, update `renderElementInner`
4. `Actions.elm` — param resolution, built-in dispatch, custom dispatch, chain fold
5. Component migration — all demo components go `msg`-polymorphic
6. `Bind.elm` — update signatures for `msg`-polymorphic bindings
7. Host app integration — `Main.elm`, `TestHarness.elm` provide `ActionConfig`
8. Schema changes — `schema.ts` adds `on`, updates rules
9. Tests — unit tests + Playwright e2e + migrate todo tests

**Team 2: CatalogSync Action Generation (parallel)**
1. Extend catalog JSON Schema to expose action definitions
2. Update `CatalogSync.elm` to read action schemas
3. Update `ElmCodeGen.elm` to generate `Action` type + `decodeAction`
4. Tests for the elm-review rule
5. Integrate — replace hand-written `Components/Actions.elm` with generated version

Teams converge at the end when generated `Actions.elm` replaces the hand-written one.

## Migration Notes

- `Actions.update` signature changes — all call sites (`Main.elm`, `TestHarness.elm`) must pass `ActionConfig` instead of bare `handleAction`
- All component modules lose their `Action` import — mechanical but touches every component file
- `RenderTest.elm` test registry components need updating for `msg`-polymorphic signatures
- `Bind.elm` type signatures change — any code using `Bind.succeed`/`Bind.bindable` must update
- Existing Playwright todo tests will be rewritten to use `on`-driven fixtures

## Files Changed

| File | Change |
|---|---|
| `packages/elm-core/src/JsonRender/Spec.elm` | Add `ActionBinding`, `EventHandler`, `on` field on `Element`, decoders |
| `packages/elm-core/src/JsonRender/Actions.elm` | Add `ActionError`, `ExecuteAction`, `ExecuteChain`, `ActionConfig`, param resolution, chain execution |
| `packages/elm-core/src/JsonRender/Render.elm` | `buildEmit`, `msg`-polymorphic types, update `renderElementInner` |
| `packages/elm-core/src/JsonRender/Resolve.elm` | Add `resolvedToValue`, `resolveActionParams`; export both for use in `Actions.elm` |
| `packages/elm-core/src/JsonRender/Bind.elm` | Update signatures for `msg`-polymorphic bindings |
| `packages/js-bridge/src/schema.ts` | Add `on` to element spec, update rules and comments |
| `demo/src/Components/Button.elm` | `msg`-polymorphic, `emit "press"` |
| `demo/src/Components/Input.elm` | `msg`-polymorphic bindings |
| `demo/src/Components/Card.elm` | `msg`-polymorphic |
| `demo/src/Components/Text.elm` | `msg`-polymorphic |
| `demo/src/Components/Stack.elm` | `msg`-polymorphic |
| `demo/src/Components/Image.elm` | `msg`-polymorphic |
| `demo/src/Components/Badge.elm` | `msg`-polymorphic |
| `demo/src/Components/Actions.elm` | Add `decodeAction` (hand-written, later generated) |
| `demo/src/Components/Registry.elm` | `Registry msg` instead of `Registry Action` |
| `demo/src/Main.elm` | Provide `ActionConfig`, update `Actions.update` call |
| `demo/src/TestHarness.elm` | Provide `ActionConfig`, update `Actions.update` call |
| `packages/elm-core/tests/SpecTest.elm` | New `on` decode tests |
| `packages/elm-core/tests/ActionsTest.elm` | New action execution tests |
| `packages/elm-core/tests/RenderTest.elm` | Update test registry, new emit tests |
| `packages/elm-review/src/JsonRender/CatalogSync.elm` | Read action schemas, generate `Actions.elm` |
| `packages/elm-review/src/JsonRender/Internal/ElmCodeGen.elm` | Generate `Action` type + `decodeAction` |
| `demo/test/fixtures/on/` | New e2e test fixtures |
| `demo/test/fixtures/repeat/` | Update todo fixtures with `on` handlers |
| `demo/test/specs/on.spec.ts` | New Playwright spec |
| `demo/test/specs/repeat.spec.ts` | Migrate to UI-driven interactions |
