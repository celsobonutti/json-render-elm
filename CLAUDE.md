# json-render-elm

Elm renderer for the [json-render](https://json-render.dev) generative UI framework. AI generates JSON specs constrained to a component catalog; this project decodes, evaluates, and renders them in Elm.

## Project Structure

Three packages + a demo/test app:

```
packages/
  elm-core/       # Elm package: types, decoding, expressions, rendering
  elm-review/     # elm-review rules: CatalogSync validates/generates component modules
  js-bridge/      # npm package (@json-render/elm): ports, bridge, Elm-specific schema
demo/             # Vite + Elm demo app AND Playwright test harness
docs/
  superpowers/
    specs/         # Design specs
    plans/         # Implementation plans
```

## Commands

```bash
# Elm core tests (58 unit tests)
cd packages/elm-core && npx elm-test

# elm-review tests
cd packages/elm-review && npx elm-test

# Playwright e2e tests (10 browser tests)
cd demo && npm run test:e2e

# Playwright parity tests (40 tests — Elm vs React side-by-side)
cd demo && npx playwright test --project=parity

# Demo dev server
cd demo && npm run dev

# Compile demo
cd demo && npx elm make src/Main.elm --output=/dev/null

# Compile test harness
cd demo && npx elm make src/TestHarness.elm --output=/dev/null

# Generate catalog schema for elm-review
cd demo && npm run catalog

# Run elm-review with fixes
cd demo && npm run elm-review:fix
```

## Design Principles

### Follow the Core Spec
This project implements the [json-render](https://json-render.dev) core specification. When designing new features, follow the core spec's semantics. Expressions compose: `$computed` args can use any expression type (`$state`, `$item`, `$computed`, etc.), and any prop that accepts an expression accepts all expression types uniformly.

## Architecture Decisions

### Port-Driven, Full-Spec Snapshots
The JS bridge sends complete `{ root, elements }` specs to Elm via ports. No streaming, no incremental patches (v1). The Elm app owns all state; the spec is stateless.

### Flat Element Model
Elements are stored in a flat `Dict String Element` keyed by ID. Parent-child relationships are string references in the `children` field. This matches json-render's wire format and avoids recursive Elm types.

### Pipeline-Style Decoders
Use `NoRedInk/elm-json-decode-pipeline` for ALL record/multi-field JSON decoders. For prop resolution, use the custom `Resolve.succeed/required/optional` pipeline. For bindings, use `Bind.succeed/bindable`.

### Type-Safe Component Registry
Components register with typed props and bindings decoders. The `register` function takes three arguments:
1. Props decoder: `Dict String ResolvedValue -> Result String props`
2. Bindings decoder: `Dict String (Value -> Msg) -> bindings`
3. View: `ComponentContext props bindings -> Html Msg`

Components without bindings pass `(\_ -> ())` and use `()` as the bindings type.

### Expression System
Expressions are `PropValue` variants resolved at render time:
- `$state` — read from state path (read-only)
- `$bindState` — read + write to state path (two-way binding)
- `$template` — string interpolation with `${/path}` syntax
- `$item` — read field from repeat context item
- `$index` — current repeat index
- `$cond` — conditional expression: `{ "$cond": <condition>, "$then": <value>, "$else": <value> }`. Condition is any PropValue evaluated for truthiness; `$then`/`$else` are PropValues resolved based on the result.
- `$computed` — registered transformation function: `{ "$computed": "funcName", "args": { ... } }`. Args can contain any expression type. Functions are registered in `FunctionDict` and threaded through the registry.

`$bindState` resolves to the current value (like `$state`) AND provides a setter function via `ComponentContext.bindings`. The setter produces `SetState path value` messages.

### Visibility Conditions
Elements can have a `visible` field with conditions matching the json-render core prompt format:
- `{ "$state": "/path" }` — truthy check
- `{ "$state": "/path", "not": true }` — falsy check
- `{ "$state": "/path", "eq": <value> }` — equals
- `{ "$state": "/path", "neq": <value> }` — not equals
- `{ "$and": [...] }` — explicit AND
- `{ "$or": [...] }` — OR
- `[cond, cond]` — implicit AND (list of conditions)
- `true` / `false` — always visible/hidden

### State on Spec
The JS bridge extracts the `state` field from incoming specs and sends it to Elm via the `jsonRenderStateIn` port as initial render state.

### Elm Schema vs React/Vue/Svelte
Our `defineSchema` in `packages/js-bridge/src/schema.ts` differs from other renderers:
- Three built-in actions: `setState`, `pushState`, `removeState` (no `validateForm`)
- Action params use `statePath` as the path parameter name (matching React renderer)
- `removeState` supports an `index` param for removing array items by index
- Expressions: `$state`, `$item`, `$index`, `$template`, `$bindState`, `$bindItem`, `$cond`, `$computed`
- Props error rendering: components show a visible error when props fail to decode instead of rendering nothing

### elm-review CatalogSync
The CatalogSync rule validates component modules against a catalog JSON Schema. It:
- Reports missing component modules with copy-pasteable stub commands
- Provides `--fix` to fill stubs with generated code (props type, bindings type, decoders, view placeholder)
- Generates registry modules with all components

### Two Entry Points in Demo
- `index.html` + `main.ts` + `Main.elm` — the demo app with Claude API integration
- `test-harness.html` + `test-harness.ts` + `TestHarness.elm` — minimal harness for Playwright tests

`TestHarness.elm` has no UI chrome. It shares the same `Components/Registry.elm`. It adds a `jsonRenderStateIn` port for test state injection (not present in Main.elm).

### Parity Test Harness
- `parity-harness.html` + `parity-harness.ts` — dual-mount page rendering specs through both Elm and React (`@json-render/react`) side-by-side
- `demo/src/react-components/` — 7 React component ports (Card, Text, Button, Input, Stack, Badge, Image) with identical `jr-` CSS classes and HTML structure
- `demo/test/parity-helpers.ts` — `sendParitySpec`, `setParityState`, `renderers`, `getParityLastAction`
- `demo/test/specs/parity/*.parity.ts` — 40 parity tests across 8 files

Run with: `cd demo && npx playwright test --project=parity`

## Testing Requirements

**Every new feature from the backlog MUST include both:**

1. **Elm unit tests** (`packages/elm-core/tests/`) — test each pipeline stage:
   - `SpecTest.elm` for JSON decoding
   - `ResolveTest.elm` for expression resolution
   - `VisibilityTest.elm` for visibility evaluation
   - `RenderTest.elm` for full pipeline integration (JSON → decode → render → assert Html)
   - `BindTest.elm` for binding combinators
   - `ActionsTest.elm` for state mutations

2. **Playwright e2e tests** (`demo/test/`) — test in a real browser:
   - Create JSON fixture files in `demo/test/fixtures/<category>/`
   - Create spec file in `demo/test/specs/<category>.spec.ts`
   - Use helpers: `sendSpec`, `setState`, `getLastAction`
   - Test pattern: `beforeEach` navigates to `""` (resolves to test harness), then send fixture + assert DOM

### Test Patterns

Elm integration tests decode from JSON strings (not constructed Elm records) to test the full pipeline:
```elm
case Decode.decodeString Spec.decoder json of
    Ok spec ->
        Render.render testRegistry state spec
            |> Query.fromHtml
            |> Query.has [ Selector.text "expected" ]
    Err err ->
        Expect.fail (Decode.errorToString err)
```

Playwright tests use `page.evaluate()` to send specs via the bridge:
```typescript
await setState(page, { key: "value" })
await sendSpec(page, "category/fixture.json")
await expect(page.locator(".jr-component")).toHaveText("expected")
```

## Component CSS Classes

All demo components use the `jr-` prefix:
- Card: `.jr-card`, `.jr-card-title`, `.jr-card-subtitle`, `.jr-card-body`
- Button: `.jr-button`, `.jr-button-primary`, `.jr-button-secondary`, `.jr-button-danger`
- Text: `.jr-text`, `.jr-text-sm`, `.jr-text-md`, `.jr-text-lg`
- Input: `.jr-input-wrapper`, `.jr-input-label`, `.jr-input`
- Stack: `.jr-stack`, `.jr-stack-vertical`, `.jr-stack-horizontal`
- Image: `.jr-image`
- Badge: `.jr-badge`

Note: the `testRegistry` in `RenderTest.elm` uses simplified class names (`.card`, not `.jr-card`). Playwright tests use the real demo component classes.

## Plan Execution Strategy

When implementing plans from brainstorming, dispatch three parallel agent teams split by package:

1. **elm-lib agent** — `packages/elm-core/` work (types, decoders, resolution, rendering, unit tests)
2. **elm-review agent** — `packages/elm-review/` work (schema parsing, code generation, CatalogSync rules)
3. **ts agent** — `packages/js-bridge/` + `demo/` work (schema, bridge, catalog, Playwright e2e tests, fixtures)

Each agent works in an isolated git worktree. After all three complete, integrate their branches and run the full test suite. Tasks that require all packages (e.g., e2e tests that need elm-core + elm-review + demo changes) are done in a final integration pass after merging.

## Feature Parity with json-render

Compared against the [json-render](https://github.com/nichochar/json-render) core framework (React renderer as reference).

### Fully Implemented

| Feature | Details |
|---|---|
| **Expressions** | All 8 types: `$state`, `$bindState`, `$item`, `$bindItem`, `$index`, `$template`, `$cond`, `$computed` |
| **Actions** | `setState`, `pushState`, `removeState` + custom actions via `ActionConfig` |
| **Chained actions** | Array of action bindings executed sequentially |
| **Event bindings** | `on` field mapping event names to action bindings |
| **Watchers** | `watch` field with state path triggers, repeat-aware |
| **Visibility** | `$state` conditions with `eq`, `neq`, `not`, `$and`, `$or`, implicit AND, boolean literals |
| **Repeat** | `repeat` field with `statePath` + optional `key` for keyed rendering |
| **State on spec** | `state` field on spec for initial state |
| **Catalog schema** | `defineSchema` with components, actions, functions + `generateCatalogSchema` export |
| **Props error rendering** | Red error box when props fail to decode |

### Not Yet Implemented

Gaps versus json-render core, grouped by area and roughly prioritized:

#### Visibility gaps
- **Comparison operators `gt`/`gte`/`lt`/`lte`** — only `eq`/`neq` are implemented
- **`$item` and `$index` conditions** — visibility only evaluates `$state`, not repeat context expressions
- **State-to-state comparison** — RHS of comparison operators is always a literal; upstream allows `{ "$state": "/path" }` as the comparand

#### Action gaps
- **`confirm` dialogs** — `{ "confirm": { "title": "...", "message": "...", "variant": "danger" } }` on action bindings to show a confirmation dialog before executing
- **`onSuccess` / `onError` handlers** — post-action callbacks: navigate, set state, or chain another action
- **`preventDefault` flag** — prevents default browser behavior on action trigger
- **`pushState` `clearStatePath` param** — clears a state path after push (e.g., reset form input)
- **`$id` auto-generation** — `"$id"` string in `pushState` value auto-generates a unique ID

#### Larger features
- **Named slots** — `ComponentContext props slots` with typed slot records decoded same way as props. The spec's `children` becomes `Dict String (List String)` keyed by slot name
- **Form validation** — `validateForm` action + validation checks (`required`, `email`, `minLength`, `pattern`, etc.), `validateOn` timing, cross-field validation, conditional validation, custom validators
- **Streaming / SpecStream** — JSONL + RFC 6902 JSON Patch incremental rendering instead of full spec snapshots
- **Edit modes** — `buildEditInstructions()`, `deepMergeSpec()`, `diffToPatches()` for spec editing workflows

### Known Behavioral Differences (Elm vs React)

Discovered via parity tests (`demo/test/specs/parity/`). These are behavioral divergences between the Elm and React renderers when given the same spec:

| Area | Elm Behavior | React Behavior |
|---|---|---|
| **Props error rendering** | Shows a red "Props error" box with the decode error message | Silently renders with `undefined` props (no visible error) |
| **Unknown `$computed` function** | Shows "Props error: Unknown function: X" | Returns `undefined`, renders empty content |
| **`$id` generation format** | 36-char UUID v4 (via `TSFoster/elm-uuid`) | Timestamp-based ID (`Date.now()-counter`, ~15 chars) |
| **Chained watcher actions** | All chained actions execute synchronously in order | First action triggers re-render, which cancels remaining chained actions via React's effect cleanup |

### Internal Improvements (not parity gaps)
- **ElmCodeGen AST refactor** — replace string concatenation in `ElmCodeGen.elm` with `the-sett/elm-syntax-dsl` for AST-based code generation. See `docs/superpowers/specs/2026-03-26-elm-codegen-refactor.md`

## Key Files

| File | Purpose |
|---|---|
| `packages/elm-core/src/JsonRender/Render.elm` | Core renderer: `ComponentContext`, `register`, `extractBindings`, `render` |
| `packages/elm-core/src/JsonRender/Resolve.elm` | Expression resolution + pipeline prop decoders |
| `packages/elm-core/src/JsonRender/Bind.elm` | Pipeline bindings decoders (`succeed`, `bindable`) |
| `packages/elm-core/src/JsonRender/Spec.elm` | Spec types + JSON decoder |
| `packages/elm-core/src/JsonRender/Internal/PropValue.elm` | Expression variants + decoder |
| `packages/elm-core/src/JsonRender/Visibility.elm` | Visibility conditions + evaluation |
| `packages/elm-core/src/JsonRender/Actions.elm` | Msg type + update for built-in actions |
| `packages/elm-core/src/JsonRender/State.elm` | JSON Pointer (RFC 6901) operations |
| `packages/js-bridge/src/schema.ts` | Elm-specific `defineSchema` for `@json-render/core` |
| `packages/js-bridge/src/index.ts` | `createElmBridge` + port wiring |
| `packages/elm-review/src/JsonRender/CatalogSync.elm` | elm-review rule for catalog sync |
| `packages/elm-review/src/JsonRender/Internal/ElmCodeGen.elm` | Generates Elm code for components |
| `demo/catalog.ts` | Component catalog (single source of truth for AI prompts) |
| `demo/src/TestHarness.elm` | Minimal Elm app for Playwright tests |
| `demo/test/helpers.ts` | Playwright test helpers |
