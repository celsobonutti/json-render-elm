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
- `$item` — read field from repeat context item (requires `repeat`, not yet implemented)
- `$index` — current repeat index (requires `repeat`, not yet implemented)

`$bindState` resolves to the current value (like `$state`) AND provides a setter function via `ComponentContext.bindings`. The setter produces `SetState path value` messages.

### Visibility Conditions
Elements can have a `visible` field with conditions: `Truthy`, `Equals`, `NotEquals`, `Not`, `And`, `Or`. JSON shape: `{ "truthy": "/path" }`, `{ "equals": { "path": "...", "value": ... } }`, `{ "and": [...] }`, etc.

### Elm Schema vs React/Vue/Svelte
Our `defineSchema` in `packages/js-bridge/src/schema.ts` differs from other renderers:
- No `on` field on elements — actions via `emit` + ports
- No `repeat` field — not yet supported
- No `state` on spec — Elm model owns state
- Three built-in actions: `setState`, `pushState`, `removeState` (no `validateForm`)
- Expressions: `$state`, `$item`, `$index`, `$template`, `$bindState` (no `$computed`, no `$bindItem`)

### elm-review CatalogSync
The CatalogSync rule validates component modules against a catalog JSON Schema. It:
- Reports missing component modules with copy-pasteable stub commands
- Provides `--fix` to fill stubs with generated code (props type, bindings type, decoders, view placeholder)
- Generates registry modules with all components

### Two Entry Points in Demo
- `index.html` + `main.ts` + `Main.elm` — the demo app with Claude API integration
- `test-harness.html` + `test-harness.ts` + `TestHarness.elm` — minimal harness for Playwright tests

`TestHarness.elm` has no UI chrome. It shares the same `Components/Registry.elm`. It adds a `jsonRenderStateIn` port for test state injection (not present in Main.elm).

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

## Backlog

Features not yet implemented, roughly prioritized:

- **`repeat`** — iterate over state arrays with `repeat: { statePath, key }` on elements. Required before `$item`/`$index` work in practice.
- **`$bindItem`** — two-way binding within repeat contexts. Implement alongside `repeat`.
- **Named slots** — `ComponentContext props slots` with typed slot records decoded same way as props. The spec's `children` becomes `Dict String (List String)` keyed by slot name.
- **`$computed`** — registered transformation functions
- **Form validation** — `validateForm` built-in action
- **Streaming optimizations** — incremental patches instead of full spec snapshots

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
