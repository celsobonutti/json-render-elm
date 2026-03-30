# json-render-elm

Elm renderer for the [json-render](https://json-render.dev) generative UI framework. AI generates JSON specs constrained to a component catalog; this project decodes, evaluates, and renders them in Elm.

## How It Works

1. You define a **component catalog** with typed props (using Zod schemas)
2. An AI generates a **JSON spec** — a flat tree of elements referencing catalog components
3. The JS bridge sends the spec to Elm via **ports**
4. Elm **decodes**, **resolves expressions**, evaluates **visibility**, and **renders** to Html

```
AI → JSON spec → JS bridge → Elm port → Decode → Resolve → Render → Html
```

## Packages

| Package | Description |
|---|---|
| [`packages/elm-core`](packages/elm-core) | Elm package — types, decoding, expression resolution, rendering |
| [`packages/js-bridge`](packages/js-bridge) | npm package (`@json-render/elm`) — ports, bridge, Elm-specific schema |
| [`packages/elm-review`](packages/elm-review) | elm-review rules — validates and generates component modules from catalog |
| [`demo`](demo) | Vite + Elm demo app with Claude API integration and Playwright tests |

## Quick Start

```bash
# Run the demo
cd demo && npm install && npm run dev

# Run Elm unit tests
cd packages/elm-core && npx elm-test

# Run Playwright e2e tests
cd demo && npm run test:e2e

# Run parity tests (Elm vs React renderer comparison)
cd demo && npx playwright test --project=parity
```

## Usage

### 1. Define a Catalog

```typescript
// catalog.ts
import { defineCatalog } from "@json-render/core";
import { schema } from "@json-render/elm";
import { z } from "zod";

export const catalog = defineCatalog(schema, {
  components: {
    Button: {
      props: z.object({
        label: z.string(),
        variant: z.enum(["primary", "secondary", "danger"]).optional(),
      }),
      slots: [],
      description: "A clickable button",
    },
    Card: {
      props: z.object({
        title: z.string(),
        subtitle: z.string().optional(),
      }),
      slots: ["default"],
      description: "A card container with a title",
    },
  },
  actions: {
    press: {
      params: z.object({}),
      description: "Generic button press",
    },
  },
});
```

### 2. Register Elm Components

Each component registers a **props decoder**, a **bindings decoder**, and a **view function**:

```elm
module Components.Button exposing (component)

import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)

type alias ButtonProps =
    { label : String
    , variant : Maybe String
    }

propsDecoder =
    ResolvedValue.succeed ButtonProps
        |> ResolvedValue.required "label" ResolvedValue.string
        |> ResolvedValue.optional "variant" ResolvedValue.string Nothing

component : Component Action
component =
    register propsDecoder (\_ -> ()) view

view ctx =
    button
        [ class "btn"
        , onClick (ctx.emit Press)
        ]
        [ text ctx.props.label ]
```

Components with two-way bindings use `Bind` for the bindings decoder:

```elm
import JsonRender.Bind as Bind

type alias InputBindings =
    { value : Maybe (Value -> Msg Action) }

bindingsDecoder =
    Bind.succeed InputBindings
        |> Bind.bindable "value"
```

### 3. Build a Registry

```elm
module Components.Registry exposing (registry)

import Dict
import JsonRender.Render exposing (Registry)

registry : Registry Action
registry =
    Dict.fromList
        [ ( "Button", Components.Button.component )
        , ( "Card", Components.Card.component )
        ]
```

### 4. Wire Up the Bridge

```typescript
// main.ts
import { createElmBridge } from "@json-render/elm";

const app = Elm.Main.init({ node: document.getElementById("app") });
const bridge = createElmBridge(app);

// Send a complete spec
bridge.sendSpec(spec);

// Or stream from an AI response
const stream = await bridge.createStream();
for await (const chunk of aiResponse) {
  stream.push(chunk);
}
```

### 5. Render in Elm

```elm
import JsonRender.Actions as Actions
import JsonRender.Render as Render
import JsonRender.Spec as Spec

-- In your view
case model.spec of
    Just spec ->
        Render.render registry model.state spec
            |> Html.map JsonRenderMsg

    Nothing ->
        text "No spec loaded"
```

## Expression System

Props can be dynamic expressions resolved at render time:

| Expression | JSON | Description |
|---|---|---|
| `$state` | `{ "$state": "/path" }` | Read from state |
| `$bindState` | `{ "$bindState": "/path" }` | Two-way binding (read + write) |
| `$template` | `{ "$template": "Hello ${/name}" }` | String interpolation |

State paths follow [RFC 6901 JSON Pointer](https://www.rfc-editor.org/rfc/rfc6901) syntax.

## Built-in Actions

| Action | Description |
|---|---|
| `setState` | Set a value at a state path |
| `pushState` | Append to an array at a state path |
| `removeState` | Remove a value at a state path |

## Visibility Conditions

Elements can be conditionally shown using the `visible` field:

```json
{
  "type": "Text",
  "props": { "content": "Logged in" },
  "visible": { "truthy": "/user/isLoggedIn" }
}
```

Supported conditions: `truthy`, `equals`, `notEquals`, `not`, `and`, `or`.

## Parity Testing

Parity tests verify that the Elm renderer produces the same output as the React renderer (`@json-render/react`) for the same JSON specs. A dual-mount harness renders each spec through both renderers side-by-side, and shared Playwright assertions check both DOMs.

```bash
cd demo && npx playwright test --project=parity
```

40 tests cover rendering, expressions, visibility, repeat, actions, watchers, state-on-spec, and error rendering.

### Known Behavioral Differences

| Area | Elm | React |
|---|---|---|
| Props error rendering | Visible red error box | Silent (renders `undefined`) |
| Unknown `$computed` function | Visible error message | Returns `undefined` |
| `$id` generation | UUID v4 (36 chars) | Timestamp-based (`Date.now()-counter`) |
| Chained watcher actions | All execute in order | Re-render cancels remaining chain |

## elm-review CatalogSync

The `JsonRender.CatalogSync` rule keeps your Elm component modules in sync with the catalog JSON Schema:

```bash
# Generate catalog schema
cd demo && npm run catalog

# Auto-fix: generate missing component stubs
cd demo && npm run elm-review:fix
```

It reports missing component modules and can auto-generate stubs with the correct props types, bindings types, decoders, and view placeholders.
