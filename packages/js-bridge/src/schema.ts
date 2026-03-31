import { defineSchema } from "@json-render/core";

/**
 * The schema for @json-render/elm
 *
 * Defines:
 * - Spec: A flat tree of elements with keys, types, props, and children references
 * - Catalog: Components with props schemas, and optional actions
 *
 * Differences from React/Vue/Svelte schemas:
 * - No `slots` — uses `hasChildren` boolean instead
 * - No `repeat` field — not yet supported (backlog)
 * - Expressions: $state, $item, $index, $template (read-only), $bindState (two-way binding), $computed (registered functions)
 */
export const schema = defineSchema(
  (s) => ({
    // What the AI-generated SPEC looks like
    spec: s.object({
      /** Root element key */
      root: s.string(),
      /** Flat map of elements by key */
      elements: s.record(
        s.object({
          /** Component type from catalog */
          type: s.ref("catalog.components"),
          /** Component props */
          props: s.propsOf("catalog.components"),
          /** Child element keys (flat reference) */
          children: s.array(s.string()),
          /** Visibility condition */
          visible: s.any(),
          /** Event handlers mapping event names to action bindings */
          on: s.any(),
          /** Watchers mapping state paths to action bindings that fire on change */
          watch: s.any(),
        }),
      ),
    }),

    // What the CATALOG must provide
    catalog: s.object({
      /** Component definitions */
      components: s.map({
        /** Zod schema for component props */
        props: s.zod(),
        /** Slots for this component. Use ['default'] for children, or named slots like ['header', 'footer'] */
        slots: s.array(s.string()),
        /** Description for AI generation hints */
        description: s.string(),
        /** Example prop values used in prompt examples (auto-generated from Zod schema if omitted) */
        example: s.any(),
      }),
      /** Action definitions (optional) */
      actions: s.map({
        /** Zod schema for action params */
        params: s.zod(),
        /** Description for AI generation hints */
        description: s.string(),
      }),
      /** Computed function definitions (optional) */
      functions: s.map({
        /** Zod schema for function params */
        params: s.zod(),
        /** Zod schema for return type */
        returnType: s.zod(),
        /** Description for AI generation hints */
        description: s.string(),
      }),
    }),
  }),
  {
    builtInActions: [
      {
        name: "setState",
        description:
          "Update a value in state at the given path. Params: { path: string, value: any }",
      },
      {
        name: "pushState",
        description:
          'Append an item to an array in state. Params: { path: string, value: any, clearStatePath?: string }. Use "$id" as a string value inside value to auto-generate a unique ID. Use clearStatePath to reset a state path (e.g., an input field) to "" after appending.',
      },
      {
        name: "removeState",
        description:
          "Remove a value from state at the given path. Params: { path: string }",
      },
    ],
    defaultRules: [
      // Element integrity
      "CRITICAL: Before outputting ANY element that references children, you MUST output each child as its own element. If an element has children: ['a', 'b'], then elements 'a' and 'b' MUST exist. A missing child element causes that branch to be invisible.",
      "SELF-CHECK: After generating all elements, walk the tree from root. Every key in every children array must resolve to a defined element.",

      // Field placement
      'The "visible" field goes on the ELEMENT object, NOT inside "props". Correct: {"type":"Card","props":{},"visible":{"$state":"/show"},"children":[...]}.',
      'Visibility conditions: sources are { "$state": "/path" }, { "$item": "field" } (in repeat), or { "$index": true } (in repeat). Operators: "eq", "neq" (any value), "gt", "gte", "lt", "lte" (numeric). Right-hand side can be a literal or { "$state": "/otherPath" }. Invert with "not": true. Combine with { "$and": [...] }, { "$or": [...] }, or an array (implicit AND). Boolean true/false for always/never.',
      'The "on" field goes on the ELEMENT object, NOT inside "props". Use it to bind events to actions: {"on":{"press":{"action":"setState","params":{"path":"/clicked","value":true}}}}. Chain multiple actions with an array: {"on":{"press":[{"action":"setState",...},{"action":"myAction",...}]}}.',
      'The "watch" field goes on the ELEMENT object. It maps state paths to actions that fire when the value at that path changes (not on initial render). Same format as "on" actions: {"watch":{"/form/country":{"action":"setState","params":{"path":"/form/city","value":""}}}}. Use for cascading updates and derived state.',

      // Expression support
      'Dynamic props: use { "$state": "/path" } to read from state, { "$bindState": "/path" } for two-way binding (read and write), { "$item": "field" } in repeat contexts, { "$index": true } for loop index, { "$template": "Hello ${/name}" } for string interpolation, { "$computed": "funcName", "args": { "argName": <expression> } } for computed values from registered functions.',

      // Props integrity
      "Every prop listed WITHOUT a `?` suffix is REQUIRED and must always be provided — even inside repeat contexts. Omitting a required prop causes a visible error.",

      // Design quality
      "Design with visual hierarchy: use container components to group content, proper spacing, and status indicators. ONLY use components from the AVAILABLE COMPONENTS list.",
      "Always include realistic, professional-looking sample data. Never leave data empty.",
    ],
  },
);

/**
 * Type for the Elm schema
 */
export type ElmSchema = typeof schema;

/**
 * Infer the spec type from a catalog
 */
export type ElmSpec<TCatalog> = typeof schema extends {
  createCatalog: (catalog: TCatalog) => { _specType: infer S };
}
  ? S
  : never;
