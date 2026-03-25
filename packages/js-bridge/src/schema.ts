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
 * - No `on` field on elements — actions are handled via Elm's `emit` + ports
 * - No `state` on spec — Elm model owns state
 * - Expressions: $state, $item, $index, $template (read-only), $bindState, $bindItem (two-way binding)
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
          /** Repeat over a state array: { statePath: "/path", key?: "fieldName" } */
          repeat: s.any(),
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
          "Append an item to an array in state. Params: { path: string, value: any }",
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

      // Expression support
      'Dynamic props: use { "$state": "/path" } to read from state, { "$bindState": "/path" } for two-way binding (read and write), { "$item": "field" } to read from repeat item (use "" for whole item), { "$index": true } for repeat index, { "$bindItem": "field" } for two-way binding to repeat item field, { "$template": "Hello ${/name}" } for string interpolation.',
      'Use "repeat": { "statePath": "/arrayPath", "key": "id" } on an element to render its children once per array item. Props on child elements can use $item and $bindItem to access item fields. The "key" field is optional but recommended for stable rendering when items change.',

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
