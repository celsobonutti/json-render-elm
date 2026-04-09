/**
 * Component catalog definition for the json-render-elm demo.
 *
 * Uses @json-render/elm's schema with defineCatalog — the single source of truth.
 * The catalog provides:
 *   - catalog.prompt()      → system prompt for Claude
 *   - catalog.jsonSchema()  → JSON Schema for the spec
 *   - catalog.validate()    → validate AI-generated specs
 */

import { defineCatalog } from "@json-render/core";
import { schema } from "../packages/js-bridge/src/schema.ts";
import { z } from "zod";

export const catalog = defineCatalog(schema, {
  components: {
    Card: {
      props: z.object({
        title: z.string(),
        subtitle: z.string().optional(),
      }),
      slots: ["default"],
      bindable: [],
      validatable: [],
      description: "A card container with a title and optional subtitle",
    },
    Button: {
      props: z.object({
        label: z.string(),
        variant: z.enum(["primary", "secondary", "danger"]).optional(),
      }),
      slots: [],
      bindable: [],
      validatable: [],
      description: "A clickable button",
    },
    Text: {
      props: z.object({
        content: z.string(),
        size: z.enum(["sm", "md", "lg", "xl"]).optional(),
      }),
      slots: [],
      bindable: [],
      validatable: [],
      description: "A text block",
    },
    Input: {
      props: z.object({
        placeholder: z.string().optional(),
        label: z.string().optional(),
        value: z.string(),
      }),
      slots: [],
      bindable: ["value"],
      validatable: ["value"],
      description:
        "A text input field with an optional label. Use { $bindState } on value for two-way binding. Use checks for validation.",
    },
    Stack: {
      props: z.object({
        direction: z.enum(["vertical", "horizontal"]).optional(),
        gap: z.number().int().optional(),
      }),
      slots: ["default"],
      bindable: [],
      validatable: [],
      description:
        "A flex container that stacks children vertically or horizontally",
    },
    Image: {
      props: z.object({
        src: z.string(),
        alt: z.string(),
      }),
      slots: [],
      bindable: [],
      validatable: [],
      description: "An image element",
    },
    Badge: {
      props: z.object({
        label: z.string(),
        color: z.enum(["green", "red", "yellow", "blue", "gray"]).optional(),
      }),
      slots: [],
      bindable: [],
      validatable: [],
      description: "A small colored badge/tag",
    },
    Toggle: {
      props: z.object({
        label: z.string(),
      }),
      slots: [],
      bindable: [],
      validatable: [],
      description: "A toggle switch with local open/closed state",
    },
    Select: {
      props: z.object({
        label: z.string().optional(),
        placeholder: z.string().optional(),
        options: z.array(z.string()),
        value: z.string(),
      }),
      slots: [],
      bindable: ["value"],
      validatable: [],
      description:
        "A filterable dropdown select with local state for open/close and search query. Use { $bindState } on value for two-way binding.",
    },
  },
  actions: {
    press: {
      params: z.object({}),
      description: "Generic button press",
    },
    export: {
      params: z.object({ format: z.string() }),
      description: "Export data in a given format",
    },
  },
  functions: {
    shout: {
      params: z.object({ text: z.string() }),
      returnType: z.string(),
      description: "Convert text to uppercase",
    },
    add: {
      params: z.object({ a: z.number(), b: z.number() }),
      returnType: z.number(),
      description: "Add two numbers together",
    },
  },
});
