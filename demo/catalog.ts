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

export const catalog: ReturnType<typeof defineCatalog> = defineCatalog(schema, {
  components: {
    Card: {
      props: z.object({
        title: z.string(),
        subtitle: z.string().optional(),
      }),
      hasChildren: true,
      description: "A card container with a title and optional subtitle",
    },
    Button: {
      props: z.object({
        label: z.string(),
        variant: z.enum(["primary", "secondary", "danger"]).optional(),
      }),
      hasChildren: false,
      description: "A clickable button",
    },
    Text: {
      props: z.object({
        content: z.string(),
        size: z.enum(["sm", "md", "lg", "xl"]).optional(),
      }),
      hasChildren: false,
      description: "A text block",
    },
    Input: {
      props: z.object({
        placeholder: z.string().optional(),
        label: z.string().optional(),
      }),
      hasChildren: false,
      description: "A text input field with an optional label",
    },
    Stack: {
      props: z.object({
        direction: z.enum(["vertical", "horizontal"]).optional(),
        gap: z.number().int().optional(),
      }),
      hasChildren: true,
      description:
        "A flex container that stacks children vertically or horizontally",
    },
    Image: {
      props: z.object({
        src: z.string(),
        alt: z.string(),
      }),
      hasChildren: false,
      description: "An image element",
    },
    Badge: {
      props: z.object({
        label: z.string(),
        color: z.enum(["green", "red", "yellow", "blue", "gray"]).optional(),
      }),
      hasChildren: false,
      description: "A small colored badge/tag",
    },
  },
  actions: {},
});
