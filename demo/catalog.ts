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
      description: "A card container with a title and optional subtitle",
    },
    Button: {
      props: z.object({
        label: z.string(),
        variant: z.enum(["primary", "secondary", "danger"]).optional(),
      }),
      slots: [],
      description: "A clickable button",
    },
    Text: {
      props: z.object({
        content: z.string(),
        size: z.enum(["sm", "md", "lg", "xl"]).optional(),
      }),
      slots: [],
      description: "A text block",
    },
    Input: {
      props: z.object({
        placeholder: z.string().optional(),
        label: z.string().optional(),
        value: z.string().optional(),
        checks: z
          .array(
            z.object({
              type: z.string(),
              message: z.string(),
              args: z.record(z.string(), z.unknown()).optional(),
            }),
          )
          .optional(),
        validateOn: z.enum(["change", "blur", "submit"]).optional(),
      }),
      slots: [],
      description:
        "A text input field with an optional label. Use { $bindState } on value for two-way binding. Use checks for validation (e.g. required, email, minLength). validateOn controls timing (default: submit).",
    },
    Stack: {
      props: z.object({
        direction: z.enum(["vertical", "horizontal"]).optional(),
        gap: z.number().int().optional(),
      }),
      slots: ["default"],
      description:
        "A flex container that stacks children vertically or horizontally",
    },
    Image: {
      props: z.object({
        src: z.string(),
        alt: z.string(),
      }),
      slots: [],
      description: "An image element",
    },
    Badge: {
      props: z.object({
        label: z.string(),
        color: z.enum(["green", "red", "yellow", "blue", "gray"]).optional(),
      }),
      slots: [],
      description: "A small colored badge/tag",
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
  },
});
