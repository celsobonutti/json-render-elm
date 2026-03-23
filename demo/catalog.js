/**
 * Component catalog definition for the json-render-elm demo.
 *
 * Uses @json-render/core with Zod schemas as the single source of truth.
 * Produces:
 *   1. The system prompt for Claude (flat spec format for our Elm renderer)
 *   2. The catalog-schema.json consumed by elm-review's CatalogSync rule
 */

import { createCatalog } from "@json-render/core"
import { z } from "zod"
import { zodToJsonSchema } from "zod-to-json-schema"

export const catalog = createCatalog({
  name: "json-render-elm-demo",
  components: {
    Card: {
      props: z.object({
        title: z.string(),
        subtitle: z.string().optional(),
      }),
      description: "A card container with a title and optional subtitle",
      hasChildren: true,
    },
    Button: {
      props: z.object({
        label: z.string(),
        variant: z.enum(["primary", "secondary", "danger"]).optional(),
      }),
      description: "A clickable button",
    },
    Text: {
      props: z.object({
        content: z.string(),
        size: z.enum(["sm", "md", "lg", "xl"]).optional(),
      }),
      description: "A text block",
    },
    Input: {
      props: z.object({
        placeholder: z.string().optional(),
        label: z.string().optional(),
      }),
      description: "A text input field with an optional label",
    },
    Stack: {
      props: z.object({
        direction: z.enum(["vertical", "horizontal"]).optional(),
        gap: z.number().int().optional(),
      }),
      description: "A flex container that stacks children vertically or horizontally",
      hasChildren: true,
    },
    Image: {
      props: z.object({
        src: z.string(),
        alt: z.string(),
      }),
      description: "An image element",
    },
    Badge: {
      props: z.object({
        label: z.string(),
        color: z.enum(["green", "red", "yellow", "blue", "gray"]).optional(),
      }),
      description: "A small colored badge/tag",
    },
  },
})

/**
 * Build the system prompt for Claude.
 * Uses the catalog's Zod schemas to describe available components,
 * but targets our Elm renderer's flat spec format (not json-render's JSONL streaming).
 */
export function buildSystemPrompt() {
  const componentDocs = catalog.componentNames
    .map((name) => {
      const def = catalog.components[name]
      const schema = zodToJsonSchema(def.props, { target: "openApi3" })
      const props = Object.entries(schema.properties || {})
        .map(([prop, propSchema]) => {
          const isRequired = (schema.required || []).includes(prop)
          const type = propSchema.enum
            ? propSchema.enum.map((v) => `"${v}"`).join("|")
            : propSchema.type
          return `${prop}${isRequired ? "" : "?"}: ${type}`
        })
        .join(", ")
      const children = def.hasChildren ? ", has children" : ""
      return `- ${name}: props { ${props} }${children}\n  ${def.description}`
    })
    .join("\n")

  return `You generate json-render UI specs from user descriptions.

Spec format:
{
  "root": "<element-id>",
  "elements": {
    "<element-id>": {
      "type": "<ComponentName>",
      "props": { ... },
      "children": ["<child-id>", ...]
    }
  }
}

Available components:
${componentDocs}

Rules:
- Element IDs must be unique strings (e.g., "card-1", "text-1")
- Only use components listed above
- Children array contains element IDs, not objects
- Components without children must have an empty children array: []
- For images, use placeholder URLs from https://placehold.co (e.g., https://placehold.co/600x400)
- Generate a complete, visually interesting UI that fulfills the user's request
- Respond with ONLY valid JSON. No markdown, no explanation, no code fences.`
}

/**
 * Export the catalog as JSON Schema for elm-review's CatalogSync rule.
 * Converts each component's Zod props schema to JSON Schema.
 */
export function toCatalogSchema() {
  const components = {}
  for (const name of catalog.componentNames) {
    const def = catalog.components[name]
    components[name] = {
      props: zodToJsonSchema(def.props, { target: "openApi3" }),
      description: def.description || "",
      hasChildren: def.hasChildren || false,
    }
  }
  return { components }
}
