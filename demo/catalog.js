/**
 * Component catalog definition for the json-render-elm demo.
 *
 * This is the single source of truth for what components the AI can use.
 * It produces:
 *   1. The system prompt for Claude (describing available components)
 *   2. The JSON schema for structured output (guaranteeing valid specs)
 *   3. The catalog-schema.json consumed by elm-review's CatalogSync rule
 */

export const components = {
  Card: {
    props: {
      type: "object",
      properties: {
        title: { type: "string" },
        subtitle: { type: "string" },
      },
      required: ["title"],
    },
    description: "A card container with a title and optional subtitle",
    hasChildren: true,
  },
  Button: {
    props: {
      type: "object",
      properties: {
        label: { type: "string" },
        variant: { type: "string", enum: ["primary", "secondary", "danger"] },
      },
      required: ["label"],
    },
    description: "A clickable button",
    hasChildren: false,
  },
  Text: {
    props: {
      type: "object",
      properties: {
        content: { type: "string" },
        size: { type: "string", enum: ["sm", "md", "lg", "xl"] },
      },
      required: ["content"],
    },
    description: "A text block",
    hasChildren: false,
  },
  Input: {
    props: {
      type: "object",
      properties: {
        placeholder: { type: "string" },
        label: { type: "string" },
      },
      required: [],
    },
    description: "A text input field with an optional label",
    hasChildren: false,
  },
  Stack: {
    props: {
      type: "object",
      properties: {
        direction: { type: "string", enum: ["vertical", "horizontal"] },
        gap: { type: "integer" },
      },
      required: [],
    },
    description: "A flex container that stacks children vertically or horizontally",
    hasChildren: true,
  },
  Image: {
    props: {
      type: "object",
      properties: {
        src: { type: "string" },
        alt: { type: "string" },
      },
      required: ["src", "alt"],
    },
    description: "An image element",
    hasChildren: false,
  },
  Badge: {
    props: {
      type: "object",
      properties: {
        label: { type: "string" },
        color: { type: "string", enum: ["green", "red", "yellow", "blue", "gray"] },
      },
      required: ["label"],
    },
    description: "A small colored badge/tag",
    hasChildren: false,
  },
}

/** Build the system prompt for Claude from the catalog. */
export function buildSystemPrompt() {
  const componentDocs = Object.entries(components)
    .map(([name, def]) => {
      const props = Object.entries(def.props.properties)
        .map(([prop, schema]) => {
          const isRequired = def.props.required.includes(prop)
          const type = schema.enum
            ? schema.enum.map((v) => `"${v}"`).join("|")
            : schema.type
          return `${prop}${isRequired ? "" : "?"}: ${type}`
        })
        .join(", ")
      const children = def.hasChildren ? ", has children" : ""
      return `- ${name}: props { ${props} }${children}`
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
- Generate a complete, visually interesting UI that fulfills the user's request`
}

/** JSON Schema for Claude's structured output — guarantees a valid spec. */
export const specSchema = {
  type: "object",
  properties: {
    root: { type: "string" },
    elements: {
      type: "object",
      additionalProperties: {
        type: "object",
        properties: {
          type: { type: "string" },
          props: { type: "object" },
          children: { type: "array", items: { type: "string" } },
        },
        required: ["type", "props", "children"],
        additionalProperties: false,
      },
    },
  },
  required: ["root", "elements"],
  additionalProperties: false,
}

/** The catalog in the format elm-review's CatalogSync rule expects. */
export function toCatalogSchema() {
  return { components }
}
