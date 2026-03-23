#!/usr/bin/env npx tsx
/**
 * Generates catalog-schema.json from the catalog definition.
 * Run this whenever you change catalog.ts:
 *
 *   npx tsx generate-catalog-schema.ts
 *
 * Note: Uses zod-to-json-schema because catalog.jsonSchema() returns {}
 * due to an upstream issue in @json-render/core's internal zodToJsonSchema.
 * When that's fixed, this can simplify to just `catalog.jsonSchema()`.
 */
import { writeFileSync } from "fs"
import { zodToJsonSchema } from "zod-to-json-schema"
import { catalog } from "./catalog.ts"

interface CatalogSchemaComponent {
  props: object
  description: string
  hasChildren: boolean
}

const components: Record<string, CatalogSchemaComponent> = {}
for (const name of catalog.componentNames) {
  const def = catalog.data.components[name as keyof typeof catalog.data.components]
  components[name] = {
    props: zodToJsonSchema(def.props, { target: "openApi3" }),
    description: def.description ?? "",
    hasChildren: def.hasChildren ?? false,
  }
}

writeFileSync("catalog-schema.json", JSON.stringify({ components }, null, 2) + "\n")
console.log("Generated catalog-schema.json")
