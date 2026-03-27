#!/usr/bin/env npx tsx
/**
 * Generates catalog-schema.json from the catalog definition.
 *
 *   npx tsx generate-catalog-schema.ts
 */
import { writeFileSync } from "fs"
import { catalog } from "./catalog.ts"
import { generateCatalogSchema } from "../packages/js-bridge/src/index.ts"

const schema = generateCatalogSchema(catalog)

writeFileSync("catalog-schema.json", JSON.stringify(schema, null, 2) + "\n")
console.log("Generated catalog-schema.json")
