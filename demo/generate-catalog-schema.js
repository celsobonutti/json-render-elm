#!/usr/bin/env node
/**
 * Generates catalog-schema.json from the catalog definition.
 * Run this whenever you change catalog.js:
 *
 *   node generate-catalog-schema.js
 */
import { writeFileSync } from "fs"
import { toCatalogSchema } from "./catalog.js"

const schema = toCatalogSchema()
writeFileSync("catalog-schema.json", JSON.stringify(schema, null, 2) + "\n")
console.log("Generated catalog-schema.json")
