#!/usr/bin/env npx tsx
/**
 * Generates review/src/CatalogData.elm from catalog-schema.json.
 *
 * Usage: npx tsx generate-catalog-schema.ts && npx tsx generate-review-catalog.ts
 * Or:    npm run catalog
 */
import { readFileSync, writeFileSync } from "fs"
import { generateElmReviewCatalog } from "../packages/js-bridge/src/index.ts"

const schema = JSON.parse(readFileSync("catalog-schema.json", "utf-8"))
const elm = generateElmReviewCatalog(schema)

writeFileSync("review/src/CatalogData.elm", elm)
console.log("Generated review/src/CatalogData.elm")
