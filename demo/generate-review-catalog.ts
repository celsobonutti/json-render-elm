#!/usr/bin/env npx tsx
/**
 * Generates review/src/CatalogData.elm from catalog-schema.json.
 * Embeds the catalog schema as a compact Elm string for ReviewConfig.elm.
 *
 * Usage: npx tsx generate-catalog-schema.ts && npx tsx generate-review-catalog.ts
 * Or:    npm run catalog
 */
import { readFileSync, writeFileSync } from "fs"

const schema = JSON.parse(readFileSync("catalog-schema.json", "utf-8"))
const compact = JSON.stringify(schema)
const escaped = compact.replace(/\\/g, "\\\\").replace(/"/g, '\\"')

const elm = `module CatalogData exposing (schemaJson)


schemaJson : String
schemaJson =
    "${escaped}"
`

writeFileSync("review/src/CatalogData.elm", elm)
console.log("Generated review/src/CatalogData.elm")
