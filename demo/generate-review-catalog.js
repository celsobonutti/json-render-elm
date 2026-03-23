#!/usr/bin/env node
/**
 * Generates review/src/CatalogData.elm from catalog-schema.json.
 * This embeds the catalog schema as an Elm string so ReviewConfig.elm can use it.
 *
 * Run after generate-catalog-schema.js:
 *   node generate-catalog-schema.js && node generate-review-catalog.js
 *
 * Or just:
 *   npm run catalog
 */
import { readFileSync, writeFileSync } from "fs"
import { toCatalogSchema } from "./catalog.js"

// Compact JSON on a single line — no newlines to worry about in Elm strings
const schema = JSON.stringify(toCatalogSchema())
// Escape backslashes and double quotes for Elm string literal
const escaped = schema.replace(/\\/g, "\\\\").replace(/"/g, '\\"')

const elm = `module CatalogData exposing (schemaJson)


schemaJson : String
schemaJson =
    "${escaped}"
`

writeFileSync("review/src/CatalogData.elm", elm)
console.log("Generated review/src/CatalogData.elm")
