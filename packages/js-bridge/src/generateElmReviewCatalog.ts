/**
 * Generates an Elm module that embeds a catalog schema as a string constant.
 * Used by elm-review's CatalogSync rule to validate component modules.
 *
 * @param schema - The catalog schema object (output of generateCatalogSchema)
 * @returns The Elm module source code as a string
 */
export function generateElmReviewCatalog(schema: Record<string, unknown>): string {
  const compact = JSON.stringify(schema)
  const escaped = compact.replace(/\\/g, '\\\\').replace(/"/g, '\\"')

  return `module CatalogData exposing (schemaJson)


schemaJson : String
schemaJson =
    "${escaped}"
`
}
