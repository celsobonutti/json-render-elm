/**
 * Generates a JSON Schema representation of a catalog definition.
 * Converts Zod prop/param schemas to JSON Schema via toJSONSchema().
 *
 * @param catalog - A catalog instance from @json-render/core's defineCatalog()
 * @returns A plain object with components, actions, and optionally functions
 */
export function generateCatalogSchema(catalog: {
  componentNames: string[]
  data: {
    components: Record<string, { props: { toJSONSchema(): object }; description?: string; slots?: string[] }>
    actions: Record<string, { params: { toJSONSchema(): object }; description?: string }>
    functions?: Record<string, { params: { toJSONSchema(): object }; returnType: { toJSONSchema(): object }; description?: string }>
  }
}): Record<string, unknown> {
  const components: Record<string, { props: object; description: string; slots: string[] }> = {}
  for (const name of catalog.componentNames) {
    const def = catalog.data.components[name]
    if (!def) continue
    components[name] = {
      props: def.props.toJSONSchema(),
      description: def.description ?? '',
      slots: def.slots ?? [],
    }
  }

  const actions: Record<string, { params: object; description: string }> = {}
  for (const name of Object.keys(catalog.data.actions)) {
    const def = catalog.data.actions[name]
    if (!def) continue
    actions[name] = {
      params: def.params.toJSONSchema(),
      description: def.description ?? '',
    }
  }

  const output: Record<string, unknown> = { components, actions }

  if (catalog.data.functions) {
    const functions: Record<string, { params: object; returnType: object; description: string }> = {}
    for (const name of Object.keys(catalog.data.functions)) {
      const def = catalog.data.functions[name]
      if (!def) continue
      functions[name] = {
        params: def.params.toJSONSchema(),
        returnType: def.returnType.toJSONSchema(),
        description: def.description ?? '',
      }
    }
    if (Object.keys(functions).length > 0) {
      output.functions = functions
    }
  }

  return output
}
