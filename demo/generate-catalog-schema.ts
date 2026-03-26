#!/usr/bin/env npx tsx
/**
 * Generates catalog-schema.json from the catalog definition.
 * Uses Zod v4's built-in toJSONSchema() for prop schemas.
 *
 *   npx tsx generate-catalog-schema.ts
 */
import { writeFileSync } from "fs";
import { catalog } from "./catalog.ts";

interface CatalogSchemaComponent {
  props: object;
  description: string;
  slots: string[];
}

interface CatalogSchemaAction {
  params: object;
  description: string;
}

interface CatalogSchemaFunction {
  params: object;
  returnType: object;
  description: string;
}

const components: Record<string, CatalogSchemaComponent> = {};
for (const name of catalog.componentNames) {
  const def =
    catalog.data.components[name as keyof typeof catalog.data.components];
  if (!def) continue;
  components[name] = {
    props: def.props.toJSONSchema(),
    description: def.description ?? "",
    slots: def.slots ?? [],
  };
}

const actions: Record<string, CatalogSchemaAction> = {};
for (const name of Object.keys(catalog.data.actions)) {
  const def =
    catalog.data.actions[name as keyof typeof catalog.data.actions];
  if (!def) continue;
  actions[name] = {
    params: def.params.toJSONSchema(),
    description: def.description ?? "",
  };
}

const functions: Record<string, CatalogSchemaFunction> = {};
if (catalog.data.functions) {
  for (const name of Object.keys(catalog.data.functions)) {
    const def =
      catalog.data.functions[name as keyof typeof catalog.data.functions];
    if (!def) continue;
    functions[name] = {
      params: def.params.toJSONSchema(),
      returnType: def.returnType.toJSONSchema(),
      description: def.description ?? "",
    };
  }
}

const output: Record<string, unknown> = { components, actions };
if (Object.keys(functions).length > 0) {
  output.functions = functions;
}

writeFileSync(
  "catalog-schema.json",
  JSON.stringify(output, null, 2) + "\n",
);
console.log("Generated catalog-schema.json");
