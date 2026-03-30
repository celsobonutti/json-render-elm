import { defineRegistry } from "@json-render/react"
import type { Catalog } from "@json-render/core"
import { catalog } from "../../catalog.ts"
import { Card } from "./Card.tsx"
import { Text } from "./Text.tsx"
import { Button } from "./Button.tsx"
import { Input } from "./Input.tsx"
import { Stack } from "./Stack.tsx"
import { Badge } from "./Badge.tsx"
import { Image } from "./Image.tsx"

// The catalog is defined with the Elm schema (which has extra fields like `on`,
// `watch`, `functions`), but defineRegistry only uses the catalog for type
// inference at compile time — at runtime the _catalog parameter is unused.
// Cast to `Catalog` to satisfy the type checker.
export const { registry } = defineRegistry(catalog as unknown as Catalog, {
  components: {
    Card,
    Text,
    Button,
    Input,
    Stack,
    Badge,
    Image,
  },
})
