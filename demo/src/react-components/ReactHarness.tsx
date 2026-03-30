import { useState, useEffect } from "react"
import { Renderer, JSONUIProvider } from "@json-render/react"
import type { Spec } from "@json-render/react"
import { registry } from "./registry.tsx"

export function ReactHarness() {
  const [spec, setSpec] = useState<Spec | null>(null)
  const [state, setState] = useState<Record<string, unknown>>({})
  const [key, setKey] = useState(0)

  useEffect(() => {
    ;(window as any).__reactSetSpec = (newSpec: any) => {
      setSpec(newSpec)
      setState(newSpec.state ?? {})
      setKey((k) => k + 1)
    }
    ;(window as any).__reactSetState = (newState: Record<string, unknown>) => {
      setState(newState)
      setKey((k) => k + 1)
    }
  }, [])

  if (!spec) return null

  return (
    <JSONUIProvider
      key={key}
      registry={registry}
      initialState={state}
      handlers={{
        press: async () => {
          ;(window as any).__lastReactAction = { name: "press" }
        },
        export: async (params) => {
          ;(window as any).__lastReactAction = { name: "export", ...params }
        },
      }}
      functions={{ shout: (args: Record<string, unknown>) => (args.text as string).toUpperCase() }}
    >
      <Renderer spec={spec} registry={registry} />
    </JSONUIProvider>
  )
}
