export { schema, type ElmSchema, type ElmSpec } from './schema'

export interface ElmPorts {
  jsonRenderSpecIn: {
    send: (value: unknown) => void
  }
  jsonRenderActionOut: {
    subscribe: (callback: (value: unknown) => void) => void
  }
}

export interface ElmApp {
  ports: ElmPorts
}

export interface BridgeOptions {
  onAction?: (name: string, params: unknown) => void
}

export interface ElmBridge {
  sendSpec: (spec: unknown) => void
  createStream: () => Promise<{ push: (chunk: string) => void }>
}

export function createElmBridge(app: ElmApp, options: BridgeOptions = {}): ElmBridge {
  function sendSpec(spec: unknown) {
    app.ports.jsonRenderSpecIn.send(spec)
  }

  app.ports.jsonRenderActionOut.subscribe((action: unknown) => {
    const { name, params } = action as { name: string; params: unknown }
    if (options.onAction) {
      options.onAction(name, params)
    }
  })

  async function createStream() {
    // Dynamic import to keep @json-render/core as optional peer dep (ESM-compatible)
    const { createSpecStreamCompiler } = await import('@json-render/core')
    const compiler = createSpecStreamCompiler()
    return {
      push(chunk: string) {
        const { result } = compiler.push(chunk)
        sendSpec(result)
      },
    }
  }

  return { sendSpec, createStream }
}
