export { schema, type ElmSchema, type ElmSpec } from './schema'
import '../../elm-core/src/JsonRender/WatcherTrigger.js'

export interface ElmPorts {
  jsonRenderSpecIn: {
    send: (value: unknown) => void
  }
}

export interface ElmApp {
  ports: ElmPorts
}

export interface ElmBridge {
  sendSpec: (spec: unknown) => void
  createStream: () => Promise<{ push: (chunk: string) => void }>
}

export function createElmBridge(app: ElmApp): ElmBridge {
  function sendSpec(spec: unknown) {
    app.ports.jsonRenderSpecIn.send(spec)
  }

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
