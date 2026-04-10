export { schema, type ElmSchema, type ElmSpec } from './schema'
export { generateCatalogSchema } from './generateCatalogSchema'
export { generateElmReviewCatalog } from './generateElmReviewCatalog'
import type { Catalog } from '@json-render/core'
import './WatcherTrigger.js'
import './ValidationField.js'
import './ComponentMount.js'

export interface ElmPorts {
  jsonRenderSpecIn: {
    send: (value: unknown) => void
  }
  componentPortIn?: {
    send: (value: unknown) => void
  }
  componentPortOut?: {
    subscribe: (callback: (value: unknown) => void) => void
  }
}

export interface ElmApp {
  ports: ElmPorts
}

// Extract component names from a Catalog instance
type ComponentNames<C> = C extends Catalog<any, infer T>
  ? T extends { components: infer Comps }
    ? keyof Comps & string
    : never
  : never

export interface ElmBridge<Components extends string = string> {
  sendSpec: (spec: unknown) => void
  createStream: () => Promise<{ push: (chunk: string) => void }>
  registerPortHandler: (
    componentType: Components,
    portName: string,
    handler: (instanceId: string, send: (value: unknown) => void) => (() => void) | void
  ) => void
  onPortCommand: (
    componentType: Components,
    portName: string,
    handler: (instanceId: string, value: unknown) => void
  ) => void
}

type PortHandlerFactory = (
  instanceId: string,
  send: (value: unknown) => void
) => (() => void) | void

type PortCommandHandler = (instanceId: string, value: unknown) => void

export function createElmBridge<C extends Catalog>(app: ElmApp, catalog: C): ElmBridge<ComponentNames<C>>
export function createElmBridge(app: ElmApp): ElmBridge
export function createElmBridge(app: ElmApp, _catalog?: unknown): ElmBridge {
  // componentType -> portName -> handler factory
  const inboundHandlers = new Map<string, Map<string, PortHandlerFactory>>()
  // componentType -> portName -> handler
  const outboundHandlers = new Map<string, Map<string, PortCommandHandler>>()
  // instanceId -> list of cleanup functions
  const instanceCleanups = new Map<string, Array<() => void>>()

  function sendSpec(spec: unknown) {
    // Run all port cleanup before sending new spec
    for (const [, cleanups] of instanceCleanups.entries()) {
      for (const fn of cleanups) fn()
    }
    instanceCleanups.clear()

    app.ports.jsonRenderSpecIn.send(spec)
  }

  async function createStream() {
    const { createSpecStreamCompiler } = await import('@json-render/core')
    const compiler = createSpecStreamCompiler()
    return {
      push(chunk: string) {
        const { result } = compiler.push(chunk)
        sendSpec(result)
      },
    }
  }

  function registerPortHandler(
    componentType: string,
    portName: string,
    handler: PortHandlerFactory
  ) {
    if (!inboundHandlers.has(componentType)) {
      inboundHandlers.set(componentType, new Map())
    }
    inboundHandlers.get(componentType)!.set(portName, handler)
  }

  function onPortCommand(
    componentType: string,
    portName: string,
    handler: PortCommandHandler
  ) {
    if (!outboundHandlers.has(componentType)) {
      outboundHandlers.set(componentType, new Map())
    }
    outboundHandlers.get(componentType)!.set(portName, handler)
  }

  // Mount detection — set up inbound port handlers when components mount
  document.addEventListener('component-mounted', ((e: CustomEvent) => {
    const { instanceId, componentType } = e.detail ?? {}
    if (!instanceId || !componentType) return

    const typeHandlers = inboundHandlers.get(componentType)
    if (!typeHandlers) return

    const cleanups: Array<() => void> = []
    for (const [portName, factory] of typeHandlers.entries()) {
      const send = (value: unknown) => {
        app.ports.componentPortIn?.send({
          instanceId,
          port: portName,
          value
        })
      }
      const cleanup = factory(instanceId, send)
      if (cleanup) cleanups.push(cleanup)
    }
    if (cleanups.length > 0) {
      instanceCleanups.set(instanceId, cleanups)
    }
  }) as EventListener)

  // Unmount detection — global cleanup called from ComponentMount.disconnectedCallback
  ;(window as any).__jsonRenderPortCleanup = (instanceId: string, _componentType: string) => {
    const cleanups = instanceCleanups.get(instanceId)
    if (cleanups) {
      for (const fn of cleanups) fn()
      instanceCleanups.delete(instanceId)
    }
  }

  // Outbound port command dispatch
  app.ports.componentPortOut?.subscribe((msg: unknown) => {
    const { instanceId, port: portName, value } = msg as {
      instanceId: string
      port: string
      value: unknown
    }
    const el = document.querySelector(`[data-instance="${instanceId}"]`)
    const componentType = el?.getAttribute('data-component-type')
    if (!componentType) return

    const typeHandlers = outboundHandlers.get(componentType)
    if (!typeHandlers) return

    const handler = typeHandlers.get(portName)
    if (handler) handler(instanceId, value)
  })

  return { sendSpec, createStream, registerPortHandler, onPortCommand }
}
