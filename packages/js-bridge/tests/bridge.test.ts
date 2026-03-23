import { describe, it, expect, vi } from 'vitest'
import { createElmBridge } from '../src/index'

function createMockElmApp() {
  const specCallbacks: Array<(value: unknown) => void> = []
  const actionSubscribers: Array<(value: unknown) => void> = []

  return {
    ports: {
      jsonRenderSpecIn: {
        send: vi.fn((value: unknown) => {
          specCallbacks.forEach(cb => cb(value))
        }),
      },
      jsonRenderActionOut: {
        subscribe: vi.fn((cb: (value: unknown) => void) => {
          actionSubscribers.push(cb)
        }),
      },
      _simulateAction(value: unknown) {
        actionSubscribers.forEach(cb => cb(value))
      },
    },
  }
}

describe('createElmBridge', () => {
  it('should return sendSpec and createStream', () => {
    const app = createMockElmApp()
    const bridge = createElmBridge(app)

    expect(bridge.sendSpec).toBeTypeOf('function')
    expect(bridge.createStream).toBeTypeOf('function')
  })

  it('should send spec to Elm port', () => {
    const app = createMockElmApp()
    const bridge = createElmBridge(app)

    const spec = {
      root: 'card-1',
      elements: {
        'card-1': { type: 'Card', props: { title: 'Hello' }, children: [] },
      },
    }

    bridge.sendSpec(spec)

    expect(app.ports.jsonRenderSpecIn.send).toHaveBeenCalledWith(spec)
  })

  it('should forward custom actions to onAction callback', () => {
    const app = createMockElmApp()
    const onAction = vi.fn()
    createElmBridge(app, { onAction })

    app.ports._simulateAction({ name: 'export_report', params: { format: 'pdf' } })

    expect(onAction).toHaveBeenCalledWith('export_report', { format: 'pdf' })
  })

  it('should not throw when onAction is not provided', () => {
    const app = createMockElmApp()
    createElmBridge(app)

    expect(() => {
      app.ports._simulateAction({ name: 'some_action', params: null })
    }).not.toThrow()
  })
})
