import { describe, it, expect, vi } from 'vitest'
import { createElmBridge } from '../src/index'

function createMockElmApp() {
  return {
    ports: {
      jsonRenderSpecIn: {
        send: vi.fn(),
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
})
