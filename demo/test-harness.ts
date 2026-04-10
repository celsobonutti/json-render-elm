// @ts-ignore -- Elm module has no type declarations
import { Elm } from "./src/TestHarness.elm"
import { createElmBridge } from "../packages/js-bridge/src/index.ts"
import { catalog } from "./catalog"

const root = document.querySelector("#app")
const seedArray = new Uint32Array(1)
crypto.getRandomValues(seedArray)
const app = Elm.TestHarness!.init({ node: root, flags: seedArray[0] })

const bridge = createElmBridge(app, catalog)

;(window as any).__bridge = bridge
;(window as any).__setState = (state: object) => {
  app.ports.jsonRenderStateIn.send(state)
}

app.ports.testActionOut.subscribe((action: unknown) => {
  ;(window as any).__lastAction = action
})

app.ports.testDecodeErrorOut.subscribe((error: string) => {
  ;(window as any).__lastDecodeError = error
})

// --- Select component port handlers ---

bridge.registerPortHandler("Select", "clickOutside", (instanceId, send) => {
  const handler = (e: MouseEvent) => {
    const el = document.querySelector(`[data-instance="${instanceId}"]`)
    if (el && !el.contains(e.target as Node)) {
      send(null)
    }
  }
  document.addEventListener("click", handler)
  return () => document.removeEventListener("click", handler)
})
