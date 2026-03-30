// Parity harness — mounts both Elm and React renderers side-by-side
// for cross-renderer parity testing.

// --- Elm side (reuse existing TestHarness.elm + bridge) ---
// @ts-ignore -- Elm module has no type declarations
import { Elm } from "./src/TestHarness.elm"
import { createElmBridge } from "../packages/js-bridge/src/index.ts"

// --- React side ---
import { createElement } from "react"
import { createRoot } from "react-dom/client"
import { ReactHarness } from "./src/react-components/ReactHarness.tsx"

// Mount Elm
const elmRoot = document.querySelector("#elm-root")!
const seedArray = new Uint32Array(1)
crypto.getRandomValues(seedArray)
const app = Elm.TestHarness!.init({ node: elmRoot, flags: seedArray[0] })
const bridge = createElmBridge(app)

// Mount React
const reactRoot = createRoot(document.querySelector("#react-root")!)
reactRoot.render(createElement(ReactHarness))

// Capture Elm actions
app.ports.testActionOut.subscribe((action: unknown) => {
  ;(window as any).__lastElmAction = action
})

app.ports.testDecodeErrorOut.subscribe((error: string) => {
  ;(window as any).__lastElmDecodeError = error
})

// Expose unified parity bridge
;(window as any).__parityBridge = {
  sendSpec(spec: unknown) {
    // Send to Elm
    bridge.sendSpec(spec)
    // Send to React
    ;(window as any).__reactSetSpec(spec)
  },
  setState(state: unknown) {
    // Send to Elm
    app.ports.jsonRenderStateIn.send(state)
    // Send to React
    ;(window as any).__reactSetState(state)
  },
  getLastAction(renderer: "elm" | "react") {
    return renderer === "elm"
      ? (window as any).__lastElmAction
      : (window as any).__lastReactAction
  },
}
