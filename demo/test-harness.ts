import "./src/web-components/JrDropdown.ts"
// @ts-ignore -- Elm module has no type declarations
import { Elm } from "./src/TestHarness.elm"
import { createElmBridge } from "../packages/js-bridge/src/index.ts"

const root = document.querySelector("#app")
const seedArray = new Uint32Array(1)
crypto.getRandomValues(seedArray)
const app = Elm.TestHarness!.init({ node: root, flags: seedArray[0] })

const bridge = createElmBridge(app)

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
