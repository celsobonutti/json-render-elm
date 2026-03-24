// @ts-ignore -- Elm module has no type declarations
import { Elm } from "./src/TestHarness.elm"
import { createElmBridge } from "../packages/js-bridge/src/index.ts"

const root = document.querySelector("#app")
const app = Elm.TestHarness!.init({ node: root })

const bridge = createElmBridge(app, {
  onAction: (name: string, params: unknown) => {
    ;(window as any).__lastAction = { name, params }
  },
})

;(window as any).__bridge = bridge
;(window as any).__setState = (state: object) => {
  app.ports.jsonRenderStateIn.send(state)
}
