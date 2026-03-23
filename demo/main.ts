import "./style.css"
// @ts-ignore -- Elm module has no type declarations
import { Elm } from "./src/Main.elm"
import { createElmBridge } from "../packages/js-bridge/src/index.ts"

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer")
  ElmDebugTransform.register({ simple_mode: true })
}

const root = document.querySelector("#app div")
const app = Elm.Main!.init({ node: root })

// Set up the json-render-elm bridge (handles spec-in and action-out ports)
const bridge = createElmBridge(app, {
  onAction: (name: string, params: unknown) => {
    console.log("Custom action:", name, params)
  },
})

// App-specific: prompt → API → bridge.sendSpec
app.ports.sendPrompt.subscribe(async (prompt: string) => {
  try {
    const res = await fetch("/api/generate", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ prompt }),
    })
    const data = await res.json()
    if (data.error) {
      app.ports.receiveError.send(data.error)
    } else {
      bridge.sendSpec(data.spec)
    }
  } catch (e) {
    const message = e instanceof Error ? e.message : String(e)
    app.ports.receiveError.send(message)
  }
})
