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

// Set up the json-render-elm bridge (handles spec-in port)
const bridge = createElmBridge(app)

// Export action: download current state as JSON file
app.ports.downloadJson.subscribe((state: unknown) => {
  const json = JSON.stringify(state, null, 2)
  const blob = new Blob([json], { type: "application/json" })
  const url = URL.createObjectURL(blob)
  const a = document.createElement("a")
  a.href = url
  a.download = "state.json"
  a.click()
  URL.revokeObjectURL(url)
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
