import "./style.css"
import "./src/component-registry.ts"
// @ts-ignore -- Elm module has no type declarations
import { Elm } from "./src/Main.elm"
import { createElmBridge } from "../packages/js-bridge/src/index.ts"

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer")
  ElmDebugTransform.register({ simple_mode: true })
}

const root = document.querySelector("#app div")
const seedArray = new Uint32Array(1)
crypto.getRandomValues(seedArray)
const app = Elm.Main!.init({ node: root, flags: seedArray[0] })

// Set up the json-render-elm bridge (handles spec-in port)
const bridge = createElmBridge(app)

// --- Fixture catalog sidebar ---

const fixtures = import.meta.glob<{ default: unknown }>(
  "./test/fixtures/**/*.json",
  { eager: false }
)

type FixtureMap = Record<string, string[]>

function buildCatalog(): FixtureMap {
  const catalog: FixtureMap = {}
  const prefix = "./test/fixtures/"
  for (const path of Object.keys(fixtures).sort()) {
    const relative = path.slice(prefix.length) // "repeat/todo-list.json"
    const slash = relative.indexOf("/")
    if (slash === -1) continue
    const category = relative.slice(0, slash)
    const name = relative.slice(slash + 1).replace(/\.json$/, "")
    if (!catalog[category]) catalog[category] = []
    catalog[category].push(name)
  }
  return catalog
}

function renderCatalog() {
  const catalog = buildCatalog()
  const listEl = document.querySelector(".catalog-list")
  if (!listEl) return

  for (const [category, items] of Object.entries(catalog)) {
    const section = document.createElement("div")
    section.className = "catalog-category"

    const header = document.createElement("button")
    header.className = "catalog-category-header"
    header.innerHTML = `<span class="arrow">\u25BE</span> ${category}`
    header.addEventListener("click", () => {
      section.classList.toggle("collapsed")
    })
    section.appendChild(header)

    const itemsContainer = document.createElement("div")
    itemsContainer.className = "catalog-items"
    for (const name of items) {
      const btn = document.createElement("button")
      btn.className = "catalog-item"
      btn.textContent = name
      btn.dataset.fixture = `${category}/${name}`
      btn.addEventListener("click", () => loadFixture(`${category}/${name}`))
      itemsContainer.appendChild(btn)
    }
    section.appendChild(itemsContainer)

    listEl.appendChild(section)
  }
}

let activeFixture: string | null = null

async function loadFixture(name: string) {
  const path = `./test/fixtures/${name}.json`
  const loader = fixtures[path]
  if (!loader) return
  const mod = await loader()
  bridge.sendSpec(mod.default)

  // Highlight active fixture
  activeFixture = name
  document.querySelectorAll(".catalog-item").forEach((el) => {
    el.classList.toggle("active", (el as HTMLElement).dataset.fixture === name)
  })
}

// Toggle sidebar
const layout = document.getElementById("layout")!
const toggleBtn = document.getElementById("catalog-toggle")!
const closeBtn = document.querySelector(".catalog-close")!

toggleBtn.addEventListener("click", () => layout.classList.add("catalog-open"))
closeBtn.addEventListener("click", () => layout.classList.remove("catalog-open"))

renderCatalog()

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
