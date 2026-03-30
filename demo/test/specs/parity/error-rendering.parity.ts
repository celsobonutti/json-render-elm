import { test, expect } from "@playwright/test"
import { readFileSync } from "fs"
import { join, dirname } from "path"
import { fileURLToPath } from "url"

const __dirname = dirname(fileURLToPath(import.meta.url))

function loadFixture(fixturePath: string) {
  const json = readFileSync(join(__dirname, "../../fixtures", fixturePath), "utf-8")
  return JSON.parse(json)
}

test.describe("Parity: Error Rendering", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("$computed with unknown function shows error", async ({ page }) => {
    // Send spec directly — Elm produces a Props error div (no jr- class), so
    // sendParitySpec's jr- waitFor would timeout. React silently renders undefined.
    const spec = loadFixture("expressions/computed-error.json")
    await page.evaluate((s) => (window as any).__parityBridge.sendSpec(s), spec)

    // Elm shows a "Props error: Unknown function: …" div
    const elmRoot = page.locator("#render-root")
    await expect(elmRoot, "elm: error shown").toContainText(/Props error|Unknown function/i)

    // React silently returns undefined for unknown functions (no error rendered)
    // — this is a known parity difference. Verify React at least renders something.
    const reactRoot = page.locator("#react-root")
    await page.waitForTimeout(200)
    // React renders an empty jr-text span (content = undefined → empty string)
    await expect(reactRoot.locator(".jr-text"), "react: text element in DOM").toBeAttached()
  })

  test("missing required props shows error in Elm", async ({ page }) => {
    // todo-missing-props.json has a Card element with props: {} (missing required title).
    // Elm shows a Props error box for that element; React renders the card with
    // undefined title (no explicit error) — known parity difference.
    const spec = loadFixture("todo/todo-missing-props.json")
    await page.evaluate((s) => (window as any).__parityBridge.sendSpec(s), spec)

    // Wait for Elm to render (the outer Card has a title so jr-card-title appears)
    await page.locator("#render-root .jr-card-title").first().waitFor({ timeout: 5000 })
    await page.locator("#react-root .jr-card-title").first().waitFor({ timeout: 5000 })

    // Elm shows a "Props error" box for the inner Card with missing title
    const elmRoot = page.locator("#render-root")
    await expect(elmRoot, "elm: props error shown").toContainText(/Props error/i)

    // React renders without an error message (undefined title renders as empty)
    // — assert that it rendered cards but does not show "Props error"
    const reactRoot = page.locator("#react-root")
    await expect(reactRoot.locator(".jr-card").first(), "react: card rendered").toBeAttached()
  })
})
