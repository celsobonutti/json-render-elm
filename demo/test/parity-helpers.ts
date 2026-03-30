import { Page, Locator } from "@playwright/test"
import { readFileSync } from "fs"
import { join, dirname } from "path"
import { fileURLToPath } from "url"

const __dirname = dirname(fileURLToPath(import.meta.url))

/**
 * Returns named [rendererName, rootLocator] pairs for iterating over both renderers.
 *
 * Note: Elm replaces its mount node (#elm-root) with its own DOM, so we use
 * #render-root (the Elm renderer's content root) as the Elm locator. React
 * renders inside #react-root, which stays in the DOM.
 */
export function renderers(page: Page): [string, Locator][] {
  return [
    ["elm", page.locator("#render-root")],
    ["react", page.locator("#react-root")],
  ]
}

/**
 * Send a JSON fixture spec to both renderers and wait for both to render.
 */
export async function sendParitySpec(page: Page, fixturePath: string) {
  const json = readFileSync(join(__dirname, "fixtures", fixturePath), "utf-8")
  const spec = JSON.parse(json)
  await page.evaluate((s) => (window as any).__parityBridge.sendSpec(s), spec)

  // Wait for both renderers to produce visible content.
  // Elm replaces #elm-root with its own DOM; rendered content lives under #render-root.
  await page
    .locator("#render-root [class^='jr-']")
    .first()
    .waitFor({ timeout: 5000 })
  await page
    .locator("#react-root [class^='jr-']")
    .first()
    .waitFor({ timeout: 5000 })
}

/**
 * Update state on both renderers.
 */
export async function setParityState(page: Page, state: object) {
  await page.evaluate((s) => (window as any).__parityBridge.setState(s), state)
  // Wait for re-render
  await page.waitForTimeout(100)
}

/**
 * Get the last captured action from a specific renderer.
 * Only captures custom actions (press, export). Built-in actions (setState,
 * pushState, removeState) are verified through DOM assertions.
 */
export async function getParityLastAction(
  page: Page,
  renderer: "elm" | "react"
) {
  return page.evaluate(
    (r) => (window as any).__parityBridge.getLastAction(r),
    renderer
  )
}
