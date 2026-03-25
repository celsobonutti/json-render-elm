import { Page } from "@playwright/test"
import { readFileSync } from "fs"
import { join, dirname } from "path"
import { fileURLToPath } from "url"

const __dirname = dirname(fileURLToPath(import.meta.url))

export async function sendSpec(
  page: Page,
  fixturePath: string,
  options: { waitForVisible?: boolean } = {}
) {
  const { waitForVisible = true } = options
  const json = readFileSync(join(__dirname, "fixtures", fixturePath), "utf-8")
  const spec = JSON.parse(json)
  await page.evaluate((s) => (window as any).__bridge.sendSpec(s), spec)
  if (waitForVisible) {
    await page.locator("#render-root > :first-child:not(#no-spec)").waitFor()
  } else {
    // When all elements are hidden, Elm replaces #no-spec with an empty text
    // node. Wait for #no-spec to detach as the signal that the spec rendered.
    await page.locator("#no-spec").waitFor({ state: "detached" })
  }
}

export async function setState(page: Page, state: object) {
  await page.evaluate((s) => (window as any).__setState(s), state)
}
