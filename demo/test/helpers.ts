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

export async function getLastAction(page: Page) {
  return page.evaluate(() => (window as any).__lastAction)
}

/**
 * Call the /api/generate endpoint with a prompt, then send the resulting spec
 * to the test harness for rendering.
 */
export async function generateAndRender(page: Page, prompt: string) {
  const res = await page.evaluate(async (p: string) => {
    const resp = await fetch("/api/generate", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ prompt: p }),
    })
    return resp.json()
  }, prompt)

  if (res.error) {
    throw new Error(`Generate failed: ${res.error}`)
  }

  // Clear any previous decode error
  await page.evaluate(() => {
    ;(window as any).__lastDecodeError = null
  })
  await page.evaluate((s) => (window as any).__bridge.sendSpec(s), res.spec)
  await page
    .locator("#render-root > :first-child:not(#no-spec)")
    .waitFor({ timeout: 5000 })
    .catch(async () => {
      const decodeError = await page.evaluate(
        () => (window as any).__lastDecodeError
      )
      if (decodeError) {
        throw new Error(`Spec decode error: ${decodeError}`)
      }
      throw new Error("Spec failed to render (no visible output)")
    })
  return res.spec
}
