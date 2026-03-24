import { Page } from "@playwright/test"
import { readFileSync } from "fs"
import { join, dirname } from "path"
import { fileURLToPath } from "url"

const __dirname = dirname(fileURLToPath(import.meta.url))

export async function sendSpec(page: Page, fixturePath: string) {
  const json = readFileSync(join(__dirname, "fixtures", fixturePath), "utf-8")
  const spec = JSON.parse(json)
  await page.evaluate((s) => (window as any).__bridge.sendSpec(s), spec)
  await page.locator("#render-root :first-child:not(#no-spec)").waitFor()
}

export async function setState(page: Page, state: object) {
  await page.evaluate((s) => (window as any).__setState(s), state)
}

export async function getLastAction(page: Page) {
  return page.evaluate(() => (window as any).__lastAction)
}
