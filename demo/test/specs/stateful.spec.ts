import { test, expect } from "@playwright/test"
import { sendSpec } from "../helpers"

test.describe("Stateful components", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("renders stateful toggle with initial state", async ({ page }) => {
    await sendSpec(page, "stateful/toggle.json")
    await expect(page.locator(".jr-toggle-btn")).toHaveText("Power: OFF")
  })

  test("toggles state on click", async ({ page }) => {
    await sendSpec(page, "stateful/toggle.json")
    const btn = page.locator(".jr-toggle-btn")
    await expect(btn).toHaveText("Power: OFF")

    await btn.click()
    await expect(btn).toHaveText("Power: ON")

    await btn.click()
    await expect(btn).toHaveText("Power: OFF")
  })
})
