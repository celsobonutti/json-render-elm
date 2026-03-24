import { test, expect } from "@playwright/test"
import { sendSpec } from "../helpers"

test.describe("Rendering", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("renders a Card with title", async ({ page }) => {
    await sendSpec(page, "rendering/card-basic.json")
    await expect(page.locator(".jr-card")).toBeVisible()
    await expect(page.locator(".jr-card-title")).toHaveText("Hello World")
  })

  test("renders nested children", async ({ page }) => {
    await sendSpec(page, "rendering/nested-children.json")
    await expect(page.locator(".jr-card-title")).toHaveText("Parent")
    await expect(page.locator(".jr-text")).toHaveText("Child content")
    await expect(page.locator(".jr-card-body .jr-text")).toBeVisible()
  })
})
