import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Visibility", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("truthy condition shows element when true", async ({ page }) => {
    await setState(page, { show: true })
    await sendSpec(page, "visibility/truthy-condition.json")
    await expect(page.locator(".jr-text")).toHaveText("Secret message")
  })

  test("truthy condition hides element when false", async ({ page }) => {
    await setState(page, { show: false })
    await sendSpec(page, "visibility/truthy-condition.json", { waitForVisible: false })
    await expect(page.locator(".jr-text")).toHaveCount(0)
  })
})
