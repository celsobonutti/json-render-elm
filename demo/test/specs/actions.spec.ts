import { test, expect } from "@playwright/test"
import { sendSpec } from "../helpers"

test.describe("Actions", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("button click emits press action", async ({ page }) => {
    await sendSpec(page, "actions/custom-action.json")
    const button = page.locator(".jr-button")
    await expect(button).toHaveText("Submit")
    await button.click()
    // Press action is handled in Elm (currently a no-op).
    // Verify the button is still rendered and clickable (no crash).
    await expect(button).toBeVisible()
  })
})
