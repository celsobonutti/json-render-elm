import { test, expect } from "@playwright/test"
import { sendParitySpec, renderers } from "../../parity-helpers"

test.describe("Parity: Rendering", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("Card with title", async ({ page }) => {
    await sendParitySpec(page, "rendering/card-basic.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-card"), `${name}: card visible`).toBeVisible()
      await expect(root.locator(".jr-card-title"), `${name}: title text`).toHaveText("Hello World")
    }
  })

  test("nested children", async ({ page }) => {
    await sendParitySpec(page, "rendering/nested-children.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-card-title"), `${name}: parent title`).toHaveText("Parent")
      await expect(root.locator(".jr-text"), `${name}: child text`).toHaveText("Child content")
      await expect(root.locator(".jr-card-body .jr-text"), `${name}: child nested`).toBeVisible()
    }
  })
})
