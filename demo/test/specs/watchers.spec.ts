import { test, expect } from "@playwright/test"
import { sendSpec } from "../helpers"

test.describe("Watchers", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("watcher fires action when watched path changes", async ({ page }) => {
    await sendSpec(page, "watchers/reset-on-change.json")

    // Initial state: category=electronics, filter=all
    const texts = page.locator(".jr-text")
    await expect(texts.nth(0)).toHaveText("electronics")
    await expect(texts.nth(1)).toHaveText("all")

    // Click button to change category to "books"
    await page.locator(".jr-button").click()

    // Watcher on /category fires → sets /filter to "reset"
    await expect(texts.nth(0)).toHaveText("books")
    await expect(texts.nth(1)).toHaveText("reset")
  })

  test("chained watcher actions execute in order", async ({ page }) => {
    await sendSpec(page, "watchers/chained-watch.json")

    // Initial state
    const texts = page.locator(".jr-text")
    await expect(texts.nth(0)).toHaveText("US")

    // Click to change country
    await page.locator(".jr-button").click()

    // Chained watchers: city=cleared, status=updated
    await expect(texts.nth(0)).toHaveText("Canada")
    await expect(texts.nth(1)).toHaveText("cleared")
    await expect(texts.nth(2)).toHaveText("updated")
  })

  test("watcher does not fire on initial render", async ({ page }) => {
    await sendSpec(page, "watchers/no-fire-on-load.json")

    // Output should remain "initial" — watcher should NOT have fired
    await expect(page.locator(".jr-text")).toHaveText("initial")
  })

  test("watcher inside repeat fires for each item with $item context", async ({
    page,
  }) => {
    await sendSpec(page, "watchers/repeat-item-watch.json")

    // Initial: shows titles
    const texts = page.locator(".jr-text")
    await expect(texts).toHaveCount(2)
    await expect(texts.nth(0)).toHaveText("Buy milk")
    await expect(texts.nth(1)).toHaveText("Walk dog")

    // Click "Mark All Done" — sets /markAll to true
    // Watcher on each repeated item fires, setting each item's done to true
    await page.locator(".jr-button").click()

    // Both items should now show "DONE"
    await expect(texts.nth(0)).toHaveText("DONE")
    await expect(texts.nth(1)).toHaveText("DONE")
  })
})
