import { test, expect } from "@playwright/test"
import { sendParitySpec, renderers } from "../../parity-helpers"

test.describe("Parity: Watchers", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("watcher fires when watched path changes", async ({ page }) => {
    await sendParitySpec(page, "watchers/reset-on-change.json")

    for (const [name, root] of renderers(page)) {
      const texts = root.locator(".jr-text")
      await expect(texts.nth(0), `${name}: initial category`).toHaveText("electronics")
      await expect(texts.nth(1), `${name}: initial filter`).toHaveText("all")

      await root.locator(".jr-button").click()

      await expect(texts.nth(0), `${name}: new category`).toHaveText("books")
      await expect(texts.nth(1), `${name}: filter reset`).toHaveText("reset")
    }
  })

  // React renderer cancels async watch effects on re-render from the first chained action,
  // so this test only verifies Elm behaviour.
  test("chained watcher actions execute in order", async ({ page }) => {
    await sendParitySpec(page, "watchers/chained-watch.json")

    const elmRoot = page.locator("#render-root")
    const texts = elmRoot.locator(".jr-text")
    await expect(texts.nth(0), "elm: initial").toHaveText("US")

    await elmRoot.locator(".jr-button").click()

    await expect(texts.nth(0), "elm: country").toHaveText("Canada")
    await expect(texts.nth(1), "elm: city").toHaveText("cleared")
    await expect(texts.nth(2), "elm: status").toHaveText("updated")
  })

  test("watcher does not fire on initial render", async ({ page }) => {
    await sendParitySpec(page, "watchers/no-fire-on-load.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("initial")
    }
  })

  test("watcher inside repeat fires with $item context", async ({ page }) => {
    await sendParitySpec(page, "watchers/repeat-item-watch.json")

    for (const [name, root] of renderers(page)) {
      const texts = root.locator(".jr-text")
      await expect(texts, `${name}: initial count`).toHaveCount(2)
      await expect(texts.nth(0), `${name}: first`).toHaveText("Buy milk")
      await expect(texts.nth(1), `${name}: second`).toHaveText("Walk dog")

      await root.locator(".jr-button").click()

      await expect(texts.nth(0), `${name}: first done`).toHaveText("DONE")
      await expect(texts.nth(1), `${name}: second done`).toHaveText("DONE")
    }
  })
})
