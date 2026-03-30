import { test, expect } from "@playwright/test"
import {
  sendParitySpec,
  renderers,
  getParityLastAction,
} from "../../parity-helpers"

test.describe("Parity: Events and Actions", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("button setState shows hidden element", async ({ page }) => {
    await sendParitySpec(page, "on/button-set-state.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}: initially hidden`).toHaveCount(0)

      await root.locator(".jr-button").click()

      await expect(root.locator(".jr-text"), `${name}: visible after click`).toHaveText("Clicked!")
    }
  })

  test("button pushState adds item to list", async ({ page }) => {
    await sendParitySpec(page, "on/button-push-state.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}: initial count`).toHaveCount(1)

      await root.locator(".jr-button").click()

      await expect(root.locator(".jr-text"), `${name}: after push`).toHaveCount(2)
      await expect(root.locator(".jr-text").nth(1), `${name}: new item`).toHaveText("New Item")
    }
  })

  test("chained actions execute in order", async ({ page }) => {
    await sendParitySpec(page, "on/chained-actions.json")

    for (const [name, root] of renderers(page)) {
      const input = root.locator(".jr-input")
      await expect(input, `${name}: initial value`).toHaveValue("Buy milk")

      await root.locator(".jr-button").click()

      // Input cleared by chained setState
      await expect(input, `${name}: input cleared`).toHaveValue("")

      // Todo pushed to list
      const todoItems = root.locator(".jr-stack-vertical .jr-stack-vertical .jr-text")
      await expect(todoItems, `${name}: todo count`).toHaveCount(1)
    }
  })

  test("custom action fires on both renderers", async ({ page }) => {
    await sendParitySpec(page, "on/custom-action.json")

    for (const [name, root] of renderers(page)) {
      await root.locator(".jr-button").click()
      await page.waitForTimeout(100)

      const action = await getParityLastAction(page, name as "elm" | "react")
      expect(action, `${name}: action dispatched`).toEqual({ name: "press" })
    }
  })

  test("pushState with $id generates UUIDs", async ({ page }) => {
    await sendParitySpec(page, "on/push-state-id.json")

    for (const [name, root] of renderers(page)) {
      const items = root.locator(".jr-stack-vertical .jr-stack-vertical .jr-text")

      await root.locator(".jr-button").click()
      await expect(items, `${name}: first push`).toHaveCount(1)

      await root.locator(".jr-button").click()
      await expect(items, `${name}: second push`).toHaveCount(2)

      const text1 = await items.nth(0).textContent()
      const text2 = await items.nth(1).textContent()

      // Both should be generated IDs (not the literal "$id"), and unique
      expect(text1, `${name}: id1 not literal`).not.toBe("$id")
      expect(text2, `${name}: id2 not literal`).not.toBe("$id")
      expect(text1, `${name}: unique ids`).not.toBe(text2)
    }
  })

  test("pushState with clearStatePath clears input", async ({ page }) => {
    await sendParitySpec(page, "on/push-state-clear.json")

    for (const [name, root] of renderers(page)) {
      const input = root.locator(".jr-input")
      await expect(input, `${name}: initial`).toHaveValue("Buy milk")

      await root.locator(".jr-button").click()

      await expect(input, `${name}: cleared`).toHaveValue("")
      const todoItems = root.locator(".jr-stack-vertical .jr-stack-vertical .jr-text")
      await expect(todoItems, `${name}: pushed`).toHaveCount(1)
      await expect(todoItems.nth(0), `${name}: content`).toHaveText("Buy milk")
    }
  })

  test("expression params resolve current state", async ({ page }) => {
    await sendParitySpec(page, "on/expression-params.json")

    for (const [name, root] of renderers(page)) {
      const input = root.locator(".jr-input")
      await input.fill("hello")

      await root.locator(".jr-button").click()

      const items = root.locator(".jr-stack-vertical .jr-stack-vertical .jr-text")
      await expect(items, `${name}: pushed`).toHaveCount(1)
    }
  })
})
