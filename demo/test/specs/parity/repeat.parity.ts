import { test, expect } from "@playwright/test"
import { sendParitySpec, setParityState, renderers } from "../../parity-helpers"

test.describe("Parity: Repeat", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("renders children once per array item", async ({ page }) => {
    await sendParitySpec(page, "repeat/todo-list.json")

    for (const [name, root] of renderers(page)) {
      const items = root.locator(".jr-text")
      await expect(items, `${name}: count`).toHaveCount(3)
      await expect(items.nth(0), `${name}: first`).toHaveText("Buy milk")
      await expect(items.nth(1), `${name}: second`).toHaveText("Walk dog")
      await expect(items.nth(2), `${name}: third`).toHaveText("Write code")
    }
  })

  test("renders nothing for empty array", async ({ page }) => {
    await sendParitySpec(page, "repeat/todo-list.json")
    await setParityState(page, { todos: [] })

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveCount(0)
    }
  })

  test("renders multiple children per iteration", async ({ page }) => {
    await sendParitySpec(page, "repeat/multi-child.json")

    for (const [name, root] of renderers(page)) {
      const texts = root.locator(".jr-text")
      await expect(texts, `${name}: text count`).toHaveCount(2)
      await expect(texts.nth(0), `${name}: first text`).toHaveText("Alice")
      await expect(texts.nth(1), `${name}: second text`).toHaveText("Bob")

      const badges = root.locator(".jr-badge")
      await expect(badges, `${name}: badge count`).toHaveCount(2)
      await expect(badges.nth(0), `${name}: first badge`).toHaveText("active")
      await expect(badges.nth(1), `${name}: second badge`).toHaveText("inactive")
    }
  })

  test("updates DOM when state array changes", async ({ page }) => {
    await sendParitySpec(page, "repeat/todo-list.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}: initial`).toHaveCount(3)
    }

    await setParityState(page, {
      todos: [
        { id: "1", title: "Buy milk" },
        { id: "2", title: "Walk dog" },
      ],
    })

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}: after update`).toHaveCount(2)
    }
  })

  test("add todo via button click with chained actions", async ({ page }) => {
    await sendParitySpec(page, "repeat/todo-interactive.json")

    for (const [name, root] of renderers(page)) {
      // Reset state before each renderer so clicks don't interfere with each other
      await setParityState(page, { newTodo: "Buy milk", todos: [] })

      await expect(root.locator(".jr-input"), `${name}: input value`).toHaveValue("Buy milk")

      await root.locator(".jr-button").click()

      await expect(root.locator(".jr-text").first(), `${name}: todo appears`).toHaveText("Buy milk")
      await expect(root.locator(".jr-input"), `${name}: input cleared`).toHaveValue("")
    }
  })
})
