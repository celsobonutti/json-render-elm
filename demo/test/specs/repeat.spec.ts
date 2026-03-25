import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Repeat", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("renders children once per array item", async ({ page }) => {
    await setState(page, {
      todos: [
        { id: "1", title: "Buy milk" },
        { id: "2", title: "Walk dog" },
        { id: "3", title: "Write code" },
      ],
    })
    await sendSpec(page, "repeat/todo-list.json")

    const items = page.locator(".jr-text")
    await expect(items).toHaveCount(3)
    await expect(items.nth(0)).toHaveText("Buy milk")
    await expect(items.nth(1)).toHaveText("Walk dog")
    await expect(items.nth(2)).toHaveText("Write code")
  })

  test("renders nothing for empty array", async ({ page }) => {
    await setState(page, { todos: [] })
    await sendSpec(page, "repeat/todo-list.json", { waitForVisible: false })

    await expect(page.locator(".jr-text")).toHaveCount(0)
  })

  test("renders multiple children per iteration", async ({ page }) => {
    await setState(page, {
      items: [
        { id: "1", name: "Alice", status: "active" },
        { id: "2", name: "Bob", status: "inactive" },
      ],
    })
    await sendSpec(page, "repeat/multi-child.json")

    const texts = page.locator(".jr-text")
    await expect(texts).toHaveCount(2)
    await expect(texts.nth(0)).toHaveText("Alice")
    await expect(texts.nth(1)).toHaveText("Bob")

    const badges = page.locator(".jr-badge")
    await expect(badges).toHaveCount(2)
    await expect(badges.nth(0)).toHaveText("active")
    await expect(badges.nth(1)).toHaveText("inactive")
  })

  test("updates DOM when state array changes", async ({ page }) => {
    await setState(page, {
      todos: [{ id: "1", title: "Buy milk" }],
    })
    await sendSpec(page, "repeat/todo-list.json")
    await expect(page.locator(".jr-text")).toHaveCount(1)

    await setState(page, {
      todos: [
        { id: "1", title: "Buy milk" },
        { id: "2", title: "Walk dog" },
      ],
    })
    await expect(page.locator(".jr-text")).toHaveCount(2)
  })
})
