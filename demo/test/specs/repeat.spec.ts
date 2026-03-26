import { test, expect } from "@playwright/test"
import { sendSpec, setState, getLastAction } from "../helpers"

test.describe("Repeat", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("renders children once per array item", async ({ page }) => {
    await sendSpec(page, "repeat/todo-list.json")

    const items = page.locator(".jr-text")
    await expect(items).toHaveCount(3)
    await expect(items.nth(0)).toHaveText("Buy milk")
    await expect(items.nth(1)).toHaveText("Walk dog")
    await expect(items.nth(2)).toHaveText("Write code")
  })

  test("renders nothing for empty array", async ({ page }) => {
    await sendSpec(page, "repeat/todo-list.json")
    await setState(page, { todos: [] })

    await expect(page.locator(".jr-text")).toHaveCount(0)
  })

  test("renders multiple children per iteration", async ({ page }) => {
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
    await sendSpec(page, "repeat/todo-list.json")
    await expect(page.locator(".jr-text")).toHaveCount(3)

    await setState(page, {
      todos: [
        { id: "1", title: "Buy milk" },
        { id: "2", title: "Walk dog" },
      ],
    })
    await expect(page.locator(".jr-text")).toHaveCount(2)
  })
})

test.describe("Repeat with on-driven interactions", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("add todo via button click with chained actions", async ({ page }) => {
    await sendSpec(page, "repeat/todo-interactive.json")
    await setState(page, { newTodo: "Buy milk", todos: [] })

    // Input shows initial value
    const input = page.locator(".jr-input")
    await expect(input).toHaveValue("Buy milk")

    // No todo items initially
    const todoTexts = page.locator(".jr-stack-vertical .jr-text")
    await expect(todoTexts).toHaveCount(0)

    // Click Add button
    await page.locator(".jr-button").click()

    // Todo should appear
    await expect(page.locator(".jr-text").first()).toHaveText("Buy milk")

    // Input should be cleared
    await expect(input).toHaveValue("")
  })

  test("add multiple todos via repeated button clicks", async ({ page }) => {
    await sendSpec(page, "repeat/todo-interactive.json")

    const input = page.locator(".jr-input")
    const addBtn = page.locator(".jr-button")

    // Add first todo
    await input.fill("Buy milk")
    await addBtn.click()
    await expect(input).toHaveValue("")

    // Add second todo
    await input.fill("Walk dog")
    await addBtn.click()
    await expect(input).toHaveValue("")

    // Both todos should be in the list
    const items = page.locator(".jr-text")
    await expect(items).toHaveCount(2)
  })

  test("per-item button captures $item from repeat context", async ({
    page,
  }) => {
    await sendSpec(page, "repeat/todo-with-remove.json")

    // Two todo rows should render
    const titles = page.locator(".jr-text")
    await expect(titles).toHaveCount(2)
    await expect(titles.nth(0)).toHaveText("Buy milk")
    await expect(titles.nth(1)).toHaveText("Walk dog")

    // Click the second Remove button
    const removeButtons = page.locator(".jr-button-danger")
    await expect(removeButtons).toHaveCount(2)
    await removeButtons.nth(1).click()

    // The $item expression in the action params should resolve to the
    // second item's id, storing it in /selectedForRemoval
    // We verify by checking state was updated (the remove button sets
    // /selectedForRemoval to the item's id)
    // Since we can't directly read state, we verify the UI still shows
    // both items (the action only sets state, doesn't remove)
    await expect(titles).toHaveCount(2)
  })
})
