import { test, expect } from "@playwright/test"
import { sendSpec, setState, getLastAction } from "../helpers"

test.describe("on field - event handling", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("button with on.press setState updates state and shows text", async ({
    page,
  }) => {
    await setState(page, { clicked: false })
    await sendSpec(page, "on/button-set-state.json")

    // Status text should be hidden initially
    const statusTexts = page.locator(".jr-text")
    await expect(statusTexts).toHaveCount(0)

    // Click the button
    await page.locator(".jr-button").click()

    // After click, setState sets /clicked to true, making status visible
    await expect(page.locator(".jr-text")).toHaveText("Clicked!")
  })

  test("button with on.press pushState adds item to list", async ({
    page,
  }) => {
    await setState(page, {
      items: [{ id: "1", title: "Existing Item" }],
    })
    await sendSpec(page, "on/button-push-state.json")

    // One item initially
    const items = page.locator(".jr-text")
    await expect(items).toHaveCount(1)
    await expect(items.nth(0)).toHaveText("Existing Item")

    // Click the Add Item button
    await page.locator(".jr-button").click()

    // After click, pushState adds a new item
    await expect(page.locator(".jr-text")).toHaveCount(2)
    await expect(page.locator(".jr-text").nth(1)).toHaveText("New Item")
  })

  test("chained actions execute in order", async ({ page }) => {
    await setState(page, {
      newTodo: "Buy milk",
      todos: [],
      loading: false,
    })
    await sendSpec(page, "on/chained-actions.json")

    // No todo items initially
    // The input should show the current value
    const input = page.locator(".jr-input")
    await expect(input).toHaveValue("Buy milk")

    // Click Add Todo button
    await page.locator(".jr-button").click()

    // After chained actions:
    // 1. loading set to true (then set back to false)
    // 2. "Buy milk" pushed to /todos
    // 3. /newTodo cleared to ""
    // 4. loading set to false

    // Input should be cleared
    await expect(input).toHaveValue("")

    // Todo should appear in the list
    const todoItems = page.locator(
      ".jr-stack-vertical .jr-stack-vertical .jr-text"
    )
    await expect(todoItems).toHaveCount(1)

    // Loading text should NOT be visible (loading was set back to false)
    const allTexts = page.locator(".jr-text")
    const loadingVisible = await allTexts
      .filter({ hasText: "Loading..." })
      .count()
    expect(loadingVisible).toBe(0)
  })

  test("custom action fires via testActionOut port", async ({ page }) => {
    await sendSpec(page, "on/custom-action.json")
    await page.locator(".jr-button").click()

    // Wait a tick for the port subscription to fire
    await page.waitForTimeout(100)

    const action = await getLastAction(page)
    expect(action).toEqual({ name: "press" })
  })

  test("expression params resolve current state at execution time", async ({
    page,
  }) => {
    await setState(page, {
      inputValue: "",
      collected: [],
    })
    await sendSpec(page, "on/expression-params.json")

    // Type a value into the input
    const input = page.locator(".jr-input")
    await input.fill("hello")

    // Click button to push the current inputValue to /collected
    await page.locator(".jr-button").click()

    // The pushed item should be the resolved $state value "hello"
    const items = page.locator(
      ".jr-stack-vertical .jr-stack-vertical .jr-text"
    )
    await expect(items).toHaveCount(1)

    // Type another value and push again
    await input.fill("world")
    await page.locator(".jr-button").click()

    await expect(items).toHaveCount(2)
  })
})
