import { test, expect } from "@playwright/test"
import { sendSpec, getLastAction } from "../helpers"

test.describe("on field - event handling", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("button with on.press setState updates state and shows text", async ({
    page,
  }) => {
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

  test("pushState with $id generates unique IDs for each push", async ({
    page,
  }) => {
    await sendSpec(page, "on/push-state-id.json")

    const items = page.locator(".jr-stack-vertical .jr-stack-vertical .jr-text")
    await expect(items).toHaveCount(0)

    await page.locator(".jr-button").click()
    await expect(items).toHaveCount(1)

    await page.locator(".jr-button").click()
    await expect(items).toHaveCount(2)

    const text1 = await items.nth(0).textContent()
    const text2 = await items.nth(1).textContent()

    expect(text1).not.toBe("$id")
    expect(text2).not.toBe("$id")
    expect(text1).not.toBe(text2)

    expect(text1!.length).toBe(36)
    expect(text2!.length).toBe(36)
  })

  test("pushState with clearStatePath clears input after push", async ({
    page,
  }) => {
    await sendSpec(page, "on/push-state-clear.json")

    const input = page.locator(".jr-input")
    await expect(input).toHaveValue("Buy milk")

    await page.locator(".jr-button").click()

    await expect(input).toHaveValue("")

    const todoItems = page.locator(
      ".jr-stack-vertical .jr-stack-vertical .jr-text"
    )
    await expect(todoItems).toHaveCount(1)
    await expect(todoItems.nth(0)).toHaveText("Buy milk")

    await input.fill("Walk dog")
    await page.locator(".jr-button").click()

    await expect(input).toHaveValue("")
    await expect(todoItems).toHaveCount(2)
    await expect(todoItems.nth(1)).toHaveText("Walk dog")
  })

  test("expression params resolve current state at execution time", async ({
    page,
  }) => {
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

  test("action with preventDefault does not trigger page navigation", async ({
    page,
  }) => {
    await sendSpec(page, "on/prevent-default-form.json")

    // Status should be hidden initially
    await expect(page.locator(".jr-text")).toHaveCount(0)

    // Record the URL before clicking
    const urlBefore = page.url()

    // Click the button with preventDefault
    await page.locator(".jr-button").click()

    // State should update — status text appears
    await expect(page.locator(".jr-text")).toHaveText("Submitted!")

    // URL should not have changed (no navigation)
    expect(page.url()).toBe(urlBefore)
  })
})
