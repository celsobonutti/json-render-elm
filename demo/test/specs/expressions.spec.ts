import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Expressions", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("$state reads value from state", async ({ page }) => {
    await setState(page, { greeting: "Hello Alice" })
    await sendSpec(page, "expressions/state-read.json")
    await expect(page.locator(".jr-text")).toHaveText("Hello Alice")
  })

  test("$template interpolates state values", async ({ page }) => {
    await setState(page, { user: { name: "Bob" } })
    await sendSpec(page, "expressions/template-interpolation.json")
    await expect(page.locator(".jr-text")).toHaveText("Welcome, Bob!")
  })

  test("$bindState reads initial value into input", async ({ page }) => {
    await setState(page, { form: { name: "Alice" } })
    await sendSpec(page, "expressions/bind-state.json")
    await expect(page.locator(".jr-input")).toHaveValue("Alice")
  })

  test("$bindState updates state on input change", async ({ page }) => {
    await setState(page, { form: { name: "Alice" } })
    await sendSpec(page, "expressions/bind-state.json")

    const input = page.locator(".jr-input")
    await input.clear()
    await input.fill("Bob")
    await expect(input).toHaveValue("Bob")
  })

  test("$bindItem reads initial value from repeat item", async ({ page }) => {
    await setState(page, {
      items: [
        { id: "1", label: "First", text: "hello" },
        { id: "2", label: "Second", text: "world" },
      ],
    })
    await sendSpec(page, "expressions/bind-item.json")

    const inputs = page.locator(".jr-input")
    await expect(inputs).toHaveCount(2)
    await expect(inputs.nth(0)).toHaveValue("hello")
    await expect(inputs.nth(1)).toHaveValue("world")
  })

  test("$computed resolves with registered function", async ({ page }) => {
    await sendSpec(page, "expressions/computed-basic.json")
    await expect(page.locator(".jr-text")).toHaveText("HELLO WORLD")
  })

  test("$computed with unknown function shows error", async ({ page }) => {
    await sendSpec(page, "expressions/computed-error.json")
    await expect(page.locator("body")).toContainText("Props error")
  })

  test("$computed updates when bound state changes via input", async ({ page }) => {
    await setState(page, { name: "alice" })
    await sendSpec(page, "expressions/computed-with-bind.json")

    // Initial: input shows "alice", computed shows "ALICE"
    await expect(page.locator(".jr-input")).toHaveValue("alice")
    await expect(page.locator(".jr-text")).toHaveText("ALICE")

    // Type new value: computed display updates reactively
    const input = page.locator(".jr-input")
    await input.clear()
    await input.fill("bob")
    await expect(page.locator(".jr-text")).toHaveText("BOB")
  })

  test("$computed shows empty result when bound input is cleared", async ({ page }) => {
    await setState(page, { name: "hello" })
    await sendSpec(page, "expressions/computed-with-bind.json")

    await expect(page.locator(".jr-text")).toHaveText("HELLO")

    // Clear input: computed result becomes empty string (shouted "")
    const input = page.locator(".jr-input")
    await input.clear()
    await expect(page.locator(".jr-text")).toHaveText("")
  })

  test("$bindItem updates item state on input change", async ({ page }) => {
    await setState(page, {
      items: [
        { id: "1", label: "First", text: "hello" },
        { id: "2", label: "Second", text: "world" },
      ],
    })
    await sendSpec(page, "expressions/bind-item.json")

    const secondInput = page.locator(".jr-input").nth(1)
    await secondInput.clear()
    await secondInput.fill("changed")
    await expect(secondInput).toHaveValue("changed")

    // First input is unaffected
    await expect(page.locator(".jr-input").nth(0)).toHaveValue("hello")
  })
})
