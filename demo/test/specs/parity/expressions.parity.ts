import { test, expect } from "@playwright/test"
import { sendParitySpec, setParityState, renderers } from "../../parity-helpers"

test.describe("Parity: Expressions", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("$state reads value from state", async ({ page }) => {
    await sendParitySpec(page, "expressions/state-read.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("Hello Alice")
    }
  })

  test("$template interpolates state values", async ({ page }) => {
    await sendParitySpec(page, "expressions/template-interpolation.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("Welcome, Bob!")
    }
  })

  test("$bindState reads initial value into input", async ({ page }) => {
    await sendParitySpec(page, "expressions/bind-state.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-input"), `${name}`).toHaveValue("Alice")
    }
  })

  test("$bindState updates state on input change", async ({ page }) => {
    await sendParitySpec(page, "expressions/bind-state.json")

    for (const [name, root] of renderers(page)) {
      const input = root.locator(".jr-input")
      await input.clear()
      await input.fill("Bob")
      await expect(input, `${name}`).toHaveValue("Bob")
    }
  })

  test("$bindItem reads initial values from repeat items", async ({ page }) => {
    await sendParitySpec(page, "expressions/bind-item.json")

    for (const [name, root] of renderers(page)) {
      const inputs = root.locator(".jr-input")
      await expect(inputs, `${name}: count`).toHaveCount(2)
      await expect(inputs.nth(0), `${name}: first`).toHaveValue("hello")
      await expect(inputs.nth(1), `${name}: second`).toHaveValue("world")
    }
  })

  test("$bindItem updates item state on input change", async ({ page }) => {
    await sendParitySpec(page, "expressions/bind-item.json")

    for (const [name, root] of renderers(page)) {
      const secondInput = root.locator(".jr-input").nth(1)
      await secondInput.clear()
      await secondInput.fill("changed")
      await expect(secondInput, `${name}: changed`).toHaveValue("changed")
      await expect(root.locator(".jr-input").nth(0), `${name}: unaffected`).toHaveValue("hello")
    }
  })

  test("$computed resolves with registered function", async ({ page }) => {
    await sendParitySpec(page, "expressions/computed-basic.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("HELLO WORLD")
    }
  })

  // $computed error test lives in error-rendering.parity.ts

  test("$computed updates when bound state changes", async ({ page }) => {
    await sendParitySpec(page, "expressions/computed-with-bind.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-input"), `${name}: initial input`).toHaveValue("alice")
      await expect(root.locator(".jr-text"), `${name}: initial computed`).toHaveText("ALICE")

      const input = root.locator(".jr-input")
      await input.clear()
      await input.fill("bob")
      await expect(root.locator(".jr-text"), `${name}: updated computed`).toHaveText("BOB")
    }
  })
})
