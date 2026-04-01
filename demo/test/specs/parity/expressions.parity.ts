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

  test("$cond with eq selects only the matching branch", async ({ page }) => {
    await sendParitySpec(page, "expressions/cond-eq.json")

    for (const [name, root] of renderers(page)) {
      const buttons = root.locator(".jr-button")

      // Initially no selection — both buttons should be secondary
      await expect(buttons.nth(0), `${name}: A initial`).toHaveClass(/jr-button-secondary/)
      await expect(buttons.nth(1), `${name}: B initial`).toHaveClass(/jr-button-secondary/)

      // Click button A — only A should become primary
      await buttons.nth(0).click()
      await expect(buttons.nth(0), `${name}: A after click A`).toHaveClass(/jr-button-primary/)
      await expect(buttons.nth(1), `${name}: B after click A`).toHaveClass(/jr-button-secondary/)

      // Click button B — only B should become primary
      await buttons.nth(1).click()
      await expect(buttons.nth(0), `${name}: A after click B`).toHaveClass(/jr-button-secondary/)
      await expect(buttons.nth(1), `${name}: B after click B`).toHaveClass(/jr-button-primary/)
    }
  })

  test("$cond with neq compares correctly", async ({ page }) => {
    await sendParitySpec(page, "expressions/cond-neq.json")

    for (const [name, root] of renderers(page)) {
      // role is "admin", neq "admin" is false → should show "full-access"
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("full-access")
    }
  })

  test("$cond with gt compares correctly", async ({ page }) => {
    await sendParitySpec(page, "expressions/cond-gt.json")

    for (const [name, root] of renderers(page)) {
      // count is 3, gt 5 is false → should show "few"
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("few")
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
