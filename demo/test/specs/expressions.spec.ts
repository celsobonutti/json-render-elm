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
})
