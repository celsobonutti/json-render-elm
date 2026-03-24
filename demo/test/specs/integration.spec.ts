import { test, expect } from "@playwright/test"
import { sendSpec, setState, getLastAction } from "../helpers"

test.describe("Integration: Contact Form", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("full form flow: bind, type, submit, show success", async ({ page }) => {
    await sendSpec(page, "integration/contact-form.json")

    // Card renders
    await expect(page.locator(".jr-card-title")).toHaveText("Contact Us")

    // Success message is hidden initially
    await expect(page.locator(".jr-text")).toHaveCount(0)

    // Type into bound input
    const input = page.locator(".jr-input")
    await input.fill("Alice")
    await expect(input).toHaveValue("Alice")

    // Click submit — emits custom action
    await page.locator(".jr-button").click()
    const action = await getLastAction(page)
    expect(action).toEqual({ name: "press", params: null })

    // Simulate server response: set submitted = true
    // Must preserve the form.name state that was set by typing
    await setState(page, { form: { name: "Alice" }, submitted: true })

    // Success message appears with the typed name
    await expect(page.locator(".jr-text")).toHaveText("Thanks, Alice!")
  })
})
