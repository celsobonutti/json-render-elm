import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Form Validation", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("required: shows error on empty submit", async ({ page }) => {
    await sendSpec(page, "validation/required.json")
    await page.locator(".jr-button").click()
    await expect(page.locator(".jr-input-error")).toHaveText("Name is required")
  })

  test("required: clears error when field filled and resubmitted", async ({ page }) => {
    await sendSpec(page, "validation/required.json")
    await page.locator(".jr-button").click()
    await expect(page.locator(".jr-input-error")).toBeVisible()
    await page.locator(".jr-input").fill("Alice")
    await page.locator(".jr-button").click()
    await expect(page.locator(".jr-input-error")).not.toBeVisible()
  })

  test("email: shows error for invalid email", async ({ page }) => {
    await sendSpec(page, "validation/email.json")
    await page.locator(".jr-input").fill("not-an-email")
    await page.locator(".jr-button").click()
    await expect(page.locator(".jr-input-error")).toHaveText("Invalid email address")
  })

  test("email: no error for valid email", async ({ page }) => {
    await sendSpec(page, "validation/email.json")
    await page.locator(".jr-input").fill("user@example.com")
    await page.locator(".jr-button").click()
    await expect(page.locator(".jr-input-error")).not.toBeVisible()
  })

  test("cross-field: passwords must match error", async ({ page }) => {
    await sendSpec(page, "validation/cross-field.json")
    const inputs = page.locator(".jr-input")
    await inputs.nth(0).fill("password123")
    await inputs.nth(1).fill("different")
    await page.locator(".jr-button").click()
    await expect(page.locator(".jr-input-error").last()).toHaveText("Passwords must match")
  })

  test("blur: validates on blur when validateOn is blur", async ({ page }) => {
    await sendSpec(page, "validation/validate-on-blur.json")
    const input = page.locator(".jr-input")
    await input.focus()
    await input.blur()
    await expect(page.locator(".jr-input-error")).toHaveText("Name is required")
  })

  test("full form: submit empty shows all errors", async ({ page }) => {
    await sendSpec(page, "validation/full-form.json")
    await page.locator(".jr-button").click()
    const errors = page.locator(".jr-input-error")
    await expect(errors).toHaveCount(2)
  })

  test("full form: fill and submit shows success", async ({ page }) => {
    await sendSpec(page, "validation/full-form.json")
    const inputs = page.locator(".jr-input")
    await inputs.nth(0).fill("Alice")
    await inputs.nth(1).fill("alice@example.com")
    await page.locator(".jr-button").click()
    await expect(page.locator(".jr-text")).toContainText("All valid!")
  })
})
