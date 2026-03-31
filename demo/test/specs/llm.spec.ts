import { test, expect } from "@playwright/test"
import { generateAndRender } from "../helpers"

const hasApiKey = !!process.env.ANTHROPIC_API_KEY

test.describe("LLM end-to-end", () => {
  test.skip(!hasApiKey, "ANTHROPIC_API_KEY not set")
  // LLM calls can be slow
  test.setTimeout(120_000)

  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("generates and renders a user profile card", async ({ page }) => {
    await generateAndRender(
      page,
      "A simple user profile card with a name, email, and a bio"
    )

    await expect(page.locator("text=Props error")).toHaveCount(0)
    await expect(page.locator(".jr-card")).not.toHaveCount(0)
    await expect(page.locator(".jr-text")).not.toHaveCount(0)
  })

  test("generates and renders a todo list with input and buttons", async ({
    page,
  }) => {
    await generateAndRender(
      page,
      "A to-do list with an input to add tasks, and each task has a complete button and delete button"
    )

    await expect(page.locator("text=Props error")).toHaveCount(0)
    await expect(page.locator(".jr-card")).not.toHaveCount(0)
    await expect(page.locator(".jr-input")).not.toHaveCount(0)
    await expect(page.locator(".jr-button")).not.toHaveCount(0)
  })

  test("generates and renders a dashboard with multiple cards", async ({
    page,
  }) => {
    await generateAndRender(
      page,
      "A dashboard with 3 metric cards showing revenue, users, and orders — each with a number and a label"
    )

    await expect(page.locator("text=Props error")).toHaveCount(0)
    await expect(page.locator(".jr-card")).not.toHaveCount(0)
    await expect(page.locator(".jr-text")).not.toHaveCount(0)
  })

  test("generates and renders a form with visibility conditions", async ({
    page,
  }) => {
    await generateAndRender(
      page,
      "A feedback form with a text input for comments, a submit button, and a success message that only shows after clicking submit"
    )

    await expect(page.locator("text=Props error")).toHaveCount(0)
    await expect(page.locator(".jr-input")).not.toHaveCount(0)
    await expect(page.locator(".jr-button")).not.toHaveCount(0)
  })
})
