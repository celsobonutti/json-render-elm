import { test, expect } from "@playwright/test"
import { sendSpec, getLastAction } from "../helpers"

test.describe("Actions", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("button click emits custom action via port", async ({ page }) => {
    await sendSpec(page, "actions/custom-action.json")
    await page.locator(".jr-button").click()

    const action = await getLastAction(page)
    expect(action).toEqual({ name: "press", params: null })
  })
})
