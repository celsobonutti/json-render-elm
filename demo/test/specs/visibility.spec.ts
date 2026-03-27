import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Visibility", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("truthy condition shows element when true", async ({ page }) => {
    await sendSpec(page, "visibility/truthy-condition.json")
    await expect(page.locator(".jr-text")).toHaveText("Secret message")
  })

  test("truthy condition hides element when false", async ({ page }) => {
    await sendSpec(page, "visibility/truthy-condition.json")
    await setState(page, { show: false })
    await expect(page.locator(".jr-text")).toHaveCount(0)
  })

  test("$item condition shows matching items in repeat", async ({ page }) => {
    await sendSpec(page, "visibility/item-condition.json")
    const texts = page.locator(".jr-text")
    await expect(texts).toHaveCount(2)
    await expect(texts.nth(0)).toHaveText("Buy milk")
    await expect(texts.nth(1)).toHaveText("Read book")
  })

  test("$index condition hides first item", async ({ page }) => {
    await sendSpec(page, "visibility/index-condition.json")
    const texts = page.locator(".jr-text")
    await expect(texts).toHaveCount(2)
    await expect(texts.nth(0)).toHaveText("second")
    await expect(texts.nth(1)).toHaveText("third")
  })

  test("numeric gt comparison shows element when true", async ({ page }) => {
    await sendSpec(page, "visibility/numeric-comparison.json")
    await expect(page.locator(".jr-text")).toHaveText("Count is high")
  })

  test("numeric gt comparison hides element when false", async ({ page }) => {
    await sendSpec(page, "visibility/numeric-comparison.json")
    await setState(page, { count: 3 })
    await expect(page.locator(".jr-text")).toHaveCount(0)
  })

  test("state-to-state comparison shows when under limit", async ({ page }) => {
    await sendSpec(page, "visibility/state-to-state.json")
    await expect(page.locator(".jr-text")).toHaveText("Under limit")
  })

  test("state-to-state comparison hides when over limit", async ({ page }) => {
    await sendSpec(page, "visibility/state-to-state.json")
    await setState(page, { count: 15, limit: 10 })
    await expect(page.locator(".jr-text")).toHaveCount(0)
  })
})
