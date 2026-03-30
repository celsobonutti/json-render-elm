import { test, expect } from "@playwright/test"
import {
  sendParitySpec,
  setParityState,
  renderers,
} from "../../parity-helpers"

test.describe("Parity: Visibility", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("truthy condition shows element when true", async ({ page }) => {
    await sendParitySpec(page, "visibility/truthy-condition.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("Secret message")
    }
  })

  test("truthy condition hides element when false", async ({ page }) => {
    await sendParitySpec(page, "visibility/truthy-condition.json")
    await setParityState(page, { show: false })

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveCount(0)
    }
  })

  test("$item condition shows matching items in repeat", async ({ page }) => {
    await sendParitySpec(page, "visibility/item-condition.json")

    for (const [name, root] of renderers(page)) {
      const texts = root.locator(".jr-text")
      await expect(texts, `${name}: count`).toHaveCount(2)
      await expect(texts.nth(0), `${name}: first`).toHaveText("Buy milk")
      await expect(texts.nth(1), `${name}: second`).toHaveText("Read book")
    }
  })

  test("$index condition hides first item", async ({ page }) => {
    await sendParitySpec(page, "visibility/index-condition.json")

    for (const [name, root] of renderers(page)) {
      const texts = root.locator(".jr-text")
      await expect(texts, `${name}: count`).toHaveCount(2)
      await expect(texts.nth(0), `${name}: first`).toHaveText("second")
      await expect(texts.nth(1), `${name}: second`).toHaveText("third")
    }
  })

  test("numeric gt comparison shows element when true", async ({ page }) => {
    await sendParitySpec(page, "visibility/numeric-comparison.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("Count is high")
    }
  })

  test("numeric gt comparison hides element when false", async ({ page }) => {
    await sendParitySpec(page, "visibility/numeric-comparison.json")
    await setParityState(page, { count: 3 })

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveCount(0)
    }
  })

  test("state-to-state comparison shows when under limit", async ({ page }) => {
    await sendParitySpec(page, "visibility/state-to-state.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("Under limit")
    }
  })

  test("state-to-state comparison hides when over limit", async ({ page }) => {
    await sendParitySpec(page, "visibility/state-to-state.json")
    await setParityState(page, { count: 15, limit: 10 })

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveCount(0)
    }
  })
})
