import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Dropdown (Mini Elm App)", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test("renders with placeholder when no value selected", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    const trigger = page.locator(".jr-dropdown-trigger")
    await expect(trigger).toBeVisible()
    await expect(trigger.locator(".jr-dropdown-trigger-placeholder")).toHaveText(
      "Choose a fruit..."
    )
  })

  test("click opens dropdown panel", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    await page.locator(".jr-dropdown-trigger").click()
    await expect(page.locator(".jr-dropdown-panel")).toBeVisible()
    await expect(page.locator(".jr-dropdown-option")).toHaveCount(5)
  })

  test("search filters options", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    await page.locator(".jr-dropdown-trigger").click()
    await page.locator(".jr-dropdown-search").fill("ch")
    await expect(page.locator(".jr-dropdown-option")).toHaveCount(1)
    await expect(page.locator(".jr-dropdown-option").first()).toHaveText("Cherry")
  })

  test("clicking an option updates json-render state", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    await page.locator(".jr-dropdown-trigger").click()
    await page.locator(".jr-dropdown-option", { hasText: "Banana" }).click()
    // Verify the binding round-trip: Elm state updated, re-rendered with new value
    await expect(page.locator(".jr-dropdown-trigger")).toContainText("Banana")
  })

  test("keyboard navigation: arrow down, enter selects", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    await page.locator(".jr-dropdown-trigger").click()
    const search = page.locator(".jr-dropdown-search")
    await search.press("ArrowDown")
    await search.press("ArrowDown")
    await search.press("Enter")
    await expect(page.locator(".jr-dropdown-trigger")).toContainText("Cherry")
  })

  test("escape closes dropdown", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    await page.locator(".jr-dropdown-trigger").click()
    await expect(page.locator(".jr-dropdown-panel")).toBeVisible()
    await page.locator(".jr-dropdown-search").press("Escape")
    await expect(page.locator(".jr-dropdown-panel")).not.toBeVisible()
  })

  test("reflects pre-selected value from state", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    await setState(page, { selected: "cherry" })
    await expect(page.locator(".jr-dropdown-trigger")).toContainText("Cherry")
  })
})
