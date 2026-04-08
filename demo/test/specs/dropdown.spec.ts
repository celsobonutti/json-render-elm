import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Dropdown (Web Component)", () => {
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

    // After Elm re-renders with new value, trigger should show selected label
    await expect(page.locator("jr-dropdown")).toHaveAttribute("value", "banana")
    await expect(page.locator(".jr-dropdown-trigger")).toContainText("Banana")
  })

  test("keyboard navigation: arrow down, enter selects", async ({ page }) => {
    await sendSpec(page, "dropdown/basic.json")
    await page.locator(".jr-dropdown-trigger").click()
    const search = page.locator(".jr-dropdown-search")

    await search.press("ArrowDown") // highlight Banana (index 1)
    await search.press("ArrowDown") // highlight Cherry (index 2)
    await search.press("Enter")

    await expect(page.locator(".jr-dropdown-panel")).not.toBeVisible()
    await expect(page.locator(".jr-dropdown-trigger-text")).toHaveText("Cherry")
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
    // After state update, Elm re-renders with value="cherry"
    await expect(page.locator(".jr-dropdown-trigger-text")).toHaveText("Cherry")
  })

  test("selected option is highlighted when dropdown opens", async ({
    page,
  }) => {
    await sendSpec(page, "dropdown/basic.json")
    await setState(page, { selected: "banana" })
    await page.locator(".jr-dropdown-trigger").click()
    await expect(
      page.locator(".jr-dropdown-option-selected")
    ).toHaveText("Banana")
  })
})
