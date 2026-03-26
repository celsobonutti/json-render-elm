import { test, expect } from "@playwright/test"
import { sendSpec, setState } from "../helpers"

test.describe("Todo App", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
  })

  test.describe("State loading from spec", () => {
    test("renders initial todos from spec state", async ({ page }) => {
      await sendSpec(page, "todo/todo-full.json")

      // All 4 todo titles render
      const titles = page.locator(".jr-card-title")
      await expect(titles).toHaveCount(5) // 1 main + 4 todos
      await expect(titles.nth(1)).toHaveText("Buy groceries")
      await expect(titles.nth(2)).toHaveText("Read a book")
      await expect(titles.nth(3)).toHaveText("Go for a run")
      await expect(titles.nth(4)).toHaveText("Write project proposal")
    })

    test("renders input with bound state", async ({ page }) => {
      await sendSpec(page, "todo/todo-full.json")

      const input = page.locator(".jr-input")
      await expect(input).toHaveValue("")
    })
  })

  test.describe("$cond expressions", () => {
    test("resolves $cond in badge labels based on completed status", async ({
      page,
    }) => {
      await sendSpec(page, "todo/todo-full.json")

      const badges = page.locator(".jr-badge")
      // Item 1: completed=false -> "To-Do", Item 2: completed=true -> "Completed"
      await expect(badges.nth(0)).toHaveText("To-Do")
      await expect(badges.nth(1)).toHaveText("Completed")
      await expect(badges.nth(2)).toHaveText("To-Do")
      await expect(badges.nth(3)).toHaveText("To-Do")
    })

    test("resolves $cond in button labels", async ({ page }) => {
      await sendSpec(page, "todo/todo-full.json")

      const toggleButtons = page.locator(
        ".jr-button-primary, .jr-button-secondary"
      )
      // Item 1 (not completed) -> "Mark Completed" (primary)
      // Item 2 (completed) -> "Mark To-Do" (secondary)
      await expect(
        page.locator(".jr-button-primary").filter({ hasText: "Mark Completed" })
      ).toHaveCount(3)
      await expect(
        page.locator(".jr-button-secondary").filter({ hasText: "Mark To-Do" })
      ).toHaveCount(1)
    })
  })

  test.describe("Toggle completed via $item action params", () => {
    test("clicking toggle changes completed status", async ({ page }) => {
      await sendSpec(page, "todo/todo-toggle.json")

      // First item starts completed=true with badge "Done"
      const firstBadge = page.locator(".jr-badge").first()
      await expect(firstBadge).toHaveText("Done")

      // Click "Unmark" on first item
      const unmarkBtn = page
        .locator(".jr-button-secondary")
        .filter({ hasText: "Unmark" })
        .first()
      await unmarkBtn.click()

      // Badge should switch to "Todo"
      await expect(firstBadge).toHaveText("Todo")
    })

    test("clicking toggle on incomplete item marks it complete", async ({
      page,
    }) => {
      await sendSpec(page, "todo/todo-toggle.json")

      // Second item starts completed=false
      const secondBadge = page.locator(".jr-badge").nth(1)
      await expect(secondBadge).toHaveText("Todo")

      // Click "Complete" on second item
      const completeBtn = page
        .locator(".jr-button-primary")
        .filter({ hasText: "Complete" })
        .first()
      await completeBtn.click()

      // Badge should switch to "Done"
      await expect(secondBadge).toHaveText("Done")
    })
  })

  test.describe("Delete via removeState with $index", () => {
    test("clicking delete removes the item", async ({ page }) => {
      await sendSpec(page, "todo/todo-full.json")

      // 4 todo items initially
      await expect(page.locator(".jr-badge")).toHaveCount(4)

      // Click first Delete button
      const deleteBtn = page
        .locator(".jr-button-danger")
        .filter({ hasText: "Delete" })
        .first()
      await deleteBtn.click()

      // 3 todo items remain, "Buy groceries" is gone
      await expect(page.locator(".jr-badge")).toHaveCount(3)
      await expect(page.locator(".jr-card-title")).not.toContainText([
        "Buy groceries",
      ])
    })
  })

  test.describe("Add via pushState with statePath", () => {
    test("typing and clicking Add adds a new todo", async ({ page }) => {
      await sendSpec(page, "todo/todo-full.json")

      // 4 todos initially
      await expect(page.locator(".jr-badge")).toHaveCount(4)

      // Type a new todo
      const input = page.locator(".jr-input")
      await input.fill("New task from test")

      // Click Add
      await page.locator(".jr-button-primary").filter({ hasText: "Add" }).click()

      // 5 todos now
      await expect(page.locator(".jr-badge")).toHaveCount(5)
    })
  })

  test.describe("Visibility with $state format", () => {
    test("empty message is hidden when todos exist", async ({ page }) => {
      await sendSpec(page, "todo/todo-with-visibility.json")

      // Todos are visible
      await expect(page.locator(".jr-badge")).toHaveCount(3)

      // Empty message is not shown
      await expect(
        page.locator(".jr-text").filter({ hasText: "No tasks yet" })
      ).toHaveCount(0)
    })

    test("empty message appears when all todos are deleted", async ({
      page,
    }) => {
      await sendSpec(page, "todo/todo-with-visibility.json")

      // Delete all 3 todos
      for (let i = 0; i < 3; i++) {
        await page
          .locator(".jr-button-danger")
          .filter({ hasText: "Delete" })
          .first()
          .click()
      }

      // Empty message appears
      await expect(
        page.locator(".jr-text").filter({ hasText: "No tasks yet" })
      ).toHaveCount(1)
    })
  })

  test.describe("Props error display", () => {
    test("shows error when required prop is missing", async ({ page }) => {
      await sendSpec(page, "todo/todo-missing-props.json")

      // The todo-item Card has empty props (missing required title)
      // Should show props error for each repeated item
      const errors = page.locator("text=Props error")
      await expect(errors).toHaveCount(4)
    })
  })

  test.describe("Todo with badges variant", () => {
    test("renders badges with correct colors", async ({ page }) => {
      await sendSpec(page, "todo/todo-with-badges.json")

      // 4 items: 1 completed (green), 3 not completed (yellow)
      // Item 2 is completed
      const badges = page.locator(".jr-badge")
      await expect(badges).toHaveCount(4)
      await expect(badges.nth(0)).toHaveText("Pending")
      await expect(badges.nth(1)).toHaveText("Done")
      await expect(badges.nth(2)).toHaveText("Pending")
      await expect(badges.nth(3)).toHaveText("Pending")
    })
  })
})
