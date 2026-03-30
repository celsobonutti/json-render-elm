import { test, expect } from "@playwright/test"
import { sendParitySpec, renderers } from "../../parity-helpers"

test.describe("Parity: State on Spec", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
  })

  test("spec state initializes $state expressions", async ({ page }) => {
    // state-read.json has { state: { greeting: "Hello Alice" } }
    await sendParitySpec(page, "expressions/state-read.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveText("Hello Alice")
    }
  })

  test("spec state initializes visibility conditions", async ({ page }) => {
    // truthy-condition.json has { state: { show: true } }
    await sendParitySpec(page, "visibility/truthy-condition.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toBeVisible()
    }
  })

  test("spec state initializes repeat arrays", async ({ page }) => {
    // todo-list.json has { state: { todos: [...3 items] } }
    await sendParitySpec(page, "repeat/todo-list.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-text"), `${name}`).toHaveCount(3)
    }
  })

  test("spec state initializes $bindState input values", async ({ page }) => {
    // bind-state.json has { state: { form: { name: "Alice" } } }
    await sendParitySpec(page, "expressions/bind-state.json")

    for (const [name, root] of renderers(page)) {
      await expect(root.locator(".jr-input"), `${name}`).toHaveValue("Alice")
    }
  })
})
