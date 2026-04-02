import { test, expect } from "@playwright/test"
import { sendSpec } from "../helpers"

test.describe("Fun Activity: Quiz", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("")
    await page.locator("#render-root").waitFor({ state: "attached" })
    await sendSpec(page, "rendering/fun-activity.json")
  })

  test("renders quiz header with score and question badges", async ({
    page,
  }) => {
    await expect(page.locator(".jr-card-title").first()).toHaveText(
      "🧠 Quiz de Curiosidades"
    )
    await expect(page.locator(".jr-badge").first()).toContainText("Pontuação: 0")
    await expect(page.locator(".jr-badge").nth(1)).toContainText(
      "Pergunta 1 de 5"
    )
  })

  test("shows question 1 with four answer buttons", async ({ page }) => {
    await expect(page.locator(".jr-text")).toContainText(
      "Qual é o maior oceano do mundo?"
    )
    const buttons = page.locator(".jr-button")
    await expect(buttons).toHaveCount(4)
    await expect(buttons.nth(0)).toContainText("Oceano Atlântico")
    await expect(buttons.nth(1)).toContainText("Oceano Pacífico")
  })

  test("clicking wrong answer shows feedback and next button", async ({
    page,
  }) => {
    // Click wrong answer
    await page.locator(".jr-button", { hasText: "Oceano Atlântico" }).click()

    // Feedback badge appears
    await expect(page.locator(".jr-badge", { hasText: "Errado" })).toBeVisible()

    // Next button appears
    await expect(
      page.locator(".jr-button", { hasText: "Próxima Pergunta" })
    ).toBeVisible()

    // Score stays at 0
    await expect(page.locator(".jr-badge").first()).toContainText("Pontuação: 0")
  })

  test("clicking correct answer increments score", async ({ page }) => {
    // Click correct answer (Oceano Pacífico)
    await page.locator(".jr-button", { hasText: "Oceano Pacífico" }).click()

    // Correct feedback
    await expect(
      page.locator(".jr-badge", { hasText: "Correto" })
    ).toBeVisible()

    // Score increments to 1
    await expect(page.locator(".jr-badge").first()).toContainText("Pontuação: 1")
  })

  test("next button advances to question 2", async ({ page }) => {
    await page.locator(".jr-button", { hasText: "Oceano Pacífico" }).click()
    await page.locator(".jr-button", { hasText: "Próxima Pergunta" }).click()

    // Question 2 text appears
    await expect(page.locator(".jr-text")).toContainText("Rei da Selva")

    // Question badge updates
    await expect(page.locator(".jr-badge").nth(1)).toContainText(
      "Pergunta 2 de 5"
    )

    // Feedback is cleared
    await expect(
      page.locator(".jr-badge", { hasText: "Correto" })
    ).toHaveCount(0)
    await expect(
      page.locator(".jr-badge", { hasText: "Errado" })
    ).toHaveCount(0)
  })

  test("full quiz flow through all 5 questions to result", async ({
    page,
  }) => {
    // Q1: correct (Pacífico)
    await page.locator(".jr-button", { hasText: "Oceano Pacífico" }).click()
    await page.locator(".jr-button", { hasText: "Próxima Pergunta" }).click()

    // Q2: correct (Leão)
    await page.locator(".jr-button", { hasText: "Leão" }).click()
    await page.locator(".jr-button", { hasText: "Próxima Pergunta" }).click()

    // Q3: wrong (Terra)
    await page.locator(".jr-button", { hasText: "Terra" }).click()
    await page.locator(".jr-button", { hasText: "Próxima Pergunta" }).click()

    // Q4: correct (Uruguai)
    await page.locator(".jr-button", { hasText: "Uruguai" }).click()
    await page.locator(".jr-button", { hasText: "Próxima Pergunta" }).click()

    // Q5: correct (Leonardo da Vinci)
    await page.locator(".jr-button", { hasText: "Leonardo da Vinci" }).click()
    await page.locator(".jr-button", { hasText: "Ver Resultado" }).click()

    // Result card appears with score 4/5
    await expect(
      page.locator(".jr-card-title", { hasText: "Resultado Final" })
    ).toBeVisible()
    const resultCard = page.locator(".jr-card", {
      has: page.locator(".jr-card-title", { hasText: "Resultado Final" }),
    })
    await expect(resultCard.locator(".jr-card-subtitle")).toContainText(
      "4 de 5"
    )
    await expect(
      page.locator(".jr-badge", { hasText: "4/5" })
    ).toBeVisible()
  })
})
