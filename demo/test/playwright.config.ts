import { defineConfig } from "@playwright/test"

const PORT = Number(process.env.VITE_PORT ?? 5173)

export default defineConfig({
  testDir: "./specs",
  webServer: {
    command: `npx vite --port ${PORT}`,
    port: PORT,
    cwd: "..",
    reuseExistingServer: true,
  },
  projects: [
    {
      name: "e2e",
      use: {
        baseURL: `http://localhost:${PORT}/test-harness.html`,
      },
    },
    {
      name: "parity",
      testMatch: "parity/**",
      use: {
        baseURL: `http://localhost:${PORT}/parity-harness.html`,
      },
    },
  ],
})
