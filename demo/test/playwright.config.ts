import { defineConfig } from "@playwright/test"

export default defineConfig({
  testDir: "./specs",
  webServer: {
    command: "npx vite --port 5173",
    port: 5173,
    cwd: "..",
    reuseExistingServer: true,
  },
  projects: [
    {
      name: "e2e",
      use: {
        baseURL: "http://localhost:5173/test-harness.html",
      },
    },
    {
      name: "parity",
      testMatch: "parity/**",
      use: {
        baseURL: "http://localhost:5173/parity-harness.html",
      },
    },
  ],
})
