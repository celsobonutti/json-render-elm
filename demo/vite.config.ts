import { defineConfig } from "vite"
import elmPlugin from "vite-plugin-elm"
import react from "@vitejs/plugin-react"
import { claudeMiddleware } from "./server/middleware.ts"

export default defineConfig({
  plugins: [
    elmPlugin(),
    react(),
    {
      name: "claude-api",
      configureServer(server) {
        claudeMiddleware(server.middlewares)
      },
    },
  ],
})
