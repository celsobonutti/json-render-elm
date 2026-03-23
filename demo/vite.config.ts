import { defineConfig } from "vite"
import elmPlugin from "vite-plugin-elm"
import { claudeMiddleware } from "./server/middleware.ts"

export default defineConfig({
  plugins: [
    elmPlugin(),
    {
      name: "claude-api",
      configureServer(server) {
        claudeMiddleware(server.middlewares)
      },
    },
  ],
})
