import Anthropic from "@anthropic-ai/sdk"
import { compileSpecStream } from "@json-render/core"
import type { Connect } from "vite"
import { catalog } from "../catalog.ts"

export function claudeMiddleware(app: Connect.Server): void {
  const systemPrompt = catalog.prompt({
    customRules: [
      "For images, use placeholder URLs from https://placehold.co (e.g., https://placehold.co/600x400)",
    ],
  })

  app.use(async (req, res, next) => {
    if (req.url !== "/api/generate" || req.method !== "POST") {
      return next()
    }

    let body = ""
    for await (const chunk of req) {
      body += chunk
    }

    try {
      const { prompt } = JSON.parse(body)

      if (!prompt || typeof prompt !== "string") {
        res.writeHead(400, { "Content-Type": "application/json" })
        res.end(JSON.stringify({ error: "Missing prompt" }))
        return
      }

      const client = new Anthropic()

      const response = await client.messages.create({
        model: "claude-sonnet-4-6",
        max_tokens: 4096,
        system: systemPrompt,
        messages: [{ role: "user", content: prompt }],
      })

      const text = response.content.find((b) => b.type === "text")
      if (!text) {
        res.writeHead(500, { "Content-Type": "application/json" })
        res.end(JSON.stringify({ error: "No text in response" }))
        return
      }

      // Claude outputs JSONL (JSON Patch operations) per json-render's protocol.
      // compileSpecStream compiles them into the flat spec { root, elements }.
      const spec = compileSpecStream(text.text)

      res.writeHead(200, { "Content-Type": "application/json" })
      res.end(JSON.stringify({ spec }))
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err)
      res.writeHead(500, { "Content-Type": "application/json" })
      res.end(JSON.stringify({ error: message }))
    }
  })
}
