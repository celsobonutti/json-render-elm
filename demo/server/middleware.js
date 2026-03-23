import Anthropic from "@anthropic-ai/sdk"
import { buildSystemPrompt, specSchema } from "../catalog.js"

export function claudeMiddleware(app) {
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
        system: buildSystemPrompt(),
        output_config: {
          format: {
            type: "json_schema",
            schema: specSchema,
          },
        },
        messages: [{ role: "user", content: prompt }],
      })

      const text = response.content.find((b) => b.type === "text")
      if (!text) {
        res.writeHead(500, { "Content-Type": "application/json" })
        res.end(JSON.stringify({ error: "No text in response" }))
        return
      }

      const spec = JSON.parse(text.text)

      res.writeHead(200, { "Content-Type": "application/json" })
      res.end(JSON.stringify({ spec }))
    } catch (err) {
      res.writeHead(500, { "Content-Type": "application/json" })
      res.end(JSON.stringify({ error: err.message }))
    }
  })
}
