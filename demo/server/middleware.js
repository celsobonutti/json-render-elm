import Anthropic from '@anthropic-ai/sdk'

const SYSTEM_PROMPT = `You generate json-render UI specs from user descriptions.

Spec format:
{
  "root": "<element-id>",
  "elements": {
    "<element-id>": {
      "type": "<ComponentName>",
      "props": { ... },
      "children": ["<child-id>", ...]
    }
  }
}

Available components:
- Card: props { title: string, subtitle?: string }, has children
- Button: props { label: string, variant?: "primary"|"secondary"|"danger" }
- Text: props { content: string, size?: "sm"|"md"|"lg"|"xl" }
- Input: props { placeholder?: string, label?: string }
- Stack: props { direction?: "vertical"|"horizontal", gap?: number }, has children
- Image: props { src: string, alt: string }
- Badge: props { label: string, color?: "green"|"red"|"yellow"|"blue"|"gray" }

Rules:
- Element IDs must be unique strings (e.g., "card-1", "text-1")
- Only use components listed above
- Children array contains element IDs, not objects
- Components without children must have an empty children array: []
- For images, use placeholder URLs from https://placehold.co (e.g., https://placehold.co/600x400)
- Generate a complete, visually interesting UI that fulfills the user's request`

const SPEC_SCHEMA = {
  type: 'object',
  properties: {
    root: { type: 'string' },
    elements: {
      type: 'object',
      additionalProperties: {
        type: 'object',
        properties: {
          type: { type: 'string' },
          props: { type: 'object' },
          children: { type: 'array', items: { type: 'string' } }
        },
        required: ['type', 'props', 'children'],
        additionalProperties: false
      }
    }
  },
  required: ['root', 'elements'],
  additionalProperties: false
}

export function claudeMiddleware(app) {
  app.use(async (req, res, next) => {
    if (req.url !== '/api/generate' || req.method !== 'POST') {
      return next()
    }

    let body = ''
    for await (const chunk of req) {
      body += chunk
    }

    try {
      const { prompt } = JSON.parse(body)

      if (!prompt || typeof prompt !== 'string') {
        res.writeHead(400, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ error: 'Missing prompt' }))
        return
      }

      const client = new Anthropic()

      const response = await client.messages.create({
        model: 'claude-sonnet-4-6',
        max_tokens: 4096,
        system: SYSTEM_PROMPT,
        output_config: {
          format: {
            type: 'json_schema',
            schema: SPEC_SCHEMA
          }
        },
        messages: [{ role: 'user', content: prompt }]
      })

      const text = response.content.find(b => b.type === 'text')
      if (!text) {
        res.writeHead(500, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ error: 'No text in response' }))
        return
      }

      const spec = JSON.parse(text.text)

      res.writeHead(200, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ spec }))
    } catch (err) {
      res.writeHead(500, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ error: err.message }))
    }
  })
}
