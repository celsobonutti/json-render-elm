import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('app')
})

// Prompt → API → Spec
app.ports.sendPrompt.subscribe(async (prompt) => {
  try {
    const res = await fetch('/api/generate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ prompt })
    })
    const data = await res.json()
    if (data.error) {
      app.ports.receiveError.send(data.error)
    } else {
      app.ports.receiveSpec.send(data.spec)
    }
  } catch (e) {
    app.ports.receiveError.send(e.message)
  }
})

// Custom actions from rendered UI
app.ports.outgoingAction.subscribe((action) => {
  console.log('Custom action:', action)
})
