// Web component that fires a "watcher-triggered" CustomEvent when its
// "value" attribute changes. Skips the initial set (connectedCallback)
// so watchers don't fire on first render.
if (typeof HTMLElement === "undefined") {
  // Not in a browser — skip registration (e.g. Node.js for codegen scripts)
} else {

class WatcherTrigger extends HTMLElement {
  static get observedAttributes() {
    return ["value"];
  }

  connectedCallback() {
    this._initialized = true;
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === "value" && this._initialized && oldValue !== null && oldValue !== newValue) {
      // Defer to next microtask so the event fires after Elm's DOM patching completes
      queueMicrotask(() => {
        this.dispatchEvent(new CustomEvent("watcher-triggered", { bubbles: true }));
      });
    }
  }
}

if (!customElements.get("watcher-trigger")) {
  customElements.define("watcher-trigger", WatcherTrigger);
}

} // end browser guard
