// Custom element that mounts a mini Elm app (Browser.element) for each
// json-render component instance. Bridges parent Elm ↔ mini Elm via
// attributes/ports/CustomEvents.
if (typeof HTMLElement === "undefined") {
  // Not in a browser — skip (e.g. Node.js for codegen scripts)
} else {

class ElmComponent extends HTMLElement {
  static get observedAttributes() {
    return ["props", "bindings", "events", "validation", "validate-on"];
  }

  connectedCallback() {
    // If already initialized (re-parented), skip re-init
    this._disconnecting = false;
    if (this._app) return;

    const type = this.getAttribute("type");
    const registry = window.__jrElmComponents;
    if (!registry || !registry[type]) {
      this.textContent = `Unknown component: ${type}`;
      return;
    }

    // Create a container for the mini app
    this._container = document.createElement("div");
    this._container.style.display = "contents";
    this.appendChild(this._container);

    // Collect flags from attributes
    const flags = this._collectFlags();

    // Mount the mini Elm app
    this._app = registry[type].init({
      node: this._container,
      flags: flags,
    });

    // Track the actual root element (vite-plugin-elm's HMR wrapper replaces
    // _container with a div[data-elm-hot], so we need to find it)
    this._elmRoot = Array.from(this.children).find(
      (c) => c.hasAttribute && c.hasAttribute("data-elm-hot")
    ) || this._container;

    // Listen for actions from the mini app
    if (this._app.ports && this._app.ports.actionsOut) {
      this._app.ports.actionsOut.subscribe((action) => {
        // Skip empty init actions (mini apps send {} on init)
        if (action && typeof action === "object" && Object.keys(action).length === 0) return;
        this.dispatchEvent(
          new CustomEvent("jr-component-action", {
            detail: action,
            bubbles: true,
          })
        );
      });
    }

    // For stateful components with children, observe for the slot to appear
    // and move children into it.
    this._observeSlot();
  }

  _observeSlot() {
    const root = this._elmRoot;
    if (!root) return;
    // Check immediately
    if (this._tryMoveChildren(root)) return;
    // Watch for the slot to appear after async render
    this._slotObserver = new MutationObserver(() => {
      if (this._tryMoveChildren(root)) {
        this._slotObserver.disconnect();
        this._slotObserver = null;
      }
    });
    this._slotObserver.observe(root, { childList: true, subtree: true });
  }

  _tryMoveChildren(root) {
    const slot = root.querySelector("[data-children-slot]");
    if (!slot) return false;
    const children = Array.from(this.children).filter(
      (c) => c !== root && c !== this._container
    );
    for (const child of children) {
      slot.appendChild(child);
    }
    return true;
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (!this._app || oldValue === newValue) return;
    if (this._app.ports && this._app.ports.propsIn) {
      this._app.ports.propsIn.send(this._collectFlags());
    }
  }

  disconnectedCallback() {
    // When _moveChildren re-parents this element, disconnectedCallback fires
    // followed by connectedCallback. Use a microtask to defer cleanup.
    this._disconnecting = true;
    queueMicrotask(() => {
      if (!this._disconnecting) return;
      if (this._slotObserver) {
        this._slotObserver.disconnect();
        this._slotObserver = null;
      }
      if (this._container) {
        this._container.remove();
        this._container = null;
      }
      this._elmRoot = null;
      this._app = null;
    });
  }

  _collectFlags() {
    return {
      props: this._parseJsonAttr("props"),
      bindings: this._parseJsonAttr("bindings"),
      events: this._parseJsonAttr("events"),
      validation: this._parseJsonAttr("validation"),
      validateOn: this.getAttribute("validate-on") || "submit",
    };
  }

  _parseJsonAttr(name) {
    const val = this.getAttribute(name);
    if (!val) return {};
    try {
      return JSON.parse(val);
    } catch {
      return {};
    }
  }
}

if (!customElements.get("jr-elm-component")) {
  customElements.define("jr-elm-component", ElmComponent);
}

} // end browser guard
