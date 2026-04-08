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

    // Listen for actions from the mini app
    if (this._app.ports && this._app.ports.actionsOut) {
      this._app.ports.actionsOut.subscribe((action) => {
        this.dispatchEvent(
          new CustomEvent("jr-component-action", {
            detail: action,
            bubbles: true,
          })
        );
      });
    }
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (!this._app || oldValue === newValue) return;
    if (this._app.ports && this._app.ports.propsIn) {
      this._app.ports.propsIn.send(this._collectFlags());
    }
  }

  disconnectedCallback() {
    // Elm doesn't have a destroy API, but we clean up the container
    if (this._container) {
      this._container.remove();
      this._container = null;
    }
    this._app = null;
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
