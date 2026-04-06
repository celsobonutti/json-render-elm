// Custom elements for validation field registration.
// <jr-validation-root> is the anchor element rendered by Elm at the render root.
// <validation-field> registers/unregisters on connect/disconnect by dispatching
// events on the root, which Elm catches via Html.Events.on.
if (typeof HTMLElement === "undefined") {
  // Not in a browser — skip (e.g. Node.js for codegen scripts)
} else {

class JrValidationRoot extends HTMLElement {}

if (!customElements.get("jr-validation-root")) {
  customElements.define("jr-validation-root", JrValidationRoot);
}

class ValidationField extends HTMLElement {
  connectedCallback() {
    // Walk up to find the <jr-validation-root> ancestor and cache it
    this._root = this.closest("jr-validation-root");
    if (this._root) {
      this._root.dispatchEvent(new CustomEvent("validation-register", {
        bubbles: false,
        detail: {
          path: this.getAttribute("data-path"),
          config: this.getAttribute("data-config")
        }
      }));
    }
  }

  disconnectedCallback() {
    // _root is still in the DOM — dispatch the unregister event on it
    if (this._root) {
      this._root.dispatchEvent(new CustomEvent("validation-unregister", {
        bubbles: false,
        detail: { path: this.getAttribute("data-path") }
      }));
      this._root = null;
    }
  }
}

if (!customElements.get("validation-field")) {
  customElements.define("validation-field", ValidationField);
}

} // end browser guard
