// Web component for stateful component lifecycle.
// Fires "component-mounted" on first connect and "props-changed" when the
// data-props attribute changes (skipping the initial set).
if (typeof HTMLElement === "undefined") {
  // Not in a browser — skip registration (e.g. Node.js for codegen scripts)
} else {

class ComponentMount extends HTMLElement {
  static get observedAttributes() {
    return ["data-props"];
  }

  connectedCallback() {
    this.style.display = 'contents';
    this._initialized = true;
    this.dispatchEvent(
      new CustomEvent('component-mounted', {
        bubbles: true,
        detail: {
          instanceId: this.getAttribute('data-instance'),
          componentType: this.getAttribute('data-component-type')
        }
      })
    );
  }

  disconnectedCallback() {
    // Can't dispatch bubbling events after DOM removal — use global registry
    const instanceId = this.getAttribute('data-instance');
    const componentType = this.getAttribute('data-component-type');
    if (instanceId && window.__jsonRenderPortCleanup) {
      window.__jsonRenderPortCleanup(instanceId, componentType);
    }
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === "data-props" && this._initialized && oldValue !== null && oldValue !== newValue) {
      queueMicrotask(() => {
        this.dispatchEvent(new CustomEvent("props-changed", { bubbles: true }));
      });
    }
  }
}

if (!customElements.get("component-mount")) {
  customElements.define("component-mount", ComponentMount);
}

} // end browser guard
