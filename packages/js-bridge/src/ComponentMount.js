// Web component that fires a "component-mounted" CustomEvent when first
// connected to the DOM. Used by stateful components to register their
// initial ComponentInstance in the Elm model.
if (typeof HTMLElement === "undefined") {
  // Not in a browser — skip registration (e.g. Node.js for codegen scripts)
} else {

class ComponentMount extends HTMLElement {
  connectedCallback() {
    this.style.display = 'contents';
    this.dispatchEvent(
      new CustomEvent('component-mounted', { bubbles: true })
    );
  }
}

if (!customElements.get("component-mount")) {
  customElements.define("component-mount", ComponentMount);
}

} // end browser guard
