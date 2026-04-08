interface DropdownOption {
  label: string
  value: string
}

class JrDropdown extends HTMLElement {
  private _open = false
  private _searchText = ""
  private _highlightedIndex = 0
  private _options: DropdownOption[] = []
  private _value = ""
  private _outsideClickHandler: (e: MouseEvent) => void

  static get observedAttributes() {
    return ["options", "value", "placeholder", "label"]
  }

  constructor() {
    super()
    this._outsideClickHandler = (e: MouseEvent) => {
      if (this._open && !this.contains(e.target as Node)) {
        this._open = false
        this._searchText = ""
        this._highlightedIndex = 0
        this._render()
      }
    }
  }

  connectedCallback() {
    this._parseAttributes()
    this._render()
    document.addEventListener("click", this._outsideClickHandler)
  }

  disconnectedCallback() {
    document.removeEventListener("click", this._outsideClickHandler)
  }

  attributeChangedCallback(_name: string, oldValue: string | null, newValue: string | null) {
    if (oldValue === newValue) return
    this._parseAttributes()
    this._render()
  }

  private _parseAttributes() {
    const optionsAttr = this.getAttribute("options")
    if (optionsAttr) {
      try {
        this._options = JSON.parse(optionsAttr)
      } catch {
        this._options = []
      }
    }
    this._value = this.getAttribute("value") ?? ""
  }

  private get _filteredOptions(): DropdownOption[] {
    if (!this._searchText) return this._options
    const lower = this._searchText.toLowerCase()
    return this._options.filter((o) => o.label.toLowerCase().includes(lower))
  }

  private get _selectedLabel(): string {
    const opt = this._options.find((o) => o.value === this._value)
    return opt ? opt.label : ""
  }

  private _selectOption(option: DropdownOption) {
    this._open = false
    this._searchText = ""
    this._highlightedIndex = 0
    this._render()
    this.dispatchEvent(
      new CustomEvent("jr-action", {
        detail: { event: "change", value: option.value },
        bubbles: true,
      })
    )
  }

  private _onTriggerClick(e: MouseEvent) {
    e.stopPropagation()
    this._open = !this._open
    if (!this._open) {
      this._searchText = ""
      this._highlightedIndex = 0
    }
    this._render()
    if (this._open) {
      const search = this.querySelector<HTMLInputElement>(".jr-dropdown-search")
      search?.focus()
    }
  }

  private _onSearchInput(e: Event) {
    this._searchText = (e.target as HTMLInputElement).value
    this._highlightedIndex = 0
    this._render()
    // Re-focus search after re-render
    const search = this.querySelector<HTMLInputElement>(".jr-dropdown-search")
    search?.focus()
    // Restore cursor position
    search?.setSelectionRange(search.value.length, search.value.length)
  }

  private _onKeyDown(e: KeyboardEvent) {
    const filtered = this._filteredOptions
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault()
        this._highlightedIndex = Math.min(this._highlightedIndex + 1, filtered.length - 1)
        this._render()
        this._refocusSearch()
        break
      case "ArrowUp":
        e.preventDefault()
        this._highlightedIndex = Math.max(this._highlightedIndex - 1, 0)
        this._render()
        this._refocusSearch()
        break
      case "Enter":
        e.preventDefault()
        if (filtered[this._highlightedIndex]) {
          this._selectOption(filtered[this._highlightedIndex])
        }
        break
      case "Escape":
        e.preventDefault()
        this._open = false
        this._searchText = ""
        this._highlightedIndex = 0
        this._render()
        break
    }
  }

  private _refocusSearch() {
    const search = this.querySelector<HTMLInputElement>(".jr-dropdown-search")
    search?.focus()
  }

  private _render() {
    const placeholder = this.getAttribute("placeholder") ?? "Select..."
    const labelText = this.getAttribute("label")
    const filtered = this._filteredOptions
    const selectedLabel = this._selectedLabel

    let html = ""

    if (labelText) {
      html += `<label class="jr-dropdown-label">${this._escapeHtml(labelText)}</label>`
    }

    html += `<button class="jr-dropdown-trigger" type="button">`
    html += selectedLabel
      ? `<span class="jr-dropdown-trigger-text">${this._escapeHtml(selectedLabel)}</span>`
      : `<span class="jr-dropdown-trigger-placeholder">${this._escapeHtml(placeholder)}</span>`
    html += `<span class="jr-dropdown-arrow">${this._open ? "\u25B2" : "\u25BC"}</span>`
    html += `</button>`

    if (this._open) {
      html += `<div class="jr-dropdown-panel">`
      html += `<input class="jr-dropdown-search" type="text" placeholder="Search..." value="${this._escapeAttr(this._searchText)}" />`
      html += `<div class="jr-dropdown-options">`
      if (filtered.length === 0) {
        html += `<div class="jr-dropdown-empty">No options found</div>`
      } else {
        filtered.forEach((opt, i) => {
          const classes = ["jr-dropdown-option"]
          if (i === this._highlightedIndex) classes.push("jr-dropdown-option-highlighted")
          if (opt.value === this._value) classes.push("jr-dropdown-option-selected")
          html += `<div class="${classes.join(" ")}" data-index="${i}">${this._escapeHtml(opt.label)}</div>`
        })
      }
      html += `</div></div>`
    }

    this.innerHTML = html
    this.className = "jr-dropdown" + (this._open ? " jr-dropdown-open" : "")

    // Bind events
    const trigger = this.querySelector<HTMLButtonElement>(".jr-dropdown-trigger")
    trigger?.addEventListener("click", (e) => this._onTriggerClick(e))

    if (this._open) {
      const search = this.querySelector<HTMLInputElement>(".jr-dropdown-search")
      search?.addEventListener("input", (e) => this._onSearchInput(e))
      search?.addEventListener("keydown", (e) => this._onKeyDown(e))

      this.querySelectorAll<HTMLDivElement>(".jr-dropdown-option").forEach((el) => {
        const idx = parseInt(el.dataset.index ?? "0", 10)
        el.addEventListener("click", (e) => {
          e.stopPropagation()
          if (filtered[idx]) this._selectOption(filtered[idx])
        })
        el.addEventListener("mouseenter", () => {
          // Update highlight without full re-render to avoid destroying click targets
          this.querySelector(".jr-dropdown-option-highlighted")?.classList.remove("jr-dropdown-option-highlighted")
          el.classList.add("jr-dropdown-option-highlighted")
          this._highlightedIndex = idx
        })
      })
    }
  }

  private _escapeHtml(str: string): string {
    return str.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;")
  }

  private _escapeAttr(str: string): string {
    return str.replace(/&/g, "&amp;").replace(/"/g, "&quot;")
  }
}

if (!customElements.get("jr-dropdown")) {
  customElements.define("jr-dropdown", JrDropdown)
}
