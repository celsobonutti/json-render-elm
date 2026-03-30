import { useBoundProp } from "@json-render/react"

export function Input({
  props,
  bindings,
}: {
  props: { label?: string; placeholder?: string; value?: string }
  bindings?: Record<string, string>
}) {
  const [boundValue, setBoundValue] = useBoundProp<string>(
    props.value,
    bindings?.value
  )

  const displayValue = boundValue ?? ""

  return (
    <div className="jr-input-wrapper">
      {props.label && <label className="jr-input-label">{props.label}</label>}
      <input
        className="jr-input"
        type="text"
        placeholder={props.placeholder ?? ""}
        value={displayValue}
        onChange={(e) => setBoundValue(e.target.value)}
      />
    </div>
  )
}
