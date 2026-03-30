export function Button({
  props,
  emit,
}: {
  props: { label: string; variant?: string }
  emit: (event: string) => void
}) {
  const variantClass =
    props.variant === "secondary"
      ? "jr-button-secondary"
      : props.variant === "danger"
        ? "jr-button-danger"
        : "jr-button-primary"

  return (
    <button className={`jr-button ${variantClass}`} onClick={() => emit("press")}>
      {props.label}
    </button>
  )
}
