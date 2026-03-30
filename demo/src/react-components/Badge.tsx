export function Badge({
  props,
}: {
  props: { label: string; color?: string }
}) {
  const colorClass = props.color ? `jr-badge-${props.color}` : "jr-badge-gray"
  return <span className={`jr-badge ${colorClass}`}>{props.label}</span>
}
