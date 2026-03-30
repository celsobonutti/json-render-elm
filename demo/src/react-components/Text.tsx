export function Text({
  props,
}: {
  props: { content: string; size?: string }
}) {
  const sizeClass = props.size ? `jr-text-${props.size}` : "jr-text-md"
  return <span className={`jr-text ${sizeClass}`}>{props.content}</span>
}
