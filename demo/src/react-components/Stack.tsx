import type { ReactNode } from "react"

export function Stack({
  props,
  children,
}: {
  props: { direction?: string; gap?: number }
  children?: ReactNode
}) {
  const dirClass =
    props.direction === "horizontal" ? "jr-stack-horizontal" : "jr-stack-vertical"

  return (
    <div
      className={`jr-stack ${dirClass}`}
      style={props.gap != null ? { gap: `${props.gap}px` } : undefined}
    >
      {children}
    </div>
  )
}
