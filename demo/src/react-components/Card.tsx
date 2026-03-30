import type { ReactNode } from "react"

export function Card({
  props,
  children,
}: {
  props: { title: string; subtitle?: string }
  children?: ReactNode
}) {
  return (
    <div className="jr-card">
      <h3 className="jr-card-title">{props.title}</h3>
      {props.subtitle && <p className="jr-card-subtitle">{props.subtitle}</p>}
      <div className="jr-card-body">{children}</div>
    </div>
  )
}
