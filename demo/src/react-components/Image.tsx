export function Image({
  props,
}: {
  props: { src: string; alt: string }
}) {
  return <img className="jr-image" src={props.src} alt={props.alt} />
}
