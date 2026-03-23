declare module "*.css" {}

declare module "*.elm" {
  const Elm: Record<string, { init: (options: { node: Element | null }) => any }>;
  export { Elm };
}

declare module "elm-debug-transformer" {
  export function register(options: { simple_mode: boolean }): void;
}
