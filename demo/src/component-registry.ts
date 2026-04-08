// Import all mini Elm app modules and register them globally
// for <jr-elm-component> to mount.

// @ts-ignore
import { Elm as TextElm } from "./Catalog/MiniApps/Text.elm"
// @ts-ignore
import { Elm as CardElm } from "./Catalog/MiniApps/Card.elm"
// @ts-ignore
import { Elm as ButtonElm } from "./Catalog/MiniApps/Button.elm"
// @ts-ignore
import { Elm as InputElm } from "./Catalog/MiniApps/Input.elm"
// @ts-ignore
import { Elm as StackElm } from "./Catalog/MiniApps/Stack.elm"
// @ts-ignore
import { Elm as BadgeElm } from "./Catalog/MiniApps/Badge.elm"
// @ts-ignore
import { Elm as ImageElm } from "./Catalog/MiniApps/Image.elm"
// @ts-ignore
import { Elm as DropdownElm } from "./Catalog/MiniApps/Dropdown.elm"

;(window as any).__jrElmComponents = {
  Text: TextElm.Catalog.MiniApps.Text,
  Card: CardElm.Catalog.MiniApps.Card,
  Button: ButtonElm.Catalog.MiniApps.Button,
  Input: InputElm.Catalog.MiniApps.Input,
  Stack: StackElm.Catalog.MiniApps.Stack,
  Badge: BadgeElm.Catalog.MiniApps.Badge,
  Image: ImageElm.Catalog.MiniApps.Image,
  Dropdown: DropdownElm.Catalog.MiniApps.Dropdown,
}
