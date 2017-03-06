module App.Editor where


import App.AST (class ToHtml, toHtml)
import Pux.Html (Html, p)


view :: forall a x . (ToHtml x) => x -> Html a
view tree = p [] [toHtml tree]
