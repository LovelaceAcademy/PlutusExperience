module Form (WidgetNode, render, form) where

import Prelude (($), (>>>))
import Data.Array (foldl)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Semigroup ((<>))

type InputType = String
type Label = String
type Method = String
type Action = String
type Name = String
type Value = String
type HTML = String

data Widget = Input InputType (Maybe Name) Label | Form Method Action
data WidgetNode = WidgetNode Widget (Array WidgetNode)
  
submit :: Widget
submit = Input "submit" Nothing "Submit"

address :: Widget
address = Input "text" (Just "wallet") "Wallet"

formWidget :: Widget
formWidget = Form "post" "https://httpbin.org/post"

data HTMLAttr = Attr Name Value

foldAcc :: forall a. (a -> String) -> String -> a -> String
foldAcc fn s = fn >>> (<>) s

renderAttr :: HTMLAttr -> HTML
renderAttr (Attr name value) = " " <> name <> "=\"" <> value <> "\""

tag :: HTML -> Name -> Array HTMLAttr -> HTML
tag content name attrs = let attrs' = foldl (foldAcc renderAttr) "" attrs in
  "<" <> name <> attrs' <> ">" <> content <> "</" <> name <> ">"

renderElement :: Widget -> String -> String
renderElement el ct = let tag' = tag ct in case el of
  (Form mt act) -> tag' "form" $ [Attr "method" mt, Attr "action" act]
  (Input t n "") -> tag' "input" $ [Attr "type" t] <> maybe [] (\n' -> [Attr "name" n']) n
  (Input t n lb) -> tag ( lb <> renderElement (Input t n "") "" ) "label" []

render :: WidgetNode -> String
render (WidgetNode el children) = let ct = foldl (foldAcc render) "" children in
  renderElement el ct

form :: WidgetNode
form = WidgetNode formWidget [WidgetNode address [], WidgetNode submit []]
