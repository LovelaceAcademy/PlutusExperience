module UI.Element
  ( class_
  , input
  , resultToHelp
  )
  where

import Contract.Prelude
  ( class Show
  , ($)
  , (<<<)
  , (<>)
  , Either (Left, Right)
  , either
  , maybe
  , pure
  , const
  )
import DOM.HTML.Indexed.InputType as DHIIT
import DOM.HTML.Indexed as DHI
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Validation as V
import Formless as F


type FieldConfig =
  { label :: String
  , help :: Either String String
  }

class_ :: forall r t. String -> HH.IProp ("class" :: String | r) t
class_ = HHP.class_ <<< HH.ClassName


field :: forall w i. FieldConfig -> HH.Node DHI.HTMLdiv w i
field { label, help } props children =
  let
      before =
        [ HH.label
            [ class_ "label" ]
            [ HH.span [ class_ "label-text" ] [ HH.text label ] ]
        ]
      helpError_ str = HH.label
        [ class_ "label" ]
        [ HH.span [ class_ "label-text-alt text-error" ] [ HH.text str ] ]
      help_ str = HH.label
        [ class_ "label" ]
        [ HH.span [ class_ "label-text-alt" ] [ HH.text str ] ]
      after = pure $ case help of
                          Left str -> helpError_ str
                          Right str ->  help_ str
      in
      HH.div
        ([ class_ "form-control" ] <> props)
        (before <> children <> after)

input :: forall w i. FieldConfig -> HH.Node DHI.HTMLinput w i
input cfg props children = field
  cfg
  []
  [ HH.div
      [ class_ "input-group" ]
      ([ HH.input
          ([ HHP.type_ DHIIT.InputText
          ,  either
              (const $ class_ "input input-bordered input-error")
              (const $ class_ "input input-bordered") cfg.help
          ] <> props) 
      ] <> children)
  ]

resultToHelp :: forall t e. Show e => String -> F.FormFieldResult e t -> Either String String
resultToHelp str = case _ of
  F.NotValidated -> Right str
  F.Validating -> Right "validating..."
  other -> maybe (Right str) Left $ V.showError other
