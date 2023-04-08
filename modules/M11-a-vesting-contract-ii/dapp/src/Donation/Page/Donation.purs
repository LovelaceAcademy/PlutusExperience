module Donation.Page.Donation
  ( donatePage
  )
  where

import Contract.Prelude
  ( class Newtype
  , ($)
  , (<$>)
  , Unit
  , Maybe (Nothing)
  , Either (Left)
  , unit
  , wrap
  , const
  )
import Contract.Monad as CM
import Contract.Credential as CC
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HHE
import Formless as F
import Donation.Types as DT
import Validation as V
import UI.Element as UIE
import Type.Proxy (Proxy (Proxy))

newtype DonationForm :: (Row Type -> Type) -> (Type -> Type -> Type -> Type) -> Type
newtype DonationForm r f = DonationForm
  ( r
      ( beneficiary :: f V.FieldError String DT.Address
      , value :: f V.FieldError String DT.Value
      , deadline :: f V.FieldError String DT.Deadline
      )
  )

derive instance Newtype (DonationForm r f) _

donateForm :: forall q s. F.Component DonationForm q s Unit DT.Donate Aff
donateForm = F.component formInput $ F.defaultSpec { render = render }
  where
        _beneficiary = Proxy :: Proxy "beneficiary" 
        render { form } = HH.form
          [ UIE.class_ "max-w-sm mx-auto" ]
          [ UIE.input
              { field:
                  { label: "Address verification key hash"
                  , help: UIE.resultToHelp "Paste the beneficiary verification hash" $
                      F.getResult _beneficiary form
                  }
              , placeholder: "addr_vkh..."
              }
              [ UIE.class_ "input-group-vertical"
              , HHP.value $ F.getInput _beneficiary form
              , HHE.onValueInput (F.setValidate _beneficiary)
              ]
              [ HH.button
                  [ UIE.class_ "btn" ]
                  [ HH.text "Pick" ]
              ]
          ]
        formInput _ =
          { validators: DonationForm
              { beneficiary:
                      (\addressCredential -> wrap { addressCredential, addressStakingCredential: Nothing })
                  <$> CC.PubKeyCredential
                  <$> wrap 
                  <$> V.ed25519
              , value: V.strIsBigInt
              , deadline: wrap <$> V.strIsBigInt
              }
          , initialInputs: Nothing
          }

data Action = HandleDonation DT.Donate

donatePage :: forall q i o. CM.ContractParams -> H.Component q i o Aff
donatePage cfg = H.mkComponent
  { initialState: const unit
  , eval: H.mkEval $ H.defaultEval
  , render: const render
  }
  where
        render = HH.div_ [
          HH.slot F._formless unit donateForm unit HandleDonation
        ]
