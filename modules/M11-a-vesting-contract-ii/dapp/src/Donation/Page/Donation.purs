module Donation.Page.Donation
  ( donatePage
  )
  where

import Contract.Prelude
  ( class Newtype
  , ($)
  , (<$>)
  , (<<<)
  , Unit
  , Maybe (Nothing, Just)
  , Either (Left)
  , pure
  , unit
  , wrap
  , unwrap
  , const
  , void
  , liftEffect
  , bind
  , fromMaybe
  )
import Control.Monad.Cont as CMC
import Contract.Address as CA
import Contract.Monad as CM
import Contract.Credential as CC
import Contract.Log as CL
import Ctl.Internal.Serialization.Hash as CISH
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HHE
import Formless as F
import UI.Element as UIE
import Type.Proxy (Proxy (Proxy))
import Donation.Types as DT
import Donation.Contract as DC
import Validation as V

newtype DonationForm :: (Row Type -> Type) -> (Type -> Type -> Type -> Type) -> Type
newtype DonationForm r f = DonationForm
  ( r
      ( beneficiary :: f V.FieldError CA.Bech32String DT.Beneficiary
      , value :: f V.FieldError String DT.Value
      , deadline :: f V.FieldError String DT.Deadline
      )
  )

derive instance Newtype (DonationForm r f) _

data DonationFormAction = HandlePick | HandleInput DonationFormInput
data DonationFormMessage = PickBeneficiary
type DonationFormInput = Maybe DT.Beneficiary

donateForm :: forall q s. F.Component DonationForm q s DonationFormInput DonationFormMessage Aff
donateForm = F.component formInput $ F.defaultSpec
  { handleAction = handleAction
  , handleEvent = handleEvent
  , receive = Just <<< HandleInput
  , render = render
  }
  where
        _beneficiary = Proxy :: Proxy "beneficiary" 
        render { form } = HH.form
          [ UIE.class_ "max-w-sm mx-auto" ]
          [ UIE.input
              { field:
                  { label: "Address verification key hash"
                  , help: UIE.resultToHelp "Paste the beneficiary verification hash" $
                    (F.getResult _beneficiary form :: F.FormFieldResult V.FieldError _)
                  }
              , placeholder: "addr_vkh..."
              }
              [ UIE.class_ "input-group-vertical"
              , HHP.value $ F.getInput _beneficiary form
              , HHE.onValueInput (F.setValidate _beneficiary)
              ]
              [ HH.button
                  [ UIE.class_ "btn"
                  , HHE.onClick (const $ F.injAction HandlePick)
                  ]
                  [ HH.text "Pick" ]
              ]
          ]
        toString input = fromMaybe ""
                  (   CISH.ed25519KeyHashToBech32Unsafe "addr_vkh"
                  <$> unwrap 
                  <$> unwrap 
                  <$> input 
                  )
        handleEvent _ = pure unit
        handleAction = case _ of
                            HandleInput input -> eval $ F.setValidate _beneficiary $ toString input
                            HandlePick -> H.raise PickBeneficiary
                            where
                                  eval act = F.handleAction handleAction handleEvent act
        formInput input =
          { validators: DonationForm
              { beneficiary:
                      CA.PaymentPubKeyHash
                  <$> CA.PubKeyHash 
                  <$> V.ed25519
              , value: V.strIsBigInt
              , deadline: wrap <$> V.strIsBigInt
              }
          , initialInputs: Nothing
          }

data Action = HandleDonation DonationFormMessage

donatePage :: forall q i o. CM.ContractParams -> H.Component q i o Aff
donatePage cfg = H.mkComponent
  { initialState: const $ Nothing
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  , render: render
  }
  where
        handleAction = case _ of
                            HandleDonation PickBeneficiary -> do
                               beneficiary <- CMC.lift $ CM.runContract cfg DC.ownBeneficiary
                               H.put $ Just beneficiary 
        render s = HH.div_
          [ HH.slot F._formless unit donateForm s HandleDonation
          ]
