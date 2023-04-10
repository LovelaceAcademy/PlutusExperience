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
  , show
  )
import Control.Monad.Cont as CMC
import Contract.Address as CA
import Contract.Monad as CM
import Contract.Credential as CC
import Contract.Log as CL
import Ctl.Internal.Serialization.Hash as CISH
import Data.String.Read (read)
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
      ( beneficiary :: DT.BeneficiaryField f
      , deadline :: f V.FieldError String DT.Deadline
      , value :: f V.FieldError String DT.Value
      )
  )

derive instance Newtype (DonationForm r f) _

data DonationFormMessage = PickBeneficiary
type DonationFormInput = 
  { beneficiary :: Maybe DT.Beneficiary
  }
data DonationFormAction =
    HandlePick
  | HandleInput DonationFormInput

donateForm :: forall q s. F.Component DonationForm q s (Maybe DonationFormInput) DonationFormMessage Aff
donateForm = F.component (const formInput) $ F.defaultSpec
  { handleAction = handleAction
  , handleEvent = handleEvent
  , receive = receive
  , render = render
  }
  where
        receive input = HandleInput <$> input
        render { form } = HH.form
          [ UIE.class_ "max-w-sm mx-auto" ]
          [ UIE.input
              { label: DT.beneficiary_label 
              , help: UIE.resultToHelp DT.beneficiary_help $
                  (F.getResult DT._beneficiary form :: F.FormFieldResult V.FieldError _)
              }
              [ UIE.class_ "input-group-vertical"
              , HHP.value $ F.getInput DT._beneficiary form
              , HHE.onValueInput (F.setValidate DT._beneficiary)
              , HHP.placeholder DT.beneficiary_placeholder 
              ]
              [ HH.button
                  [ UIE.class_ "btn"
                  , HHE.onClick (const $ F.injAction HandlePick)
                  ]
                  [ HH.text "Pick" ]
              ]
          ]
        handleEvent _ = pure unit
        handleAction = case _ of
          HandleInput i -> case i of
                                { beneficiary : Just value } ->
                                  eval $ F.set DT._beneficiary $ show value
                                _ -> pure unit
          HandlePick -> H.raise PickBeneficiary
          where
                eval act = F.handleAction handleAction handleEvent act
        formInput =
          { validators: DonationForm
              { beneficiary: V.hoistFnM_ DT.beneficiary_error read
              , value: V.strIsBigInt
              , deadline: wrap <$> V.strIsBigInt
              }
          , initialInputs: Nothing
          }

data Action = HandleDonation DonationFormMessage

donatePage :: forall q i o. CM.ContractParams -> H.Component q i o Aff
donatePage cfg = H.mkComponent
  { initialState: const { beneficiary: Nothing }
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  , render: render
  }
  where
        runContract :: forall a. CM.Contract a -> Aff a
        runContract = CM.runContract cfg
        handleAction (HandleDonation msg) = case msg of
          PickBeneficiary -> do
             beneficiary <- CMC.lift $ runContract DC.ownBeneficiary
             H.modify_ \s -> s { beneficiary = Just beneficiary }
        render s = HH.div_
          [ HH.slot F._formless unit donateForm (Just s) HandleDonation
          ]
