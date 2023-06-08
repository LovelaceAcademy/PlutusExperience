module Minting.Page.Minting
  ( mintPage
  )
  where

import Contract.Prelude
  ( class Newtype
  , ($)
  , (<$>)
  , (<<<)
  , (<>)
  , Maybe (Nothing, Just)
  , pure
  , unit
  , wrap
  , unwrap
  , const
  , bind
  , show
  , foldMap
  )
import Control.Monad.Cont as CMC
import Contract.Monad as CM
import Contract.Prim.ByteArray as CPBA
import Data.String.Read (read)
import Data.BigInt as DBI
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HHE
import Formless as F
import UI.Element as UIE
import Minting.Types as MT
import Minting.Contract as MC
import Validation as V

newtype DonationForm :: (Row Type -> Type) -> (Type -> Type -> Type -> Type) -> Type
newtype DonationForm r f = DonationForm
  ( r
      ( beneficiary :: MT.BeneficiaryField f
      , deadline :: MT.DeadlineField f
      , value :: MT.ValueField f
      )
  )

derive instance Newtype (DonationForm r f) _

data DonationFormMessage =
    PickBeneficiary
  | Now
  | Donate MT.Donate
type DonationFormInput = 
  { beneficiary :: Maybe MT.Beneficiary
  , deadline :: Maybe MT.Deadline
  }
data DonationFormAction =
    HandlePick
  | HandleNow
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
              { label: MT.beneficiary_label 
              , help: UIE.resultToHelp MT.beneficiary_help $
                  (F.getResult MT._beneficiary form :: F.FormFieldResult V.FieldError _)
              }
              [ UIE.class_ "input-group-vertical"
              , HHP.value $ F.getInput MT._beneficiary form
              , HHE.onValueInput (F.setValidate MT._beneficiary)
              , HHP.placeholder MT.beneficiary_placeholder 
              ]
              [ HH.button
                  [ UIE.class_ "btn"
                  , HHE.onClick (const $ F.injAction HandlePick)
                  ]
                  [ HH.text "Pick" ]
              ]
          , UIE.input
              { label: "Deadline"
              , help: UIE.resultToHelp "Unix timestamp (seconds since January 1st, 1970 at UTC)" $
                    ((F.getResult MT._deadline form) :: F.FormFieldResult V.FieldError _)
              }
              [ HHP.value $ F.getInput MT._deadline form
              , HHE.onValueInput (F.setValidate MT._deadline)
              ]
              [ HH.button
                  [ UIE.class_ "btn"
                  , HHE.onClick (const $ F.injAction HandleNow)
                  ]
                  [ HH.text "Now" ]
              ]
          , UIE.input
              { label: "Value"
              , help: UIE.resultToHelp "Lovelaces" $
                    ((F.getResult MT._value form) :: F.FormFieldResult V.FieldError _)
              }
              [ HHP.value $ F.getInput MT._value form
              , HHE.onValueInput (F.setValidate MT._value)
              ]
              []
          , UIE.submit
              [ HHE.onClick (const $ F.submit)
              ]
          ]
        handleEvent = case _ of
                           F.Submitted form -> H.raise $ Donate (F.unwrapOutputFields form)
                           _ -> pure unit
        handleAction = case _ of
          HandleInput i -> case i of
                                { beneficiary: Just value } ->
                                  eval $ F.setValidate MT._beneficiary $ show value
                                { deadline: Just value } -> 
                                  eval $ F.setValidate MT._deadline $ DBI.toString $ unwrap value
                                _ -> pure unit
          HandlePick -> H.raise PickBeneficiary
          HandleNow -> H.raise Now
          where
                eval act = F.handleAction handleAction handleEvent act
        formInput =
          { validators: DonationForm
              { beneficiary: V.hoistFnM_ MT.beneficiary_error read
              , value: V.strIsBigInt
              , deadline: wrap <$> V.strIsBigInt
              }
          , initialInputs: Nothing
          }

data Action = HandleDonation DonationFormMessage

mintPage :: forall q i o. CM.ContractParams -> H.Component q i o Aff
mintPage cfg = H.mkComponent
  { initialState: const
      { beneficiary: Nothing
      , deadline: Nothing
      , txId: Nothing
      }
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  , render: render
  }
  where
        runContract :: forall a. CM.Contract a -> Aff a
        runContract = CM.runContract cfg
        handleAction (HandleDonation msg) = case msg of
          PickBeneficiary -> do
             beneficiary <- CMC.lift $ runContract MC.ownBeneficiary
             H.modify_ \s -> s { beneficiary = Just beneficiary }
          Now -> do
             deadline <- CMC.lift $ runContract MC.nowDeadline
             H.modify_ \s -> s { deadline = Just deadline }
          Donate d -> do
             { txId } <-  CMC.lift $ runContract $ MC.donate d
             H.modify_ \s -> s { txId = Just txId }
        render { beneficiary, deadline, txId } = HH.div_ $
             foldMap
                (\txId' ->
                  [ HH.div [ UIE.class_ "max-w-sm mx-auto break-all" ]
                      [ UIE.alert [] [ HH.text $ "Transaction " <> txId' <> " submitted." ] ]
                  ]
                )
                (CPBA.byteArrayToHex <<< unwrap <$> txId)
          <> [ HH.slot F._formless unit donateForm
                 (Just { beneficiary, deadline })
                 HandleDonation
             ] 
