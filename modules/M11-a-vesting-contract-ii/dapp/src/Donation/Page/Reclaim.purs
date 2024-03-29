module Donation.Page.Reclaim
  ( reclaimPage
  ) where

import Contract.Prelude
  ( ($)
  , (<$>)
  , (<>)
  , (<<<)
  , class Newtype
  , bind
  , unit
  , const
  , pure
  , show
  , foldMap
  )

import Control.Monad.Cont as CMC
import Contract.Monad as CM
import Contract.Prim.ByteArray as CPBA
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap, unwrap)
import Data.String.Read (read)
import Donation.Contract as DC
import Donation.Types as DT
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HHE
import Halogen.HTML.Properties as HHP
import UI.Element as UIE
import Validation as V
import Type.Proxy (Proxy (Proxy))

newtype ReclaimForm (r :: Row Type -> Type) f = ReclaimForm
  ( r
      ( beneficiary :: DT.BeneficiaryField f
      , donationTxId :: f V.FieldError String DT.TransactionId
      )
  )

derive instance Newtype (ReclaimForm r f) _

data ReclaimFormMessage = PickBeneficiary | Reclaim DT.Reclaim

type ReclaimFormInput =
  { beneficiary :: Maybe DT.Beneficiary
  , donationTxId :: Maybe DT.TransactionId
  }

_donationTxId :: Proxy "donationTxId"
_donationTxId = Proxy

data ReclaimFormAction =
    HandlePick
  | HandleInput ReclaimFormInput

data Action = HandleReclaim ReclaimFormMessage

reclaimForm :: forall q s.  F.Component ReclaimForm q s (Maybe ReclaimFormInput) ReclaimFormMessage Aff
reclaimForm = F.component (const formInput) $ F.defaultSpec
  { handleAction = handleAction
  , handleEvent = handleEvent
  , receive = receive
  , render = render
  }
  where
        receive input = HandleInput <$> input
        render = \{ form } -> HH.form
          [ UIE.class_ "max-w-sm mx-auto" ]
          [ UIE.input
              { label: DT.beneficiary_label 
              , help: UIE.resultToHelp DT.beneficiary_help $
                    ((F.getResult DT._beneficiary form) :: F.FormFieldResult V.FieldError _)
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
          , UIE.input
              { label: "Donation transaction" 
              , help: UIE.resultToHelp "Paste here the donation transaction hash" $
                    ((F.getResult _donationTxId form) :: F.FormFieldResult V.FieldError _)
              }
              [ UIE.class_ "input-group-vertical"
              , HHP.value $ F.getInput _donationTxId form
              , HHE.onValueInput (F.setValidate _donationTxId)
              ]
              []
          , UIE.submit
              [ HHE.onClick (const $ F.submit)
              ]
          ]
        handleEvent  = case _ of
                            F.Submitted form -> H.raise $ Reclaim (F.unwrapOutputFields form)
                            _ -> pure unit
        handleAction = case _ of
                            HandleInput i -> case i of
                              { beneficiary: Just value } ->
                                eval $ F.setValidate DT._beneficiary $ show value
                              _ -> pure unit
                            HandlePick -> H.raise PickBeneficiary
               where
                     eval act = F.handleAction handleAction handleEvent act
        formInput =
          { validators: ReclaimForm
              { beneficiary: V.hoistFnM_ DT.beneficiary_error read
              , donationTxId: wrap <$> V.strIsByteArray
              }
          , initialInputs: Nothing
          }

reclaimPage :: forall q i o. CM.ContractParams -> H.Component q i o Aff
reclaimPage cfg = H.mkComponent
  { initialState: const $ { beneficiary: Nothing
                          , donationTxId: Nothing
                          , txId: Nothing
                          }
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  , render: render
  }
  where
        runContract :: forall a. CM.Contract a -> Aff a
        runContract = CM.runContract cfg
        handleAction (HandleReclaim msg)= case msg of
          PickBeneficiary -> do
             beneficiary <- CMC.lift $ runContract DC.ownBeneficiary 
             H.modify_ \s -> s { beneficiary = Just beneficiary }
          Reclaim d -> do
             { txId } <- CMC.lift $ runContract $ DC.reclaim d
             H.modify_ \s -> s { txId = Just txId }
        render { beneficiary, donationTxId, txId } = HH.div_ $
             foldMap
                (\txId' ->
                  [ HH.div [ UIE.class_ "max-w-sm mx-auto break-all" ]
                      [ UIE.alert [] [ HH.text $ "Transaction " <> txId' <> " submitted." ] ]
                  ]
                )
                (CPBA.byteArrayToHex <<< unwrap <$> txId)
          <> [ HH.slot F._formless unit reclaimForm
                 (Just { beneficiary, donationTxId })
                 HandleReclaim 
             ]
