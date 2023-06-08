module Minting.Page.Minting
  ( mintPage
  )
  where

import Contract.Prelude
  ( ($)
  , (<$>)
  , (<<<)
  , (<>)
  , Maybe (Nothing, Just)
  , unwrap
  , const
  , bind
  , foldMap
  )
import Control.Monad.Cont as CMC
import Contract.Monad as CM
import Contract.Prim.ByteArray as CPBA
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HHE
import UI.Element as UIE
import Minting.Contract as MC

data Action = HandleMint

mintPage :: forall q i o. CM.ContractParams -> H.Component q i o Aff
mintPage cfg = H.mkComponent
  { initialState: const
      { txId: Nothing
      }
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  , render: render
  }
  where
        runContract :: forall a. CM.Contract a -> Aff a
        runContract = CM.runContract cfg
        handleAction HandleMint = do
           { txId } <- CMC.lift $ runContract $ MC.mint
           H.modify_ \s -> s { txId = Just txId }
        render { txId } = HH.div_ $
             foldMap
                (\txId' ->
                  [ HH.div [ UIE.class_ "max-w-sm mx-auto break-all" ]
                      [ UIE.alert [] [ HH.text $ "Transaction " <> txId' <> " submitted." ] ]
                  ]
                )
                (CPBA.byteArrayToHex <<< unwrap <$> txId)
          <> [ UIE.formControl []
                [ HH.button
                  [ UIE.class_ "btn btn-primary btn-lg"
                  , HHE.onClick (const HandleMint)
                  ]
                  [ HH.text "Mint MyOwnNFT" ]
                ] 
             ]
