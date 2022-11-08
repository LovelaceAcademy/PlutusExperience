module Main (main) where

import Prelude (($), Unit)
import Effect (Effect)
import Form (render, form)

foreign import setHTML :: String -> Effect Unit

main :: Effect Unit
main = setHTML $ render form
