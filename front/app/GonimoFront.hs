{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}

import Reflex
import Reflex.Dom
import qualified Data.Text as T
import Control.Lens
import Safe
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Gonimo.SocketAPI.Types as API
import qualified GHCJS.DOM.JSFFI.Generated.Storage as Storage
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified GHCJS.DOM as DOM
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage

data ArithOp = Plus | Minus | Mult | Division deriving (Ord, Eq)

instance Show ArithOp where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Division = "/"

doOp :: ArithOp -> Double -> Double -> Double
doOp Plus = (+)
doOp Minus = (-)
doOp Mult = (*)
doOp Division = (/)

main :: IO ()
main = do
  window <- DOM.currentWindowUnchecked
  storage <- Window.getLocalStorageUnsafe window
  mAuth <- GStorage.getItem storage GStorage.keyAuthData
  pure ()

--   Storage.getItem 
-- mainWidget
--   $ el "div" $ do
--     el "ul" $ do
--       el "li" $ text "one"
--       el "li" $ text "two"
--       el "li" $ text "three"
--     a1 <- numberInput
--     d <- opInput
--     a2 <- numberInput
--     text "="
--     let values = (,) <$> a1 <*> a2
--     let r = zipDynWith (\op (a, b) -> doOp <$> pure op <*> a <*> b) (d^.dropdown_value) values
--     let strR = T.pack . maybe "" show <$> r
--     dynText strR

-- numberInput :: forall t m. MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = mdo
--         let
--           readDouble :: T.Text -> Maybe Double
--           readDouble = readMay . T.unpack


--         let
--           validState = "style" =: "border-color : blue"
--           errorState = "style" =: "border-color : red"

--         input <- textInput $ def & textInputConfig_inputType .~ "text"
--                                  & textInputConfig_initialValue .~ "0"
--                                  & textInputConfig_attributes .~ attrs

--         let result = readDouble <$> input^.textInput_value
--         let attrs = maybe errorState (const validState) <$> result
--         pure result

-- opInput :: forall t m. MonadWidget t m => m (Dropdown t ArithOp)
-- opInput = do
--    let ops = [ Plus, Minus, Mult, Division]
--    let strOps = map (T.pack . show) ops
--    let opMap = Map.fromList $ zip ops strOps
--    dropdown Plus (constDyn opMap) def
-- 1c50bdd928..da70d3da0f
