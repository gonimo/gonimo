module Gonimo.I18N where

import Reflex.Dom.Class
import Reflex.Dom.Widget.Basic
import Data.String

import qualified Data.Text as T

data Locale = DE
            | EN
            deriving (Show, Eq)

instance MonadWidget t m => IsString (m ()) where
  fromString = text . T.pack

class I18N msg where
  {-# MINIMAL i18n #-}
  i18n :: IsString a => Locale -> msg -> a

  unli18Nes :: MonadWidget t m => msg -> m ()
  unlI18Nes = sequence_ . intersperse br . map text . unlines . i18n
    where br = el "br" blank

  translatI18N :: MonadWidget t m => msg -> m ()
  translatI18N = text . i18n

  formattI18Ng :: MonadWidget t m => [m () -> m ()] -> msg -> m ()
  -- | to use for multiline text where each line gets formatted (possibly
  -- differently. For example 
  -- > formattI18Ng [el "h3", ((el "br" pure) <<) , id ]
  -- For a message that has 3 lines
  formattI18Ng fmt msg = sequence_ . zipWith (. text) fmt (unlines $ i18n msg)
