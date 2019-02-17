{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import qualified Data.Text                as T
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom.Core
import Control.Monad.IO.Class (liftIO)

import           Common.Api
import           Common.Route
import qualified Gonimo.Client.Host.Impl  as Gonimo
import qualified Gonimo.Client.Main       as Gonimo
import           Obelisk.Generated.Static


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = prerender (text "Loading ...") $ do
      conf <- liftIO Gonimo.makeEmptyHostVars
      Gonimo.app conf
  }


pageHead :: DomBuilder t m => m ()
pageHead = do
    js (static @"resources/js/adapter.js")
    js (static @"js/webrtc-shim.js")
    -- TODO: Replace with obelisk config:
    js (static @"js/env.js")
    js (static @"resources/js/screenfull.js")
    js (static @"resources/js/bowser.min.js")

    --  Prefetch world, so it will be in cache when needed on connection loss!
    prefetch (static @"pix/gonimo-Welt-animation_460x460px.gif")
    --  Prefetch alarm, so it will be in cache when loading the parent screen!
    prefetch (static @"sounds/gonimo_alarm_64kb_long.mp3")

    meta ("charset" =: "utf-8")
    meta ("name" =: "theme-color" <> "content" =: "#E6CAD4")
    meta ("name" =: "msapplication-navbutton-color" <> "content" =: "#E6CAD4")
    meta ("name" =: "apple-mobile-web-app-status-bar-style" <> "content" =: "#E6CAD4")
    meta ("name" =: "msapplication-navbutton-color" <> "content" =: "#E6CAD4")
    meta ("name" =: "apple-mobile-web-app-capable" <> "content" =: "yes")
    meta ("name" =: "apple-mobile-web-app-status-bar-style" <> "content" =: "black")


    ss (static @"resources/css/font-awesome.min.css")
    ss (static @"resources/css/bootstrap.min.css")
    ss (static @"resources/css/bootstrap-theme.min.css")
    ss (static @"css/gonimo.css")
    ss (static @"css/app.css")

    meta ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1, user-scalable=no")

    el "script" $ text $ T.unlines
      [ "/* Fixup for Safari (webkit based browsers) */"
      , "window.AudioContext = window.AudioContext || window.webkitAudioContext;"
      ]

  where
    js url = elAttr "script" ("type" =: "text/javascript" <> "src" =: url <> "charset" =: "utf-8") blank
    ss url = elAttr "link" ("href" =: url <> "rel" =: "stylesheet") blank
    prefetch url = elAttr "link" ("href" =: url <> "rel" =: "prefetch") blank
    meta attrs = elAttr "meta" attrs blank

