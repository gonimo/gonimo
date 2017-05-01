{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.App.UI.I18N where

import           Gonimo.I18N

data Message = Loading_stay_tight
             | We_are_sorry_Apple_does_not_like_us_yet
             | Gonimo_might_not_work_as_expected
             | Unfortunately_Apple_iOS_devices_cannot_be_supported
             | Also_Apple_restricts_all_other_browsers_on_iOS
             | Fortunately_Apple_has_made_some_progress
             | We_will_post_on_our_page_when_iOS_support_is_ready
             | Please_upgrade_your_browser
             | Gonimo_needs_some_cutting_edge_technology
             | If_you_can't_upgrade
             | Unsupported_browser
             | We_either_never_tested_gonimo_with_your_browser
             | Thank_you
             | OK
             deriving (Show, Eq)

instance I18N Message where
  i18n EN_GB Loading_stay_tight
          = "Loading, stay tight..."
  i18n EN_GB We_are_sorry_Apple_does_not_like_us_yet
          = "We are sorry, Apple does not like us yet!"
  i18n EN_GB Gonimo_might_not_work_as_expected
          = "Gonimo might not work as expected"
  i18n EN_GB Unfortunately_Apple_iOS_devices_cannot_be_supported
          = "Unfortunately, Apple iOS devices cannot be supported right now, because Safari does not implement the necessary technology."
  i18n EN_GB Also_Apple_restricts_all_other_browsers_on_iOS
          = "Also Apple restricts all other browsers on iOS to the same technology as Safari, so on iOS not even Chrome will work!"
  i18n EN_GB Fortunately_Apple_has_made_some_progress
          = "Fortunately Apple has made some progress lately and it seems that Safari will support Gonimo soon! You can follow us on "
  i18n EN_GB We_will_post_on_our_page_when_iOS_support_is_ready
          = ": We will post on our page, when iOS support is ready!"
  i18n EN_GB Please_upgrade_your_browser
          = "Please upgrade your browser!"
  i18n EN_GB Gonimo_needs_some_cutting_edge_technology
          = "Gonimo needs some cutting edge technology in order to work correctly and browsers get better all the time, so we recommend to download the latest version of your browsers, for the best gonimo experience."
  i18n EN_GB If_you_can't_upgrade
          = " If you can't upgrade, please double check that gonimo is working reliably for you. Especially check that you will hear an alarm whenever the connection is lost (you can test this by, for example, reloading the page at your baby station), especially check that you also hear an alarm when the screen is switched off at the parent station."
  i18n EN_GB Unsupported_browser
          = "Unsupported browser!"
  i18n EN_GB We_either_never_tested_gonimo_with_your_browser
          = "We either never tested gonimo with your browser or it is not supported right now. Please be aware that gonimo might not work properly! If you want the best experience we currently have to recommend Chrome and any non iOS platform. If you'd like to have your browser supported or want to know about the current status, please file an issue on "
  i18n EN_GB Thank_you
          = "Thank you"
  i18n EN_GB OK
          = "OK"
  i18n DE_DE msg           = i18n EN_GB msg -- fallback solution
