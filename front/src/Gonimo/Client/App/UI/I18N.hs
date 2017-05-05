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
  i18n DE_DE Loading_stay_tight
          = "Lade, einen Moment bitte ..."
  i18n EN_GB We_are_sorry_Apple_does_not_like_us_yet
          = "We are sorry, Apple does not like us yet!"
  i18n DE_DE We_are_sorry_Apple_does_not_like_us_yet
          = "Sorry, aber Apple mag uns noch nicht!"
  i18n EN_GB Gonimo_might_not_work_as_expected
          = "Gonimo might not work as expected"
  i18n DE_DE Gonimo_might_not_work_as_expected
          = "Gonimo könnte nicht wie erwartet funktionieren"
  i18n EN_GB Unfortunately_Apple_iOS_devices_cannot_be_supported
          = "Unfortunately, Apple iOS devices cannot be supported right now, because Safari does not implement the necessary technology."
  i18n DE_DE Unfortunately_Apple_iOS_devices_cannot_be_supported
          = "Bedauerlicherweise, können Apple iOS Geräte von uns noch nicht unterstützt werden, weil Safari die benötigte Technologie nicht unterstützt."
  i18n EN_GB Also_Apple_restricts_all_other_browsers_on_iOS
          = "Also Apple restricts all other browsers on iOS to the same technology as Safari, so on iOS not even Chrome will work!"
  i18n DE_DE Also_Apple_restricts_all_other_browsers_on_iOS
          = "Apple verlangt leider, dass auch alle anderen Browser die Technologie verwenden, die Safari benutzt, deshalb funktioniert auf iOS Geräten derzeit auch Chrome nicht."
  i18n EN_GB Fortunately_Apple_has_made_some_progress
          = "Fortunately Apple has made some progress lately and it seems that Safari will support Gonimo soon! You can follow us on "
  i18n DE_DE Fortunately_Apple_has_made_some_progress
          = "Glücklicherweise macht Apple Fortschritte und es sieht so aus als würde Safari Gonimo bald unterstützen. Du kannst uns auf "
  i18n EN_GB We_will_post_on_our_page_when_iOS_support_is_ready
          = ": We will post on our page, when iOS support is ready!"
  i18n DE_DE We_will_post_on_our_page_when_iOS_support_is_ready
          = " folgen: Wir werden auf unserer Seite posten, sobald es dazu Neuigkeiten gibt!"
  i18n EN_GB Please_upgrade_your_browser
          = "Please, upgrade your browser!"
  i18n DE_DE Please_upgrade_your_browser
          = "Wir würden Sie höflichst ersuchen eine Aktualisierung Ihres Browsers in Erwägung zu ziehen!"
  i18n EN_GB Gonimo_needs_some_cutting_edge_technology
          = "Gonimo needs some cutting edge technology in order to work correctly and browsers get better all the time, so we recommend to download the latest version of your browsers, for the best gonimo experience."
  i18n DE_DE Gonimo_needs_some_cutting_edge_technology
          = "Gonimo wird ermöglicht durch eine noch recht junge Technologie, die in den Browsern laufend verbessert wird. Deshalb empfehlen wir den Browser aktuell zu halten um das Beste aus Gonimo herauszuholen."
  i18n EN_GB If_you_can't_upgrade
          = " If you can't upgrade, please double check that gonimo is working reliably for you. Especially check that you will hear an alarm whenever the connection is lost (you can test this by, for example, reloading the page at your baby station), especially check that you also hear an alarm when the screen is switched off at the parent station."
  i18n DE_DE If_you_can't_upgrade
          = "Sollte ein Update nicht möglich sein, bitte teste ob Gonimo zuverlässig für dich arbeitet. Besonders solltest du überprüfen ob ein Alarm bei Verbindungsabriss ertönt, selbst wenn du das Display auf der Elternstation abschaltetst. Du kannst einen Verbindungsabriss simulieren indem du auf der Babystation zum Beispiel die Seite neu lädst."
  i18n EN_GB Unsupported_browser
          = "Unsupported browser!"
  i18n DE_DE Unsupported_browser
          = "Nicht unterstützter Browser!"
  i18n EN_GB We_either_never_tested_gonimo_with_your_browser
          = "Sorry, we either never tested Gonimo with your browser or it is not supported right now. Please be aware that gonimo might not work properly! If you want the best experience we currently have to recommend Chrome and any non iOS platform. If you'd like to have your browser supported or want to know about the current status, please file an issue on "
  i18n DE_DE We_either_never_tested_gonimo_with_your_browser
          = "Sorry, wir haben Gonimo auf deinem Browser entweder einfach noch nie getestet oder er kann nicht unterstützt werden. Wir weisen darauf hin, dass Gonimo möglicherweise nicht wie erwartet funktioniert! Momentan müssen wir leider Chrome empfehlen auf irendeinem Gerät welches nicht mit Apple iOS läuft. Wenn du deinen Browser gerne unterstützt hättest oder über den aktuellen Status gern Bescheid wüsstest, bitte erstelle ein Ticket auf "
  i18n EN_GB Thank_you
          = "Thank you"
  i18n DE_DE Thank_you
          = "Danke!"
  i18n EN_GB OK
          = "OK"
  i18n DE_DE OK
          = "OK"
