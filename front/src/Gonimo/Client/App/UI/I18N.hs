{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.App.UI.I18N where

import           Gonimo.I18N

data Message = Loading_stay_tight
             | Gonimo_might_not_work_as_expected
             | Please_upgrade_your_browser
             | Gonimo_needs_some_cutting_edge_technology
             | If_you_can't_upgrade
             | Unsupported_browser
             | We_either_never_tested_gonimo_with_your_browser
             | Thank_you
             | OK
             | Really_stop_gonimo
             | Really_leave_the_app
             -- iOS:
             | IOS_support_is_in_the_works
             | We_are_sorry_right_now_iOS_devices_might_not_work_as_expected
             | Once_we_verified_that_everything_works_as_expected_this_message_will_disappear
             | Or_On_Our
             | To_stay_up_to_date_on_the_progress
             deriving (Show, Eq)

instance I18N Message where
  i18n EN_GB Loading_stay_tight
          = "Loading, stay tight..."
  i18n DE_DE Loading_stay_tight
          = "Lade, einen Moment bitte ..."
  i18n EN_GB Gonimo_might_not_work_as_expected
          = "Gonimo might not work as expected"
  i18n DE_DE Gonimo_might_not_work_as_expected
          = "Gonimo könnte nicht wie erwartet funktionieren"
  i18n EN_GB Please_upgrade_your_browser
          = "Please, upgrade your browser!"
  i18n DE_DE Please_upgrade_your_browser
          = "Wir würden Sie höflichst ersuchen eine Aktualisierung Ihres Browsers in Erwägung zu ziehen!"
  i18n EN_GB Gonimo_needs_some_cutting_edge_technology
          = "Gonimo needs some cutting edge technology in order to work correctly and browsers get better all the time, so we recommend to download the latest version of your browsers, for the best gonimo experience."
  i18n DE_DE Gonimo_needs_some_cutting_edge_technology
          = "Gonimo wird durch eine noch recht junge Technologie ermöglicht, die in den Browsern laufend verbessert wird. Deshalb empfehlen wir den Browser aktuell zu halten um das Beste aus Gonimo herauszuholen."
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
  i18n EN_GB IOS_support_is_in_the_works
          = "iOS support is in the works!"
  i18n DE_DE IOS_support_is_in_the_works
          = "iOS Unterstützung ist in Arbeit!"
  i18n EN_GB We_are_sorry_right_now_iOS_devices_might_not_work_as_expected
          = "We are sorry, right now iOS devices might not work as expected. Unfortunately, there are still be some issues Apple and we are working on."
  i18n DE_DE We_are_sorry_right_now_iOS_devices_might_not_work_as_expected
          = "Wir entschuldigen uns, aber im Moment funktioniert Gonimo auf iOS noch nicht wie erwartet. Es gibt leider noch Probleme an denen Apple und wir arbeiten."
  i18n EN_GB Once_we_verified_that_everything_works_as_expected_this_message_will_disappear
          = "Once we verified, that everything works as expected, this message will disappear. You can follow us on "
  i18n DE_DE Once_we_verified_that_everything_works_as_expected_this_message_will_disappear
          = "Sobald wir verifiziert haben, dass alles funktioniert, wird diese Nachricht verschwinden. Du kannst uns auf "
  i18n EN_GB Or_On_Our
          = " or on our "
  i18n DE_DE Or_On_Our
          = " folgen oder auch unserem "
  i18n EN_GB To_stay_up_to_date_on_the_progress
          = " to stay up to date on the progress."
  i18n DE_DE To_stay_up_to_date_on_the_progress
          = " um über den Fortschritt am Laufenden zu bleiben."

  i18n EN_GB Really_stop_gonimo
          = "Really stop Gonimo?"
  i18n DE_DE Really_stop_gonimo
          = "Gonimo wirklich beenden?"
  i18n EN_GB Really_leave_the_app
          = "You definitely want to leave the app?"
  i18n DE_DE Really_leave_the_app
          = "Willst du die App wirklich verlassen?"
