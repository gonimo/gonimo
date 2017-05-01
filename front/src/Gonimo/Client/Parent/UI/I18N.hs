{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Parent.UI.I18N where

import           Gonimo.I18N

data Message = Add_Device
             | Stop_All
             | Connection_Lost
             | Really_stop_parent_station
             | All_open_streams_will_be_disconnected
             | Connection_is_reliable
             | Or_gone
             | Connection_unreliable
             | Might_break_unnoticed_no_alert
             | Connection_probably_unreliable
             | We_are_sorry_we_can_not_guarantee_a_reliable_connection
             | This_is_indicated_by_a_red_border
             | If_you_see_a_red_border
             | What_can_I_do
             | Use_a_different_browser_currently_we_recommend_Chrome
             | Disconnect_Connect_periodically_to_be_sure_everything_is_alright
             | For_Audio_connections_have_some_sound_at_the_baby_side_e_g_open_the_window
             | For_Video_connections_have_some_constant_motion_in_the_picture_for_example_a_clock
             | OK
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB Add_Device
          = " ADD DEVICE"
  i18n EN_GB Stop_All
          = "STOP ALL"
  i18n EN_GB Connection_Lost
          = "Connection Lost!"
  i18n EN_GB Really_stop_parent_station
          = "Really stop parent station?"
  i18n EN_GB All_open_streams_will_be_disconnected
          = "All open streams will be disconnected!"
  i18n EN_GB Connection_is_reliable
          = "Connection is reliable!"
  i18n EN_GB Or_gone
          = "(or gone)"
  i18n EN_GB Connection_unreliable
          = "Connection unreliable!"
  i18n EN_GB Might_break_unnoticed_no_alert
          = "Might break unnoticed (no alert)!"
  i18n EN_GB Connection_probably_unreliable
          = "Connection probably unreliable!"
  i18n EN_GB We_are_sorry_we_can_not_guarantee_a_reliable_connection
          = "We are sorry, we can not guarantee a reliable connection to your child on this browser!"
  i18n EN_GB This_is_indicated_by_a_red_border
          = "This is indicated by a red border around the connected device and the video, if you don't see a red border this was a false alert - sorry about that."
  i18n EN_GB If_you_see_a_red_border
          = "If you see a red border, this means the connection might break unnoticed at any time - there will be no alarm!"
  i18n EN_GB What_can_I_do
          = "What can I do?"
  i18n EN_GB Use_a_different_browser_currently_we_recommend_Chrome
          = "Use a different browser - currently we recommend Chrome."
  i18n EN_GB Disconnect_Connect_periodically_to_be_sure_everything_is_alright
          = "Disconnect/Connect periodically to be sure everything is alright."
  i18n EN_GB For_Audio_connections_have_some_sound_at_the_baby_side_e_g_open_the_window
          = "For Audio connections, have some sound at the baby side, e.g. open the window."
  i18n EN_GB For_Video_connections_have_some_constant_motion_in_the_picture_for_example_a_clock
          = "For Video connections, have some constant motion in the picture, for example a clock."
  i18n EN_GB OK
          = "OK"
  i18n

  i18n DE_DE msg = i18n EN_GB msg
