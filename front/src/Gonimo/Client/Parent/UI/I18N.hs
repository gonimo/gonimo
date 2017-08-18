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
             | OK
             | Loading_your_stream
             | Connectivity_issues
             deriving (Eq, Show)

instance I18N Message where
  i18n EN_GB Add_Device
          = " ADD DEVICE"
  i18n DE_DE Add_Device
          = " GERÄT HINZUFÜGEN"
  i18n EN_GB Stop_All
          = "STOP ALL"
  i18n DE_DE Stop_All
          = "STOPPE ALLES"
  i18n EN_GB Connection_Lost
          = "Connection Lost!"
  i18n DE_DE Connection_Lost
          = "Verbindung verloren!"
  i18n EN_GB Really_stop_parent_station
          = "Really stop parent station?"
  i18n DE_DE Really_stop_parent_station
          = "Willst du wirklich die Elternstation beenden?"
  i18n EN_GB All_open_streams_will_be_disconnected
          = "All open streams will be disconnected!"
  i18n DE_DE All_open_streams_will_be_disconnected
          = "Alle offenen Streams werden gestoppt!"
  i18n EN_GB Connection_is_reliable
          = "Connection is reliable!"
  i18n DE_DE Connection_is_reliable
          = "Verbindung ist zuverlässig!"
  i18n EN_GB Or_gone
          = "(or gone)"
  i18n DE_DE Or_gone
          = "(oder schon beendet)"
  i18n EN_GB Connection_unreliable
          = "Connection unreliable!"
  i18n DE_DE Connection_unreliable
          = "Verbindung ist unzuverlässig!"
  i18n EN_GB Might_break_unnoticed_no_alert
          = "Might break unnoticed (no alert)!"
  i18n DE_DE Might_break_unnoticed_no_alert
          = "Sie kann unbemerkt und ohne Alarm abreissen!"
  i18n EN_GB OK
          = "OK"
  i18n DE_DE OK
          = "OK"
  i18n EN_GB Loading_your_stream
          = "Loading your stream ..."
  i18n DE_DE Loading_your_stream
          = "Lade den Stream ..."
  i18n EN_GB Connectivity_issues
          = "Connectivity issues? Please press back and try again."
  i18n DE_DE Connectivity_issues
          = "Verbindungsprobleme? Bitte drücke den Pfeil zurück und versuche es nochmal."
