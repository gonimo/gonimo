{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.Reflex.Dom.WebSocket ( WebSocket
                                          , CloseParams(..)
                                          , closeCode
                                          , closeReason
                                          , configSend
                                          , configClose
                                          , configCloseTimeout
                                          , open
                                          , receive
                                          , Types.error
                                          , close
                                          , webSocket
                                          ) where

import Gonimo.Client.Reflex.Dom.WebSocket.Types as Types
import Gonimo.Client.Reflex.Dom.WebSocket.Internal
