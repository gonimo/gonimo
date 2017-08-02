{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gonimo.Constants where


import Data.Time.Clock


webSocketPingInterval :: NominalDiffTime
webSocketPingInterval = 30

-- Not used currently:
webSocketMaxRoundTrip :: NominalDiffTime
webSocketMaxRoundTrip = webSocketPingInterval


serverWatchDogTime :: NominalDiffTime
serverWatchDogTime = webSocketPingInterval * 2
