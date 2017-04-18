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
webSocketPingInterval = 23

webSocketMaxRoundTrip :: NominalDiffTime
webSocketMaxRoundTrip = 7


serverWatchDogTime :: NominalDiffTime
serverWatchDogTime = webSocketPingInterval + webSocketMaxRoundTrip / 2
