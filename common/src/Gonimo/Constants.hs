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

-- Could be used for killing connections manually, currently we rely on TCP to do it at some point.
webSocketMaxRoundTrip :: NominalDiffTime
webSocketMaxRoundTrip = 7


serverWatchDogTime :: NominalDiffTime
serverWatchDogTime = webSocketPingInterval * 2
