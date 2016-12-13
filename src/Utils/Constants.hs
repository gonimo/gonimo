module Utils.Constants where

type Microseconds = Int

standardTimeout :: Microseconds
standardTimeout = 12*10^(6::Int)

killSessionTimeout :: Microseconds
killSessionTimeout = 35*10^(6::Int)


