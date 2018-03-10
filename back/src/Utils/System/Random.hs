module Utils.System.Random where

import System.Random
import Control.Monad.State

-- Warning: an empty list leads to a crash.
-- TODO: make more failsafe randomL/randomLs functions.
randomL :: RandomGen g => [a] -> g -> (a, g)
randomL []  _   = error "randomL: empty list"
randomL lst gen =
  let len         = length lst
      (idx, gen') = randomR (0, len-1) gen
  in  (lst !! idx, gen')


randomLs :: RandomGen g => [[a]] -> g -> ([a], g)
randomLs =
  runState . traverse (state . randomL)
