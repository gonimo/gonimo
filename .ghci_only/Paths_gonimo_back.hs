module Paths_gonimo_back where

-- This is a dummy module, useful for interactive testing and development.
-- It shall never get compiled and deployed. Instead, cabal shall generate a
-- module with the same interface due to the data-files cabal directive.
--
-- This file may be not in use (yet).

getDataDir :: IO FilePath
getDataDir = do
  putStrLn "Called dummy getDataDir"
  return "."

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = do
  putStrLn "Called dummy getDataFileName."
  return f
