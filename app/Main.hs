{-# LANGUAGE LambdaCase #-}

import Do.CLI (parseInput)
import Do.Parser (parseScripts)
import Do.Run (runCommand)
import Do.Types

import System.Directory (doesFileExist)
import System.Exit (exitFailure)

import qualified Data.Text.IO as Text

--------------------------------------------------------------------------------

main ::  IO ()
main =
  doesFileExist "do.nix" >>= \case
    False -> do
      putStrLn "File do.nix does not exist."
      exitFailure
    True -> do
      file <- Text.readFile "do.nix"
      scripts <- parseScripts file
      Input names vals <- parseInput scripts
      case getCommandFromNames names scripts of
        Left command ->
          runCommand command vals
        Right _ -> do
          putStrLn "This should not happen."
          exitFailure
