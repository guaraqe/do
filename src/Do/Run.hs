module Do.Run
  ( runCommand
  ) where

import Do.Types

import Data.Text (unpack)
import System.Process (callCommand)

--------------------------------------------------------------------------------

runCommand :: Command -> [Val] -> IO ()
runCommand script vals =
  case command_script script of
    Script contents ->
      callCommand $ unlines $
         map runValEnv vals ++ [unpack contents]
