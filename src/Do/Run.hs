module Do.Run
  ( runCommand
  ) where

import Do.Types

import Data.Text (unpack)
import System.Process (callProcess, callCommand)

--------------------------------------------------------------------------------

runCommand :: Command -> [Val] -> IO ()
runCommand script vals =
  case command_script script of
    ScriptPath file ->
      callProcess file $ fmap runValStr vals
    ScriptText contents ->
      callCommand $ unlines $
         map runValEnv vals ++ [unpack contents]
