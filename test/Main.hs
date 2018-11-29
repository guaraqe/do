import Do.Parser
import Do.Types (CommandSet)

import Control.Monad (void)
import System.Directory (setCurrentDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.IO as Text

main :: IO ()
main = defaultMain tests

getCommandSet :: IO CommandSet
getCommandSet = do
  setCurrentDirectory "test/data"
  file <- Text.readFile "do.nix"
  parseScripts file

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "parse file" $
      void getCommandSet
  ]
