import Do.Parser
import Do.Types (CommandSet)

import Control.Monad (void)
import System.Directory (withCurrentDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.IO as Text

main :: IO ()
main = defaultMain tests

getCommandSet :: IO CommandSet
getCommandSet =
  withCurrentDirectory "test/data" $
    parseScripts =<< Text.readFile "do.nix"

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "parse file" $
      void getCommandSet
  ]
