{-# LANGUAGE DeriveFunctor #-}

module Do.Types
  ( Script (..)
  , Command (..)
  , CommandSetF (..)
  , CommandSet
  , mkCommandSet
  , foldCommandSet
  , getCommandFromNames
  , Input (..)
  , addLayer
  , Val (..)
  , runValStr
  , runValEnv
  , Text
  , Map
  ) where

import Data.Fix (Fix (..), cata)
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

-- | The script that is run by the command. It can either:
-- * the script file path, or
-- * the script text.
newtype Script = Script Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | A command, containing:
-- * a script
-- * the variables needed for running the script
-- * the help text of the command
data Command = Command
  { command_script :: Script
  , command_vars :: [Text]
  , command_help :: Text
  } deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | A command set is a structure linking names to either commands or another
-- command set. This is the base functor for the structure.
newtype CommandSetF a = CommandSetF
  { runCommandSet :: Map Text (Either Command a)
  } deriving (Show, Eq, Functor)

-- | A command set is a structure linking names to either commands or another
-- command set.
type CommandSet = Fix CommandSetF

-- Make a @CommandSet@.
mkCommandSet :: Map Text (Either Command CommandSet) -> CommandSet
mkCommandSet = Fix . CommandSetF

-- Consume a @CommandSetF@.
commandSetF ::
  Monoid a => (Text -> Command -> a) -> (Text -> a -> a) -> CommandSetF a -> a
commandSetF f g = Map.foldMapWithKey (\k -> either (f k) (g k)) . runCommandSet

-- Consume a @CommandSet@.
foldCommandSet ::
  Monoid a => (Text -> Command -> a) -> (Text -> a -> a) -> CommandSet -> a
foldCommandSet f g = cata (commandSetF f g)

-- Get either a command of a command set, given a path of names to follow
getCommandFromNames :: [Text] -> CommandSet -> Either Command CommandSet
getCommandFromNames [] cs = Right cs
getCommandFromNames (n:ns) (Fix (CommandSetF m)) =
  getCommandFromNames ns =<< m Map.! n

--------------------------------------------------------------------------------

-- | A value attributed to a variable.
data Val = Val
  { val_var :: Text
  , val_val :: Text
  } deriving (Show, Eq)

-- | Get the string value of a @Val@ for input as a positional variable.
runValStr :: Val -> String
runValStr (Val _ val) = unpack val

-- | Define the @Val@ as an environment variable for input as a environment
-- variable.
runValEnv :: Val -> String
runValEnv (Val var val) = unpack $
  var <> "=" <> "\"" <> val <> "\""

-- | The CLI input, containing the commands and the values of variables.
data Input = Input
  { input_command :: [Text]
  , input_vals :: [Val]
  } deriving (Show, Eq)

addLayer :: Text -> Input -> Input
addLayer n (Input c v) = Input (n:c) v
