module Do.CLI
  ( parseInput
  ) where

import Do.Types

import Data.Monoid ((<>))

import qualified Data.Text as Text
import qualified Options.Applicative as Options

--------------------------------------------------------------------------------

-- Given the subcommand name and its variables, generate a flag parser for
-- the input.
makeFlagParser :: Text -> [Text] -> Options.Parser Input
makeFlagParser name vars =
  let
    parser var =
      fmap (Val var) $ Options.strOption $ Options.long $ Text.unpack var
  in
    Input [name] <$> traverse parser vars

-- Given the subcommand name and its contents, generate the CLI subcommand using
-- the parser above.
makeCommand ::
  Text -> Command -> Options.Mod Options.CommandFields Input
makeCommand name (Command _ vars help) =
  Options.command (Text.unpack name) $
  Options.info (Options.helper <*> makeFlagParser name vars) $
    Options.fullDesc <>
    Options.header (Text.unpack name) <>
    Options.progDesc (Text.unpack help)

addCommand ::
  Text ->
  Options.Mod Options.CommandFields Input ->
  Options.Mod Options.CommandFields Input
addCommand name opts =
  let
    parser =
      addLayer name <$>
      Options.subparser opts
  in
    Options.command (Text.unpack name) $
    Options.info (Options.helper <*> parser) $
      Options.fullDesc <>
      Options.header (Text.unpack name)

makeCommandSet :: CommandSet -> Options.Mod Options.CommandFields Input
makeCommandSet = foldCommandSet makeCommand addCommand

-- | Given the set of commands, generate the CLI interface that parses and
-- returns the input in case of success.
parseInput :: CommandSet -> IO Input
parseInput scripts =
  let
    parser =
      Options.subparser $
       makeCommandSet scripts
  in
    Options.customExecParser (Options.prefs Options.showHelpOnError) $
    Options.info (Options.helper <*> parser) $
      Options.fullDesc <>
      Options.header "do: command line interface for folders" <>
      Options.progDesc "Execute one of the commands defined on the do.nix file."
