module Do.Parser
  ( parseScripts
  ) where

import Do.Types

import Control.Applicative (Alternative (..))
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.Fix (Fix (..))
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.HashMap.Strict (HashMap)
import Lens.Micro.Extras (preview)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Nix

--------------------------------------------------------------------------------

-- | Parse commands from the @do.nix@ file.
parseScripts :: Text -> IO CommandSet
parseScripts file =
  case Nix.parseNixText file of
    Nix.Failure e ->
      error $ "Could not parse do.nix file:\n" ++ show e
    Nix.Success expr -> do
      nix <- evalNix expr
      case runExcept (getCommandSet nix) of
        Left e -> error $ Text.unpack e
        Right s -> pure s

--------------------------------------------------------------------------------

type NValue = Nix.NValueNF (Nix.Lazy IO)

-- Evaluate the Nix expression to normal form.
evalNix :: Nix.NExpr -> IO NValue
evalNix expr = do
  time <- getCurrentTime
  let options = Nix.defaultOptions time
  Nix.runLazyM options $ do
    val <- Nix.nixEvalExpr Nothing expr
    Nix.normalForm val

--------------------------------------------------------------------------------

-- Get a file path from the Nix value.
getPath :: NValue -> Except Text FilePath
getPath (Fix val) =
  case preview Nix._NVPathF val of
    Nothing -> throwE "Value is not a filepath."
    Just x -> pure x

-- Get a string from the Nix value.
getText :: NValue -> Except Text Text
getText (Fix val) =
  case preview Nix._NVStrF val of
    Nothing -> throwE "Value is not a string."
    Just (x,_) -> pure x

-- Get a list from the Nix value.
getListWith :: (NValue -> Except Text a) -> NValue -> Except Text [a]
getListWith f (Fix val) =
  case preview Nix._NVListF val of
    Nothing -> throwE "Value is not a list."
    Just l ->  traverse f l

-- Get a @Script@ from the Nix value..
getCommandText :: NValue -> Except Text Script
getCommandText val =
  fmap ScriptPath (getPath val) <|> fmap ScriptText (getText val)

--------------------------------------------------------------------------------

-- | Get @Command@ from the Nix value.
getCommand :: NValue -> Except Text Command
getCommand (Fix (Nix.NVSetF expr _)) = do
  script <-
    getCommandText =<<
    getKey "script" expr

  vars <-
    getListWith getText $
    getKeyWithDefault emptyList "vars" expr

  help <-
    getText $
    getKeyWithDefault emptyText "help" expr

  pure $
    Command script vars help

getCommand _ =
  throwE "Nix expression does not evaluate to a set."

toMap :: HashMap Text a -> Map Text a
toMap = Map.fromList . HashMap.toList

-- | Get @CommandSet@ from the Nix value.
getCommandSet :: NValue -> Except Text CommandSet
getCommandSet (Fix (Nix.NVSetF expr _)) =
  let
    getter s =
      fmap Left (getCommand s) <|> fmap Right (getCommandSet s)
  in
    mkCommandSet . toMap <$> traverse getter expr

getCommandSet _ =
  throwE "Nix expression does not evaluate to a set."

--------------------------------------------------------------------------------

getKey :: Text -> HashMap Text a -> Except Text a
getKey k m =
  case HashMap.lookup k m of
    Nothing -> throwE $ "Set does not have key " <> k
    Just a -> pure a

getKeyWithDefault :: a -> Text -> HashMap Text a -> a
getKeyWithDefault d k m =
  fromMaybe d $ HashMap.lookup k m

emptyText :: NValue
emptyText = Fix $ Nix.NVStrF "" mempty

emptyList :: NValue
emptyList = Fix $ Nix.NVListF []
