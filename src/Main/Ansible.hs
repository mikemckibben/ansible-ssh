{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Main.Ansible (execSsh)
where

import           Control.Applicative       (empty)
import           Control.Lens              (makeFieldsNoPrefix, over, preview,
                                            to, (%~), (&), (.~), (^.), (^?),
                                            _Just, _Right)
import           Control.Monad             (forM_, when)
import           Data.Aeson.Lens           (key, _String, _Value)
import           Data.Either               (rights)
import           Data.List                 (intercalate)
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Semigroup
import           Data.Text                 (Text, pack, unpack)
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS (fromText)
import           System.Posix.Process      (executeFile)
import           Text.Parsec               as P
import           Text.Parsec.Char          as P
import           Text.Parsec.Combinator    as P
import qualified Turtle                    as T

type Parser = P.Parsec String ()

-- |ssh command arguments for an ansible inventory host
data SshConfig = SshConfig
  { _sshExecutable :: String
  , _sshHost       :: String
  , _sshRemoteUser :: String
  , _sshArgs       :: [String] }
  deriving (Eq, Show)

makeFieldsNoPrefix ''SshConfig

defaultSshConfig = SshConfig
  { _sshExecutable = "ssh"
  , _sshHost = ""
  , _sshRemoteUser = ""
  , _sshArgs = [] }

-- |run the ansible-inventory program with given arguments
runAnsibleInventory args = T.inproc "ansible-inventory" args empty

-- |run the ansible-config to return the project ansible configuration
runAnsibleConfig = T.inproc "ansible-config" ["view"] empty

loadDefaultSshConfig :: IO SshConfig
loadDefaultSshConfig = T.fold runAnsibleConfig (T.Fold step defaultSshConfig id)
  where step c l = maybe c (modifySshConfig c) (parseLine l)
        parseLine = match configKeyValue . unpack . T.lineToText

-- |parse the ansible ssh configuration for the given host, merging in any
-- global configuration with any per-host configuration overrides.
loadSshConfig :: String -> IO SshConfig
loadSshConfig hostname = do
  config <- loadDefaultSshConfig
  inventory <- T.strict $ runAnsibleInventory ["--host", pack hostname]
  let updateField f b = over f (flip fromMaybe $ b)
      configValue k = preview $ _Just . key k . _String . to unpack
      json = inventory ^? _Value
      host = configValue "ansible_host" json
      user = configValue "ansible_user" json
      sshexe = configValue "ansible_ssh_executable" json
      args0 = matchArgLine <$> configValue "ansible_ssh_common_args" json
      args1 = matchArgLine <$> configValue "ansible_ssh_extra_args" json
      args = args0 <> args1
      config' = config
        & updateField sshHost host
        & updateField sshRemoteUser user
        & updateField sshArgs args
        & updateField sshExecutable sshexe
  when (config' ^. sshHost . to null) $
    T.die "ERROR: Unable to determine IP for host $host in ansible-inventory. Check hostname is a valid inventory host."
  return config'

match :: Parser a -> String -> Maybe a
match p s = P.parse p "" s ^? _Right

matchDefault :: a -> Parser a -> String -> a
matchDefault a p s = fromMaybe a $ match p s

matchArgLine :: String -> [String]
matchArgLine = matchDefault [] configArgLine

configKeyValue :: Parser (String, String)
configKeyValue = do
  P.spaces
  key <- P.many (P.noneOf [' ', '='])
  P.spaces >> P.char '=' >> P.spaces
  value <- P.many P.anyChar
  return (key, value)

configArgLine :: Parser [String]
configArgLine = words `P.sepBy` (P.oneOf " \t")
  where matchedPair begin end =
          P.char begin *> (P.many (P.noneOf [end])) <* P.char end
        quoted = singleQuote <|> doubleQuote
        singleQuote = matchedPair '\'' '\''
        doubleQuote = matchedPair '"' '"'
        wordChar = P.noneOf " \t'\"" <|> escapedSpace
        escapedSpace = P.try (P.char '\\' *> P.char ' ')
        word = quoted <|> P.many1 wordChar
        words = concat <$> P.many1 word

modifySshConfig :: SshConfig -> (String, String) -> SshConfig
modifySshConfig config pair =
  case pair of
    ("remote_user", v) -> config & sshRemoteUser .~ v
    ("ssh_args", v)    -> config & sshArgs .~ matchArgLine v
    _                  -> config

-- | execute an ssh command or open interactive session to an ansible inventory
-- host.
execSsh :: Maybe String -- ^ directory to read ansible inventory
        -> String       -- ^ ansible host
        -> [String]     -- ^ ssh command arguments
        -> IO ()
execSsh ansibleDir ansibleHost extraArgs = do
  -- set current directory to ansible directory or use current working directory
  forM_ ansibleDir $ T.cd . T.fromString
  cfg <- loadSshConfig ansibleHost
  let argUser = cfg ^. sshRemoteUser . to (\u -> if null u then [] else ["-o", "User=" <> u])
      args = cfg ^. sshArgs <> argUser <> [cfg ^. sshHost] <> extraArgs
  putStrLn $ intercalate " " ("ssh" : args)
  executeFile "ssh" True args Nothing
