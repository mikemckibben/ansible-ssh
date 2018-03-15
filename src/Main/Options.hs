module Main.Options
  ( Options(..)
  , parseOptions
  )
where

import Data.Semigroup ((<>))
import Control.Applicative ((<**>), many)
import qualified Options.Applicative as O
import qualified Options.Applicative.Text as O

-- |ansible-ssh executable command line options
data Options = Options
  { optAnsibleDirectory :: Maybe String
  , optAnsibleHost :: String
  , optSshArgs :: [String] }
  deriving (Eq, Show)

cmdOptions = O.info (optParser <**> O.helper) (O.progDesc "ssh login to ansible inventory host")
  where optParser = Options
          <$> (O.optional $
               O.option O.str
               (O.metavar "DIR"
                <> O.short 'd'
                <> O.long "directory"
                <> O.help "ansible playbook directory"))
          <*> O.argument O.str
          (O.metavar "HOST"
            <> O.help "ansible inventory hostname")
          <*> (many $
               O.argument O.str
               (O.metavar "ARG"
                <> O.help "ssh arguments"))

-- |parse ansible-ssh command line options
parseOptions :: IO Options
parseOptions = O.execParser cmdOptions
