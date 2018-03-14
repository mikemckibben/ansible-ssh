{-# LANGUAGE RecordWildCards #-}

module Main where

import Main.Options
import Main.Ansible

main :: IO ()
main = do
  Options{..} <- parseOptions
  execSsh optAnsibleDirectory optAnsibleHost optSshArgs
