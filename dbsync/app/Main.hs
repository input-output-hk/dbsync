{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Prelude

import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn ("cardano-db-sync: starting..." :: Text)
