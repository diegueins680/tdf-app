{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stderr, stdout)

import TDF.App.Boot (runBootServer)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  runBootServer
