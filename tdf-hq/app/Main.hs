{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Proxy (Proxy(..))
import System.Environment (getArgs)
import System.Exit (die)
import TDF.API (describeApiType)
import TDF.Server (CombinedAPI)

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stderr, stdout)

import TDF.App.Boot (runBootServer)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  arguments <- getArgs
  case arguments of
    ["--describe-api"] -> BL.putStrLn $ encode $ object
      [ "schemaVersion" .= (1 :: Int)
      , "api" .= describeApiType (Proxy :: Proxy CombinedAPI)
      ]
    [] -> runBootServer
    _ -> die "Usage: tdf-hq-exe [--describe-api]"
