{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.Money
  ( musicMinorUnitsJSON
  , parseMusicMinorUnits
  , normalizeMusicMinorField
  ) where

import Data.Aeson (Value(..), Object, Result(..), fromJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- PAY-MUSIC-MONEY-WIRE-001. Do not pass an Int64 through binary64. Small
-- numeric representations remain compatible; larger values use exact text.
maxNumericMinor :: Int64
maxNumericMinor = 9007199254740991

musicMinorUnitsJSON :: Int64 -> Value
musicMinorUnitsJSON amount
  | amount >= negate maxNumericMinor && amount <= maxNumericMinor =
      Number (fromIntegral amount)
  | otherwise = String (T.pack (show amount))

parseMusicMinorUnits :: Value -> Parser Int64
parseMusicMinorUnits raw = case raw of
  String text | T.length text <= 20 ->
    case readMaybe (T.unpack text) :: Maybe Integer of
      Just amount
        | amount >= toInteger (minBound :: Int64)
        , amount <= toInteger (maxBound :: Int64)
        , T.pack (show amount) == text -> pure (fromInteger amount)
      _ -> invalid
  Number _ -> case fromJSON raw :: Result Int64 of
    Success amount
      | amount >= negate maxNumericMinor && amount <= maxNumericMinor -> pure amount
    _ -> invalid
  _ -> invalid
  where
    invalid = fail "Money must be a safe JSON integer or a canonical Int64 decimal string"

normalizeMusicMinorField :: Key -> Object -> Parser Object
normalizeMusicMinorField key fields = case KeyMap.lookup key fields of
  Nothing -> pure fields
  Just Null -> pure fields
  Just raw -> do
    amount <- parseMusicMinorUnits raw
    pure (KeyMap.insert key (Number (fromIntegral amount)) fields)
