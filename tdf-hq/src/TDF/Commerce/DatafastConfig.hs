{-# LANGUAGE OverloadedStrings #-}

-- | Merchant metadata required by Dataweb, shared by every checkout domain.
-- API credentials and this metadata do not establish merchant certification.
module TDF.Commerce.DatafastConfig
  ( loadDatafastMerchantParameters
  , validateDatafastMerchantParameters
  , encodeDatafastForm
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, isControl)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import Network.HTTP.Types.URI (renderSimpleQuery)

-- HTTP form encoding works on UTF-8 bytes, never Unicode code points.
encodeDatafastForm :: [(ByteString, ByteString)] -> ByteString
encodeDatafastForm = renderSimpleQuery False

loadDatafastMerchantParameters :: IO (Either Text [(ByteString, ByteString)])
loadDatafastMerchantParameters = do
  values <- mapM readVariable
    [ "DATAFAST_MID", "DATAFAST_TID", "DATAFAST_PSERV"
    , "DATAFAST_USER_DATA2", "DATAFAST_VERSIONDF"
    ]
  pure (validateDatafastMerchantParameters (Map.fromList (catMaybes values)))
  where
    readVariable name = fmap ((,) name) <$> lookupEnv name

validateDatafastMerchantParameters
  :: Map.Map String String
  -> Either Text [(ByteString, ByteString)]
validateDatafastMerchantParameters values = do
  mid <- reference "DATAFAST_MID"
  tid <- reference "DATAFAST_TID"
  pserv <- reference "DATAFAST_PSERV"
  merchantName <- required "DATAFAST_USER_DATA2" 30
  let configuredVersion = T.strip . T.pack $ Map.findWithDefault "" "DATAFAST_VERSIONDF" values
      version = if T.null configuredVersion then "2" else configuredVersion
  unless (T.length version <= 4 && T.all asciiDigit version && not ("0" `T.isPrefixOf` version)) $
    Left "DATAFAST_VERSIONDF must be 1 to 4 ASCII digits without leading zeros"
  pure $ map (\(key, value) -> (key, TE.encodeUtf8 value))
    [ ("customParameters[SHOPPER_MID]", mid)
    , ("customParameters[SHOPPER_TID]", tid)
    , ("customParameters[SHOPPER_PSERV]", pserv)
    -- Fixed identification value in Datafast's integration specification.
    , ("customParameters[SHOPPER_ECI]", "0103910")
    , ("customParameters[SHOPPER_VERSIONDF]", version)
    , ("risk.parameters[USER_DATA2]", merchantName)
    ]
  where
    required name maximumLength = do
      let value = T.strip . T.pack $ Map.findWithDefault "" name values
      unless (not (T.null value) && T.length value <= maximumLength
          && T.all (not . isControl) value) $
        Left (T.pack name <> " must be configured with bounded text without control characters")
      pure value
    reference name = do
      value <- required name 256
      unless (T.any asciiAlphanumeric value
          && T.all (\c -> asciiAlphanumeric c || c `elem` ("-_." :: String)) value) $
        Left (T.pack name <> " must contain safe provider-reference characters")
      pure value
    asciiDigit c = c >= '0' && c <= '9'
    asciiAlphanumeric c = isAsciiLower c || isAsciiUpper c || asciiDigit c
