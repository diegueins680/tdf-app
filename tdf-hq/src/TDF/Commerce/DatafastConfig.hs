{-# LANGUAGE OverloadedStrings #-}

-- | Merchant metadata required by Dataweb, shared by every checkout domain.
-- API credentials and this metadata do not establish merchant certification.
module TDF.Commerce.DatafastConfig
  ( loadDatafastMerchantParameters
  , validateDatafastMerchantParameters
  , encodeDatafastForm
  , splitDatafastBuyerName
  , DatafastCartLine(..)
  , datafastCartParameters
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, isControl)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import Network.HTTP.Types.URI (renderSimpleQuery)
import TDF.Commerce.Money (checkedCheckoutSubtotals)

-- | Provider projection of a persisted checkout line. Prices are minor units,
-- never a recomputation from the live product catalogue or client form.
data DatafastCartLine = DatafastCartLine
  { dfclDescription :: Text
  , dfclQuantity :: Int
  , dfclUnitAmountMinor :: Int64
  } deriving (Eq, Show)

-- | Render the documented Dataweb cart only when every integer line conserves
-- the exact charged amount. This does not classify tax or infer a zero rate.
datafastCartParameters
  :: Int64 -> Text -> [DatafastCartLine]
  -> Either Text [(ByteString, ByteString)]
datafastCartParameters amount currency linesToSend = do
  unless (T.toUpper (T.strip currency) == "USD") $
    Left "Dataweb cart requires a USD checkout"
  unless (amount <= 999999999999) $
    Left "Dataweb cart amount exceeds the provider decimal field"
  _ <- checkedCheckoutSubtotals amount
    [(dfclQuantity line, dfclUnitAmountMinor line) | line <- linesToSend]
  mapM_ validateLine linesToSend
  pure (concat (zipWith renderLine [0 :: Int ..] linesToSend))
  where
    validateLine line = do
      let description = dfclDescription line
      unless (not (T.null (T.strip description)) && T.length description <= 255
          && not (T.any isControl description) && not (T.any (== '&') description)) $
        Left "Dataweb cart description must be nonempty bounded text without controls or ampersands"
      unless (dfclQuantity line <= 99999) $
        Left "Dataweb cart quantity exceeds the provider field"
    renderLine index line =
      let prefix = "cart.items[" <> T.pack (show index) <> "]."
          textPair key value = (TE.encodeUtf8 (prefix <> key), TE.encodeUtf8 value)
          cents = dfclUnitAmountMinor line
          fraction = T.pack (show (cents `mod` 100))
          decimal = T.pack (show (cents `div` 100)) <> "." <> T.justifyRight 2 '0' fraction
          description = T.strip (dfclDescription line)
      in [ textPair "name" description
         , textPair "description" description
         , textPair "price" decimal
         , textPair "quantity" (T.pack (show (dfclQuantity line)))
         ]

-- HTTP form encoding works on UTF-8 bytes, never Unicode code points.
encodeDatafastForm :: [(ByteString, ByteString)] -> ByteString
encodeDatafastForm = renderSimpleQuery False

-- | Project only supplied name components; never invent a missing surname.
-- This preserves the existing free-text projection, not a verified identity.
splitDatafastBuyerName :: Text -> (Text, Text)
splitDatafastBuyerName raw =
  case T.words raw of
    [] -> ("", "")
    [name] -> (name, "")
    (givenName:rest) -> (givenName, T.unwords rest)

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
