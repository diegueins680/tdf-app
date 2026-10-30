{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.TicketBuyerContact
  ( ticketPaymentBuyerPhone
  , normalizeTicketBuyerPhone
  ) where

import Data.Aeson (decodeStrict', withObject, (.:?))
import Data.Aeson.Types (parseMaybe)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Only the buyer_phone field saved with the order is a phone source. Historical
-- absent/malformed metadata remains optional; never substitute an email or ID.
ticketPaymentBuyerPhone :: Maybe Text -> Maybe Text
ticketPaymentBuyerPhone raw = do
  metadata <- decodeStrict' . TE.encodeUtf8 =<< raw
  phone <- parseMaybe (withObject "ticket buyer contact" (.:? "buyer_phone")) metadata
  either (const Nothing) id (normalizeTicketBuyerPhone phone)

-- The same boundary validates initial order input and the stored projection.
normalizeTicketBuyerPhone :: Maybe Text -> Either Text (Maybe Text)
normalizeTicketBuyerPhone raw = case T.strip <$> raw of
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just clean
    | T.length clean > 24 -> Left "buyerPhone is too long"
    | T.any (\char -> not (isAlphaNum char || char `elem` ['+','-',' ','(',')'])) clean ->
        Left "buyerPhone contains unsupported characters"
    | otherwise -> Right (Just clean)
