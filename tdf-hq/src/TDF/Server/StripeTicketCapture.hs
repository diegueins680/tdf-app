{-# LANGUAGE OverloadedStrings #-}

{- | Captured money on the legacy Stripe ticket webhook that the order cannot
admit (PAY-STRIPE-WEBHOOK-001, PAY-CAPTURE-RECON-001): how it is read from the
signed event and how it is left for review.
-}
module TDF.Server.StripeTicketCapture
    ( StripeTicketCapture (..)
    , parseStripeTicketCapture
    , stripeTicketClosedOrderNeedsReview
    , recordStripeTicketCapture
    ) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Object, Parser, parseMaybe)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist.Sql (SqlPersistT)

import qualified TDF.Commerce.CheckoutStore as Checkout

{- | What a signed @payment_intent.succeeded@ event says Stripe captured,
read without the ticket binding. It is what remains when the binding in
@metadata.tdf_context@ is missing or does not match the stored order: the
money was still captured and has to be surfaced for review
(PAY-STRIPE-WEBHOOK-001, PAY-CAPTURE-RECON-001).
-}
data StripeTicketCapture = StripeTicketCapture
    { stcAmountReceived :: Int64
    , stcCurrency :: Maybe T.Text
    , stcEnvironment :: Checkout.CheckoutEnvironment
    }
    deriving (Eq, Show)

-- | 'Nothing' unless the PaymentIntent is succeeded with a positive amount.
-- A missing @livemode@ is treated as production so the capture is never
-- hidden in the sandbox queue.
parseStripeTicketCapture :: Aeson.Value -> Maybe StripeTicketCapture
parseStripeTicketCapture payload = do
    (status, amountReceived, mCurrency, mLiveMode) <-
        parseMaybe
            ( Aeson.withObject "event" $ \eventObject -> do
                dataObject <- eventObject Aeson..: "data" :: Parser Object
                paymentIntentObject <- dataObject Aeson..: "object" :: Parser Object
                (,,,)
                    <$> paymentIntentObject Aeson..: "status"
                    <*> paymentIntentObject Aeson..: "amount_received"
                    <*> paymentIntentObject Aeson..:? "currency"
                    <*> eventObject Aeson..:? "livemode"
            )
            payload
    if T.toLower (T.strip status) == ("succeeded" :: T.Text) && amountReceived > (0 :: Int64)
        then
            Just
                StripeTicketCapture
                    { stcAmountReceived = amountReceived
                    , stcCurrency = mCurrency
                    , stcEnvironment =
                        if mLiveMode == Just False
                            then Checkout.CheckoutSandbox
                            else Checkout.CheckoutProduction
                    }
        else Nothing

-- | A valid capture for an order that is neither pending nor already paid
-- (cancelled, expired, refunded) issued no ticket and needs review. A paid
-- order is a redelivery and needs none.
stripeTicketClosedOrderNeedsReview :: T.Text -> Bool
stripeTicketClosedOrderNeedsReview status =
    T.toLower (T.strip status) `notElem` ["pending", "paid"]

{- | Leave an open reconciliation exception for a capture on a legacy ticket
order. Stripe has no merchant account id in this configuration, so the
merchant reference is the constant @stripe@; the open-exception fingerprint
keeps redeliveries to one row.
-}
recordStripeTicketCapture ::
    -- | exception type
    T.Text ->
    -- | ticket order key
    T.Text ->
    -- | PaymentIntent id
    T.Text ->
    -- | order total in minor units
    Int ->
    -- | order currency
    T.Text ->
    UTCTime ->
    StripeTicketCapture ->
    SqlPersistT IO ()
recordStripeTicketCapture exceptionType orderKey paymentIntentId expectedAmount orderCurrency now capture =
    Checkout.recordReconciliationException
        Checkout.ProviderStripe
        (stcEnvironment capture)
        "stripe"
        exceptionType
        ("event_ticket_order:" <> orderKey)
        paymentIntentId
        (fromIntegral expectedAmount)
        (Just (stcAmountReceived capture))
        (fromMaybe orderCurrency (stcCurrency capture))
        now
