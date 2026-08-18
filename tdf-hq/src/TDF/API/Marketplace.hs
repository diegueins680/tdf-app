{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Marketplace where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types
  ( MarketplaceItemDTO
  , MarketplaceCartDTO
  , MarketplaceCartItemUpdate
  , MarketplaceCheckoutReq
  , MarketplaceOrderDTO
  , MarketplaceOrderUpdate
  , MarketplaceFulfillmentUpdate
  , MarketplaceRentalUpdate
  , MarketplaceRentalTermsUpdate
  , MarketplaceManualEvidenceSubmit
  , MarketplaceManualPaymentReview
  , MarketplaceCustomerRequestSubmit
  , MarketplaceCustomerRequestReview
  , MarketplaceCustomerRequestDTO
  , MarketplaceDepositSettlementSubmit
  , MarketplaceDepositSettlementReview
  , MarketplaceDepositSettlementDTO
  , MarketplaceCommerceDTO
  , DatafastCheckoutDTO
  , PaypalCreateDTO
  , PaypalCaptureReq
  )
import           TDF.DTO.SocialEventsDTO (StripePaymentIntentDTO)

type MarketplaceAPI =
       Get '[JSON] [MarketplaceItemDTO]
  :<|> Capture "id" Text :> Get '[JSON] MarketplaceItemDTO
  :<|> "cart" :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> Get '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "items" :> ReqBody '[JSON] MarketplaceCartItemUpdate :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "checkout" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] MarketplaceOrderDTO
  :<|> "cart" :> Capture "cartId" Text :> "stripe" :> "payment-intent" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] StripePaymentIntentDTO
  :<|> "cart" :> Capture "cartId" Text :> "datafast" :> "checkout" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] DatafastCheckoutDTO
  :<|> "datafast" :> "status" :> Header "X-Order-Lookup-Token" Text :> QueryParam "orderId" Text :> QueryParam "resourcePath" Text :> Get '[JSON] MarketplaceOrderDTO
  :<|> "cart" :> Capture "cartId" Text :> "paypal" :> "create" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] PaypalCreateDTO
  :<|> "paypal" :> "capture" :> Header "X-Order-Lookup-Token" Text :> ReqBody '[JSON] PaypalCaptureReq :> Post '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> Header "X-Order-Lookup-Token" Text :> Get '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> "manual-payment" :> "evidence" :> Header "X-Order-Lookup-Token" Text :> ReqBody '[JSON] MarketplaceManualEvidenceSubmit :> Post '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> "requests" :> Header "X-Order-Lookup-Token" Text :> Get '[JSON] [MarketplaceCustomerRequestDTO]
  :<|> "orders" :> Capture "orderId" Text :> "requests" :> Header "X-Order-Lookup-Token" Text :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCustomerRequestSubmit :> Post '[JSON] MarketplaceCustomerRequestDTO

type MarketplaceAdminAPI =
       Capture "listingId" Text :> "rental-terms" :> ReqBody '[JSON] MarketplaceRentalTermsUpdate :> Put '[JSON] MarketplaceItemDTO
  :<|> "orders" :> QueryParam "status" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [MarketplaceOrderDTO]
  :<|> "orders" :> Capture "orderId" Text :> "commerce" :> Get '[JSON] MarketplaceCommerceDTO
  :<|> "orders" :> Capture "orderId" Text :> "manual-payment" :> "review" :> ReqBody '[JSON] MarketplaceManualPaymentReview :> Post '[JSON] MarketplaceCommerceDTO
  :<|> "orders" :> Capture "orderId" Text :> ReqBody '[JSON] MarketplaceOrderUpdate :> Put '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> "fulfillment" :> ReqBody '[JSON] MarketplaceFulfillmentUpdate :> Put '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> "rental" :> ReqBody '[JSON] MarketplaceRentalUpdate :> Put '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> "customer-requests" :> Get '[JSON] [MarketplaceCustomerRequestDTO]
  :<|> "orders" :> Capture "orderId" Text :> "customer-requests" :> Capture "requestId" Text :> "review" :> ReqBody '[JSON] MarketplaceCustomerRequestReview :> Post '[JSON] MarketplaceCustomerRequestDTO
  :<|> "orders" :> Capture "orderId" Text :> "deposit-settlements" :> Get '[JSON] [MarketplaceDepositSettlementDTO]
  :<|> "orders" :> Capture "orderId" Text :> "deposit-settlements" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceDepositSettlementSubmit :> Post '[JSON] MarketplaceDepositSettlementDTO
  :<|> "orders" :> Capture "orderId" Text :> "deposit-settlements" :> Capture "settlementId" Text :> "review" :> ReqBody '[JSON] MarketplaceDepositSettlementReview :> Post '[JSON] MarketplaceDepositSettlementDTO
