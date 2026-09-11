{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.ProviderCapabilityStore
  ( loadProviderActivations
  ) where

import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawSql)

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment(..)
  , PaymentProvider(..)
  , checkoutEnvironmentText
  )
import           TDF.Commerce.ProviderCapabilities
  ( ProviderActivation(..)
  , PaymentCapability
  , PaymentMethod
  , paymentCapabilityFromText
  , paymentMethodFromText
  )

loadProviderActivations
  :: CheckoutEnvironment
  -> SqlPersistT IO [ProviderActivation]
loadProviderActivations environment = do
  accountRows <- (rawSql
    "SELECT account.provider, account.enabled, account.credential_status,\
    \ account.contract_status, COALESCE(flag.enabled, FALSE)\
    \ FROM commerce_provider_account account\
    \ LEFT JOIN revenue_feature_flag flag\
    \   ON flag.flag_key = account.feature_flag_key\
    \  AND flag.environment = account.environment\
    \ WHERE account.environment = ?\
    \ ORDER BY account.provider"
    [PersistText (checkoutEnvironmentText environment)]
    :: SqlPersistT IO
        [(Single Text, Single Bool, Single Text, Single Text, Single Bool)])
  capabilityRows <- (rawSql
    "SELECT account.provider, capability.payment_method, capability.capability\
    \ FROM commerce_provider_account account\
    \ JOIN commerce_provider_capability capability\
    \   ON capability.provider_account_id = account.id\
    \ WHERE account.environment = ?\
    \ AND capability.verification_status = ?\
    \ ORDER BY account.provider, capability.payment_method, capability.capability"
    [ PersistText (checkoutEnvironmentText environment)
    , PersistText (case environment of
        CheckoutSandbox -> "sandbox_verified"
        CheckoutProduction -> "production_verified")
    ]
    :: SqlPersistT IO [(Single Text, Single Text, Single Text)])
  pure
    [ ProviderActivation
        { paProvider = provider
        , paEnvironment = environment
        , paFeatureEnabled = enabled && (environment == CheckoutSandbox || featureEnabled)
        , paCredentialsValidated = credentialStatus == "validated"
        , paContractApproved = contractStatus == "approved"
        , paVerifiedMethods = methodsFor providerText capabilityRows
        , paVerifiedCapabilities = capabilitiesFor providerText capabilityRows
        , paVerifiedMethodCapabilities = methodCapabilitiesFor providerText capabilityRows
        }
    | ( Single providerText
      , Single enabled
      , Single credentialStatus
      , Single contractStatus
      , Single featureEnabled
      ) <- accountRows
    , Just provider <- [parseProvider providerText]
    ]

methodsFor
  :: Text
  -> [(Single Text, Single Text, Single Text)]
  -> [PaymentMethod]
methodsFor provider rows = mapMaybe paymentMethodFromText
  [ method
  | (Single rowProvider, Single method, _) <- rows
  , rowProvider == provider
  ]

capabilitiesFor
  :: Text
  -> [(Single Text, Single Text, Single Text)]
  -> [PaymentCapability]
capabilitiesFor provider rows = mapMaybe paymentCapabilityFromText
  [ capability
  | (Single rowProvider, _, Single capability) <- rows
  , rowProvider == provider
  ]

methodCapabilitiesFor
  :: Text
  -> [(Single Text, Single Text, Single Text)]
  -> [(PaymentMethod, PaymentCapability)]
methodCapabilitiesFor provider rows = mapMaybe parsePair
  [ (method, capability)
  | (Single rowProvider, Single method, Single capability) <- rows
  , rowProvider == provider
  ]
  where
    parsePair (method, capability) =
      (,) <$> paymentMethodFromText method <*> paymentCapabilityFromText capability

parseProvider :: Text -> Maybe PaymentProvider
parseProvider provider = case provider of
  "datafast" -> Just ProviderDatafast
  "paypal" -> Just ProviderPayPal
  "placetopay" -> Just ProviderPlaceToPay
  "payphone" -> Just ProviderPayPhone
  "stripe" -> Just ProviderStripe
  "bank_transfer" -> Just ProviderBankTransfer
  _ -> Nothing
