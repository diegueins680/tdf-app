{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}
module TDF.EventOperations.HttpTestConfig (httpTestConfig) where

import TDF.Config (AppConfig(..))

-- Complete configuration: never load host environment credentials or enable external workers.
httpTestConfig :: AppConfig
httpTestConfig = AppConfig
  { dbHost = "127.0.0.1", dbPort = "0", dbUser = "test", dbPass = "", dbName = "unused"
  , dbConnUrl = Nothing, dbSslMode = Nothing, appPort = 0
  , resetDb = False, seedDatabase = False, runMigrations = False, seedTriggerToken = Nothing
  , appBaseUrl = Nothing, assetsBaseUrl = Nothing, assetsRootDir = "unused"
  , courseDefaultSlug = "test", courseDefaultMapUrl = Nothing, courseDefaultInstructorAvatar = Nothing
  , openAiApiKey = Nothing, openAiModel = "unused", openAiEmbedModel = "unused"
  , chatKitWorkflowId = Nothing, chatKitApiBase = disabledEndpoint
  , ragTopK = 1, ragChunkWords = 1, ragChunkOverlap = 0, ragAvailabilityDays = 1
  , ragAvailabilityPerResource = 1, ragRefreshHours = 1, ragEmbedBatchSize = 1
  , emailConfig = Nothing, googleClientId = Nothing
  , facebookAppId = Nothing, facebookAppSecret = Nothing, facebookGraphBase = disabledEndpoint
  , facebookMessagingToken = Nothing, facebookMessagingPageId = Nothing
  , facebookMessagingApiBase = disabledEndpoint, instagramAppToken = Nothing
  , instagramGraphBase = disabledEndpoint, instagramMessagingToken = Nothing
  , instagramMessagingAccountId = Nothing, instagramMessagingApiBase = disabledEndpoint
  , instagramVerifyToken = Nothing
  , sessionCookieName = "tdf_session", sessionCookieDomain = Nothing, sessionCookiePath = "/"
  , sessionCookieSecure = False, sessionCookieSameSite = "Lax", sessionCookieMaxAgeSeconds = Nothing
  , stripeSecretKey = Nothing, stripePublishableKey = Nothing, stripeWebhookSecret = Nothing
  , contextualReputationEnabled = False, publicReputationProjectionEnabled = False
  , eventDiscoveryEnabled = False, eventDiscoveryAutoPublish = False, eventDiscoveryPilotLimit = 1
  , ticketmasterApiKey = Nothing, ticketmasterApiBase = disabledEndpoint
  , eventDiscoveryLookaheadDays = 1, eventDiscoveryMaxPagesPerCity = 1, eventDiscoveryHourLocal = 0
  , eventDiscoveryCountryCode = Nothing, googleRoutesApiKey = Nothing, googleRoutesApiBase = disabledEndpoint
  , eventLogisticsRecheckEnabled = False, artistEnrichmentEnabled = False
  , artistEnrichmentAutoPublish = False, artistEnrichmentHourLocal = 0
  , artistEnrichmentBatchSize = 1, artistEnrichmentStaleDays = 1
  , defaultCurrency = "USD", supportedCurrencies = ["USD"], defaultTimezone = "America/Guayaquil"
  , supportedLocales = ["es", "en"], defaultLocale = "es", enableGdprCompliance = True
  }
  where disabledEndpoint = "http://127.0.0.1:1"
