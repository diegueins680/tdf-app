module TDF.Services.MessagingManager
  ( messagingManagerSettings
  , sharedMessagingManager
  ) where

import Network.HTTP.Client (Manager, ManagerSettings(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO.Unsafe (unsafePerformIO)

-- http-client may replay even POST when a reused connection loses its response.
-- Message delivery has no provider idempotency key; retain the unknown outcome.
messagingManagerSettings :: ManagerSettings
messagingManagerSettings = tlsManagerSettings
  { managerRetryableException = const False }

sharedMessagingManager :: Manager
sharedMessagingManager = unsafePerformIO (newManager messagingManagerSettings)
{-# NOINLINE sharedMessagingManager #-}
