{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.WorkerLoggingSpec (spec) where

import qualified Control.Exception as E
import           Control.Monad (void)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import           Data.Text (Text)
import           Test.Hspec
import qualified Test.QuickCheck as QC

import qualified TDF.Commerce.MerchReservationWorker as Merch
import qualified TDF.Commerce.ProviderEventWorker as Events
import qualified TDF.Commerce.ProviderReconciliation as Query

-- The names are markers, not real credentials, card data, accounts or customers.
syntheticFailure :: String
syntheticFailure = "Authorization=TEST_ONLY_BEARER; database=TEST_ONLY_DSN; "
  <> "key=TEST_ONLY_ENCRYPTION_KEY; buyer=TEST_ONLY_BUYER; "
  <> "cardData=TEST_ONLY_CARD_MARKER; callback=TEST_ONLY_CALLBACK?token=TEST_ONLY_TOKEN"
  <> "\n\r\t\NUL\ESC\\\"{\"level\":\"info\",\"paid\":true}"

data UnrenderableFailure = UnrenderableFailure
instance Show UnrenderableFailure where
  show _ = error "Exception rendering must not be evaluated by payment worker logging"
instance E.Exception UnrenderableFailure

type Iteration = IO () -> (String -> IO ()) -> (String -> IO ()) -> IO ()

emptyEvents :: Events.ProviderEventWorkerStats
emptyEvents = Events.ProviderEventWorkerStats 0 0 0 0 0

spec :: Spec
spec = describe "payment worker logging boundary" $ do
  failureBoundary "provider events"
    (\tick -> Events.providerEventWorkerIterationWith (tick >> pure emptyEvents))
    "provider-event-worker" "tick failed"
  failureBoundary "merch reservation expiry"
    (\tick -> Merch.merchReservationWorkerIterationWith (tick >> pure 0))
    "merch-reservation-worker" "tick failed"
  failureBoundary "provider query recovery"
    (\tick logError _ -> Query.providerQueryWorkerIterationWith (tick >> pure 0) logError)
    "provider-query-worker" "tick failed; lease recovery required"

  it "keeps provider-event counters on the information sink without payment claims" $ do
    (info, captureInfo) <- captureLog
    (errors, captureError) <- captureLog
    Events.providerEventWorkerIterationWith
      (pure (Events.ProviderEventWorkerStats 5 1 1 2 1)) captureError captureInfo
    readIORef errors `shouldReturn` []
    messages <- readIORef info
    map decodeLog messages `shouldBe` [Just (A.object
      [ "component" .= ("provider-event-worker" :: Text), "level" .= ("info" :: Text)
      , "claimed" .= (5 :: Int), "processed" .= (1 :: Int), "ignored" .= (1 :: Int)
      , "retried" .= (2 :: Int), "deadLettered" .= (1 :: Int) ])]

  it "keeps reservation expiry counts on the information sink" $ do
    (info, captureInfo) <- captureLog
    (errors, captureError) <- captureLog
    Merch.merchReservationWorkerIterationWith (pure 3) captureError captureInfo
    readIORef errors `shouldReturn` []
    messages <- readIORef info
    map decodeLog messages `shouldBe` [Just (A.object
      [ "component" .= ("merch-reservation-worker" :: Text)
      , "level" .= ("info" :: Text), "expiredCheckouts" .= (3 :: Int) ])]

  it "does not repeat successful work when its information sink fails" $ do
    ticks <- newIORef (0 :: Int)
    let tick result = modifyIORef' ticks (+ 1) >> pure result
        unavailable _ = E.throwIO (userError syntheticFailure)
    Events.providerEventWorkerIterationWith
      (tick (Events.ProviderEventWorkerStats 1 1 0 0 0)) unavailable unavailable
    Merch.merchReservationWorkerIterationWith (tick 1) unavailable unavailable
    readIORef ticks `shouldReturn` 2

failureBoundary :: String -> Iteration -> Text -> Text -> Spec
failureBoundary label iteration component message = describe label $ do
  let expected = Just (A.object
        [ "component" .= component, "level" .= ("error" :: Text), "message" .= message ])
      failing = E.throwIO (userError syntheticFailure)

  it "emits one fixed JSON event and no exception details" $ do
    (errors, captureError) <- captureLog
    (info, captureInfo) <- captureLog
    iteration failing captureError captureInfo
    messages <- readIORef errors
    map decodeLog messages `shouldBe` [expected]
    readIORef info `shouldReturn` []

  it "makes failure output independent of arbitrary exception text" $ QC.property $
    \payload -> QC.ioProperty $ do
      (errors, captureError) <- captureLog
      iteration (E.throwIO (userError payload)) captureError (const (pure ()))
      messages <- readIORef errors
      pure (map decodeLog messages == [expected])

  it "never evaluates the exception renderer" $ do
    (errors, captureError) <- captureLog
    iteration (E.throwIO UnrenderableFailure) captureError (const (pure ()))
    messages <- readIORef errors
    map decodeLog messages `shouldBe` [expected]

  it "emits no failure or success event for idle work" $ do
    (messages, capture) <- captureLog
    iteration (pure ()) capture capture
    readIORef messages `shouldReturn` []

  it "survives an unavailable error sink without retrying work inside the iteration" $ do
    ticks <- newIORef (0 :: Int)
    let unavailable _ = E.throwIO (userError syntheticFailure)
    iteration (modifyIORef' ticks (+ 1) >> failing) unavailable unavailable
    iteration (modifyIORef' ticks (+ 1)) unavailable unavailable
    readIORef ticks `shouldReturn` 2

  it "propagates worker cancellation without logging it as a retryable failure" $ do
    (messages, capture) <- captureLog
    result <- E.try (iteration (E.throwIO E.ThreadKilled) capture capture)
      :: IO (Either E.AsyncException ())
    result `shouldBe` Left E.ThreadKilled
    readIORef messages `shouldReturn` []

  it "propagates cancellation during logging" $ do
    let cancelled _ = E.throwIO E.ThreadKilled
    result <- E.try (iteration failing cancelled cancelled)
      :: IO (Either E.AsyncException ())
    result `shouldBe` Left E.ThreadKilled

captureLog :: IO (IORef [String], String -> IO ())
captureLog = do
  messages <- newIORef []
  -- Force the same string a real handle would write; do not hide lazy render errors.
  pure (messages, \message -> do
    void (E.evaluate (length message))
    modifyIORef' messages (<> [message]))

decodeLog :: String -> Maybe A.Value
decodeLog = A.decode . BL.pack
