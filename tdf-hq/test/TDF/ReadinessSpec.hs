module TDF.ReadinessSpec (spec, main) where

import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException(..), Exception, SomeException, fromException, throwIO, try)
import Test.Hspec
import TDF.App.Readiness (databaseReady, withinReadinessDeadline)

data PrivateFailure = PrivateFailure
instance Show PrivateFailure where show _ = error "PRIVATE_EXCEPTION_MUST_NOT_RENDER"
instance Exception PrivateFailure

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "database readiness boundary" $ do
  it "accepts only a successful probe" $
    withinReadinessDeadline 1000000 (pure True) `shouldReturn` True
  it "rejects a negative probe" $
    withinReadinessDeadline 1000000 (pure False) `shouldReturn` False
  it "rejects synchronous database errors without rendering private detail" $
    withinReadinessDeadline 1000000 (throwIO PrivateFailure) `shouldReturn` False
  it "includes unavailable pool acquisition in the failure boundary" $
    databaseReady (error "PRIVATE_POOL_DETAILS") `shouldReturn` False
  it "rejects work that exceeds its deadline" $
    withinReadinessDeadline 1000 (threadDelay 100000 >> pure True) `shouldReturn` False
  it "rejects a zero deadline" $
    withinReadinessDeadline 0 (pure True) `shouldReturn` False
  mapM_ (\exception -> it ("propagates " <> show exception) $ do
    result <- try (withinReadinessDeadline 1000000 (throwIO exception)) :: IO (Either SomeException Bool)
    case result of
      Left caught -> fromException caught `shouldBe` Just exception
      Right _ -> expectationFailure "Cancellation became a readiness result") [ThreadKilled, UserInterrupt]
