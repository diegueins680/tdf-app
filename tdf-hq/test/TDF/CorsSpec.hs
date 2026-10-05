{-# LANGUAGE OverloadedStrings #-}
module TDF.CorsSpec (spec, main) where

import Control.Exception (IOException, bracket)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as BS
import Network.Wai (defaultRequest, requestHeaders)
import qualified Network.Wai as Wai
import Network.Wai.Internal (ResponseReceived (..))
import qualified Network.HTTP.Types as HTTPTypes
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec
import TDF.Cors (corsPolicy)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "production CORS trust boundary" $ do
        let productionCorsOverrides =
                [ ("APP_ENV", Just "production")
                , ("ENVIRONMENT", Nothing)
                , ("NODE_ENV", Nothing)
                , ("RUNTIME_ENV", Nothing)
                , ("ALLOWED_ORIGINS", Nothing)
                , ("ALLOW_ORIGINS", Nothing)
                , ("ALLOW_ORIGIN", Nothing)
                , ("CORS_ALLOW_ORIGINS", Nothing)
                , ("CORS_ALLOW_ORIGIN", Nothing)
                , ("ALLOW_ALL_ORIGINS", Nothing)
                , ("CORS_ALLOW_ALL_ORIGINS", Nothing)
                , ("CORS_DISABLE_DEFAULTS", Just "true")
                , ("DISABLE_DEFAULT_CORS", Nothing)
                , ("HQ_APP_URL", Just "https://www.tdfrecords.net")
                ]
            withProductionCors override action =
                withEnvOverrides (override ++ filter (\(key, _) -> key `notElem` map fst override) productionCorsOverrides) action
        forM_ ["APP_ENV", "ENVIRONMENT", "NODE_ENV", "RUNTIME_ENV"] $ \key ->
            forM_ ["prod", "production", " Live "] $ \value ->
                it ("rejects allow-all when " <> key <> "=" <> value) $
                    withProductionCors ([(key, Just value), ("ALLOW_ALL_ORIGINS", Just "true")]
                        ++ [("APP_ENV", Just "development") | key /= "APP_ENV"]) $
                        corsPolicy `shouldThrow` \err ->
                            "Production CORS requires explicit trusted origins" `isInfixOf` show (err :: IOException)
        forM_ ["ALLOW_ALL_ORIGINS", "CORS_ALLOW_ALL_ORIGINS"] $ \flag ->
            it ("rejects credentialed allow-all in production through " <> flag) $
                withProductionCors [(flag, Just "true")] $
                    corsPolicy `shouldThrow` \err ->
                        "Production CORS requires explicit trusted origins" `isInfixOf` show (err :: IOException)
        forM_ ["ALLOWED_ORIGINS", "ALLOW_ORIGINS", "ALLOW_ORIGIN", "CORS_ALLOW_ORIGINS", "CORS_ALLOW_ORIGIN"] $ \flag ->
            it ("rejects a production wildcard through " <> flag) $
                withProductionCors [(flag, Just "*")] $
                    corsPolicy `shouldThrow` \err ->
                        "Production CORS requires explicit trusted origins" `isInfixOf` show (err :: IOException)
        it "accepts the canonical origin and denies an untrusted origin before the handler" $
            withProductionCors [("ALLOW_ORIGINS", Just "https://www.tdfrecords.net,https://tdfrecords.net")] $ do
                middleware <- corsPolicy
                let application _ respond = respond (Wai.responseLBS HTTPTypes.status200 [] "accepted")
                    request origin = defaultRequest { requestHeaders = [("Origin", origin)] }
                _ <- middleware application (request "https://www.tdfrecords.net") $ \response -> do
                    Wai.responseStatus response `shouldBe` HTTPTypes.status200
                    lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response)
                        `shouldBe` Just "https://www.tdfrecords.net"
                    lookup "Access-Control-Allow-Credentials" (Wai.responseHeaders response)
                        `shouldBe` Just "true"
                    pure ResponseReceived
                _ <- middleware (\_ _ -> expectationFailure "Untrusted origin reached handler" >> pure ResponseReceived)
                        (request "https://conformance.invalid") $ \response -> do
                    Wai.responseStatus response `shouldNotBe` HTTPTypes.status200
                    lookup "Access-Control-Allow-Credentials" (Wai.responseHeaders response)
                        `shouldNotBe` Just "true"
                    pure ResponseReceived
                pure ()
        forM_ ["GET", "POST"] $ \verb ->
            it ("accepts public checkout lookup-token preflight for trusted origin: " <> show verb) $
                withProductionCors [("ALLOW_ORIGINS", Just "https://www.tdfrecords.net")] $ do
                    middleware <- corsPolicy
                    let preflight origin = defaultRequest
                          { Wai.requestMethod = "OPTIONS"
                          , requestHeaders = [("Origin", origin)
                            , ("Access-Control-Request-Method", verb)
                            , ("Access-Control-Request-Headers", "content-type,x-order-lookup-token,idempotency-key")]
                          }
                        noHandler _ _ = expectationFailure "Preflight reached application" >> pure ResponseReceived
                    _ <- middleware noHandler (preflight "https://www.tdfrecords.net") $ \response -> do
                        Wai.responseStatus response `shouldBe` HTTPTypes.status200
                        lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response)
                            `shouldBe` Just "https://www.tdfrecords.net"
                        lookup "Access-Control-Allow-Headers" (Wai.responseHeaders response)
                            `shouldSatisfy` maybe False (BS.isInfixOf "x-order-lookup-token")
                        pure ResponseReceived
                    _ <- middleware noHandler (preflight "https://checkout.attacker.invalid") $ \response -> do
                        Wai.responseStatus response `shouldNotBe` HTTPTypes.status200
                        lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response) `shouldBe` Nothing
                        pure ResponseReceived
                    pure ()
        forM_ ["https://tdfui.pages.dev", "https://pr-123.tdfui.pages.dev", "https://pr-123.tdf-app.pages.dev", "http://localhost:5173"] $ \origin ->
            it ("does not implicitly trust production origin " <> show origin) $
                withProductionCors [("CORS_DISABLE_DEFAULTS", Just "false")] $ do
                    middleware <- corsPolicy
                    _ <- middleware (\_ _ -> expectationFailure "Implicit origin reached handler" >> pure ResponseReceived)
                            defaultRequest { requestHeaders = [("Origin", origin)] } $ \response -> do
                        Wai.responseStatus response `shouldNotBe` HTTPTypes.status200
                        lookup "Access-Control-Allow-Credentials" (Wai.responseHeaders response) `shouldNotBe` Just "true"
                        pure ResponseReceived
                    pure ()
        it "admits an explicitly approved preview origin in production" $
            withProductionCors [("ALLOW_ORIGINS", Just "https://approved.tdfui.pages.dev")] $ do
                middleware <- corsPolicy
                _ <- middleware (\_ respond -> respond (Wai.responseLBS HTTPTypes.status200 [] "accepted"))
                        defaultRequest { requestHeaders = [("Origin", "https://approved.tdfui.pages.dev")] } $ \response -> do
                    Wai.responseStatus response `shouldBe` HTTPTypes.status200
                    lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response) `shouldBe` Just "https://approved.tdfui.pages.dev"
                    pure ResponseReceived
                pure ()


withEnvOverrides :: [(String, Maybe String)] -> IO a -> IO a
withEnvOverrides overrides action =
    bracket setup restore (const action)
  where
    setup = do
        previous <- mapM capture overrides
        apply overrides
        pure previous
    restore previous = apply previous
    capture (key, _) = do
        value <- lookupEnv key
        pure (key, value)
    apply = mapM_ assign
    assign (key, value) =
        case value of
            Just raw -> setEnv key raw
            Nothing -> unsetEnv key
