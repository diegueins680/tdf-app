{-# LANGUAGE OverloadedStrings #-}

-- | AUTH-CORS-001: pure origin predicates of the CORS boundary under
-- generated origins. The middleware construction itself is covered by
-- CorsSpec and scripts/verify-cors-boundary.py.
module TDF.CorsPropertiesSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char (toLower)
import           Data.Either (isLeft, isRight)
import           Data.List (isInfixOf)
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Cors
  ( deriveCorsOriginFromAppBase
  , isAccountDeletionRequestAllowed
  , isTrustedPreviewOrigin
  )

newtype Label = Label String deriving (Show)

instance Arbitrary Label where
  arbitrary = Label <$> (chooseInt (1, 12) >>= \n -> vectorOf n (elements (['a'..'z'] ++ ['0'..'9'])))

trustedRoots :: [String]
trustedRoots = ["tdfui.pages.dev", "tdf-app.pages.dev"]

mixCase :: [Bool] -> String -> String
mixCase flags = zipWith (\u c -> if u then toUpperAscii c else c) (cycle (flags ++ [False]))
  where toUpperAscii c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

spec :: Spec
spec = describe "CORS origin predicates (AUTH-CORS-001)" $ do
  describe "isTrustedPreviewOrigin" $ do
    it "accepts exactly the two Pages roots and one preview label under them, over https, in any case" $
      property $ \(Label label) flags -> forAll (elements trustedRoots) $ \root ->
        let origins = ["https://" <> root, "https://" <> label <> "." <> root]
        in conjoin [isTrustedPreviewOrigin (BS.pack (mixCase flags o)) === True | o <- origins]
    it "rejects http, ports, paths, deeper labels, lookalike suffixes and foreign hosts" $
      property $ \(Label a) (Label b) -> forAll (elements trustedRoots) $ \root ->
        let rejected =
              [ "http://" <> root
              , "https://" <> root <> ":8443"
              , "https://" <> root <> "/"
              , "https://" <> root <> "?x=1"
              , "https://" <> a <> "." <> b <> "." <> root
              , "https://" <> a <> root               -- missing dot: evil-tdfui.pages.dev
              , "https://" <> root <> "." <> a        -- suffix appended
              , "https://" <> a <> ".pages.dev"       -- another Pages project
              , "https://" <> a <> ".example.com"
              , "https://"
              , root
              ]
        in conjoin [counterexample o (isTrustedPreviewOrigin (BS.pack o) === False) | o <- rejected, not (a `isInfixOf` root)]
    it "never accepts an origin whose host is not a label-wise suffix match" $
      property $ \(Label h) (Label tld) ->
        let host = h <> "." <> tld
        in not (host `elem` trustedRoots) ==> isTrustedPreviewOrigin (BS.pack ("https://" <> host)) === False

  describe "isAccountDeletionRequestAllowed" $ do
    it "requires the proof header, rejects the null origin and never admits an origin only via the wildcard" $
      property $ \(Label a) ->
        let origin = BS.pack ("https://" <> a <> ".example.com")
            proof = Just "TDF-Account-Deletion"
        in conjoin
             [ isAccountDeletionRequestAllowed [origin] (Just origin) proof === True
             , isAccountDeletionRequestAllowed [origin] (Just origin) Nothing === False
             , isAccountDeletionRequestAllowed [origin] (Just origin) (Just "other") === False
             , isAccountDeletionRequestAllowed [origin] (Just "null") proof === False
             , isAccountDeletionRequestAllowed ["*"] (Just origin) proof === False
             , isAccountDeletionRequestAllowed [] (Just origin) proof === False
             , isAccountDeletionRequestAllowed [] Nothing proof === True
             ]

  describe "deriveCorsOriginFromAppBase" $ do
    it "yields a scheme and host only, and is idempotent" $
      property $ \(Label h) (Label t) https -> forAll (elements ["", "/", "/app", "/app/x"]) $ \path ->
        let scheme = if https then "https://" else "http://"
            base = scheme <> h <> "." <> t <> path
        in case deriveCorsOriginFromAppBase base of
             Left err -> counterexample err False
             Right origin -> origin === scheme <> h <> "." <> t
               .&&. deriveCorsOriginFromAppBase origin === Right origin
    it "rejects default ports, non-ASCII, whitespace and non-http schemes" $ do
      deriveCorsOriginFromAppBase "https://tdf.example:443" `shouldSatisfy` isLeft
      deriveCorsOriginFromAppBase "http://tdf.example:80" `shouldSatisfy` isLeft
      deriveCorsOriginFromAppBase "https://tdf.example /" `shouldSatisfy` isLeft
      deriveCorsOriginFromAppBase "https://tdf.ex\8203ample" `shouldSatisfy` isLeft
      deriveCorsOriginFromAppBase "ftp://tdf.example" `shouldSatisfy` isLeft
      deriveCorsOriginFromAppBase "https://tdf.example:8443/app" `shouldSatisfy` isRight
      deriveCorsOriginFromAppBase "https://tdf.example/app?x=1" `shouldSatisfy` isLeft
      deriveCorsOriginFromAppBase "https://tdf.example/app#frag" `shouldSatisfy` isLeft
