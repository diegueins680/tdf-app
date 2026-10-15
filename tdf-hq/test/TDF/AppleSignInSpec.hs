{-# LANGUAGE OverloadedStrings #-}

-- | Sign in with Apple identity-token verification against synthetic RSA keys.
-- No test touches the network: the JWKS fetch is injected and counted.
module TDF.AppleSignInSpec (spec) where

import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (i2osp)
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Test.Hspec

import TDF.AppleSignIn
  ( AppleIdentity (..)
  , AppleJwk (..)
  , AppleJwks (..)
  , AppleTokenError (..)
  , AppleVerification (..)
  , appleIssuer
  , appleJwkPublicKey
  , appleNonceDigest
  , base64UrlDecode
  , decodeAppleJwks
  , newAppleKeyCache
  , parseAppleTokenHeaderKid
  , verifyAppleIdentityToken
  , verifyAppleIdentityTokenWithKeys
  )

data SigningKey = SigningKey
  { skKid :: Text
  , skPublic :: RSA.PublicKey
  , skPrivate :: RSA.PrivateKey
  }

newSigningKey :: Text -> IO SigningKey
newSigningKey kid = do
  (publicKey, privateKey) <- RSA.generate 256 65537
  pure SigningKey { skKid = kid, skPublic = publicKey, skPrivate = privateKey }

b64 :: ByteString -> Text
b64 = TE.decodeUtf8 . B64URL.encodeUnpadded

jwkFor :: SigningKey -> AppleJwk
jwkFor key =
  AppleJwk
    { ajKid = skKid key
    , ajKty = "RSA"
    , ajAlg = Just "RS256"
    , ajUse = Just "sig"
    , ajN = b64 (i2osp (RSA.public_n (skPublic key)))
    , ajE = b64 (i2osp (RSA.public_e (skPublic key)))
    }

now :: UTCTime
now = UTCTime (fromGregorian 2026 10 13) 43200

epochSeconds :: UTCTime -> Integer
epochSeconds = floor . utcTimeToPOSIXSeconds

audience :: Text
audience = "com.tdfrecords.app"

verification :: AppleVerification
verification = AppleVerification { avAudiences = [audience], avNow = now, avExpectedNonce = Nothing }

-- | Standard claims; tests override individual pairs.
baseClaims :: [(Text, Value)]
baseClaims =
  [ ("iss", String appleIssuer)
  , ("aud", String audience)
  , ("sub", String "001234.abcdef1234567890abcdef1234567890.1234")
  , ("exp", Number (fromInteger (epochSeconds now + 600)))
  , ("iat", Number (fromInteger (epochSeconds now - 5)))
  , ("email", String "relay-abc123@privaterelay.appleid.com")
  , ("email_verified", String "true")
  , ("is_private_email", String "true")
  ]

withClaims :: [(Text, Value)] -> [(Text, Value)]
withClaims overrides = foldr override baseClaims overrides
  where
    override (key, value) claims
      | any ((== key) . fst) claims = map (\(k, v) -> if k == key then (k, value) else (k, v)) claims
      | otherwise = (key, value) : claims

withoutClaim :: Text -> [(Text, Value)] -> [(Text, Value)]
withoutClaim key = filter ((/= key) . fst)

claimsObject :: [(Text, Value)] -> Value
claimsObject pairs = object [Key.fromText k .= v | (k, v) <- pairs]

encodeSegment :: Value -> Text
encodeSegment = b64 . BL.toStrict . encode

signToken :: SigningKey -> Value -> Value -> IO Text
signToken key header claims = do
  let signingInput = encodeSegment header <> "." <> encodeSegment claims
  signed <- either (fail . show) pure (PKCS15.sign Nothing (Just SHA256) (skPrivate key) (TE.encodeUtf8 signingInput))
  pure (signingInput <> "." <> b64 signed)

rs256Header :: Text -> Value
rs256Header kid = object ["alg" .= ("RS256" :: Text), "kid" .= kid, "typ" .= ("JWT" :: Text)]

tokenWith :: SigningKey -> [(Text, Value)] -> IO Text
tokenWith key claims = signToken key (rs256Header (skKid key)) (claimsObject claims)

verifyWith :: SigningKey -> AppleVerification -> Text -> Either AppleTokenError AppleIdentity
verifyWith key config = verifyAppleIdentityTokenWithKeys config lookupKey
  where
    lookupKey kid = if kid == skKid key then Just (skPublic key) else Nothing

spec :: Spec
spec = describe "Sign in with Apple identity token verification" $ do
  key <- runIO (newSigningKey "apple-kid-1")

  describe "verifyAppleIdentityTokenWithKeys" $ do
    it "accepts a token signed by a known key with matching issuer, audience and lifetime" $ do
      token <- tokenWith key baseClaims
      verifyWith key verification token
        `shouldBe` Right
          AppleIdentity
            { aiSubject = "001234.abcdef1234567890abcdef1234567890.1234"
            , aiEmail = Just "relay-abc123@privaterelay.appleid.com"
            , aiEmailVerified = True
            , aiIsPrivateEmail = True
            }

    it "accepts boolean claim encodings and audience arrays" $ do
      token <- tokenWith key (withClaims
        [ ("aud", Array (pure (String audience)))
        , ("email_verified", Bool True)
        , ("is_private_email", Bool False)
        ])
      fmap aiIsPrivateEmail (verifyWith key verification token) `shouldBe` Right False

    it "reports a missing email as Nothing rather than failing" $ do
      token <- tokenWith key (withoutClaim "email" (withoutClaim "email_verified" baseClaims))
      fmap aiEmail (verifyWith key verification token) `shouldBe` Right Nothing
      fmap aiEmailVerified (verifyWith key verification token) `shouldBe` Right False

    it "rejects a signature produced by a different key" $ do
      rogue <- newSigningKey (skKid key)
      token <- tokenWith rogue baseClaims
      verifyWith key verification token `shouldBe` Left AppleTokenBadSignature

    it "rejects tampered claims even when the signature was valid for the original claims" $ do
      token <- tokenWith key baseClaims
      case T.splitOn "." token of
        [header, _, signature] -> do
          let tampered = header <> "." <> encodeSegment (claimsObject (withClaims [("sub", String "attacker")])) <> "." <> signature
          verifyWith key verification tampered `shouldBe` Left AppleTokenBadSignature
        parts -> expectationFailure ("Expected three JWT segments, got " <> show (length parts))

    it "rejects the wrong issuer" $ do
      token <- tokenWith key (withClaims [("iss", String "https://accounts.google.com")])
      verifyWith key verification token `shouldBe` Left AppleTokenWrongIssuer

    it "rejects audiences this deployment does not accept" $ do
      token <- tokenWith key (withClaims [("aud", String "com.example.other")])
      verifyWith key verification token `shouldBe` Left AppleTokenWrongAudience
      arrayToken <- tokenWith key (withClaims [("aud", Array (pure (String "com.example.other")))])
      verifyWith key verification arrayToken `shouldBe` Left AppleTokenWrongAudience
      validToken <- tokenWith key baseClaims
      verifyWith key verification { avAudiences = [] } validToken `shouldBe` Left AppleTokenWrongAudience

    it "rejects expired tokens beyond the 60 second skew and accepts tokens inside it" $ do
      expired <- tokenWith key (withClaims [("exp", Number (fromInteger (epochSeconds now - 61)))])
      verifyWith key verification expired `shouldBe` Left AppleTokenExpired
      boundary <- tokenWith key (withClaims [("exp", Number (fromInteger (epochSeconds now - 60)))])
      verifyWith key verification boundary `shouldBe` Left AppleTokenExpired
      inSkew <- tokenWith key (withClaims [("exp", Number (fromInteger (epochSeconds now - 59)))])
      verifyWith key verification inSkew `shouldSatisfy` either (const False) (const True)

    it "rejects tokens issued in the future beyond the skew" $ do
      future <- tokenWith key (withClaims [("iat", Number (fromInteger (epochSeconds now + 61)))])
      verifyWith key verification future `shouldBe` Left AppleTokenNotYetValid

    it "requires the raw nonce to hash to the token nonce" $ do
      token <- tokenWith key (withClaims [("nonce", String (appleNonceDigest "client-nonce-1"))])
      verifyWith key verification { avExpectedNonce = Just "client-nonce-1" } token
        `shouldSatisfy` either (const False) (const True)
      verifyWith key verification { avExpectedNonce = Just "client-nonce-2" } token
        `shouldBe` Left AppleTokenNonceMismatch
      verifyWith key verification token `shouldBe` Left AppleTokenNonceRequired
      plain <- tokenWith key baseClaims
      verifyWith key verification { avExpectedNonce = Just "client-nonce-1" } plain
        `shouldBe` Left AppleTokenNonceMismatch

    it "rejects a token whose header names an unknown key id" $ do
      other <- newSigningKey "apple-kid-unknown"
      token <- tokenWith other baseClaims
      verifyWith key verification token `shouldBe` Left (AppleTokenUnknownKey "apple-kid-unknown")

    it "rejects alg=none and every non-RS256 algorithm before looking at the signature" $ do
      let noneHeader = object ["alg" .= ("none" :: Text), "kid" .= skKid key]
          noneToken = encodeSegment noneHeader <> "." <> encodeSegment (claimsObject baseClaims) <> ".x"
      verifyWith key verification noneToken `shouldBe` Left (AppleTokenUnsupportedAlgorithm "none")
      hsToken <- signToken key (object ["alg" .= ("HS256" :: Text), "kid" .= skKid key]) (claimsObject baseClaims)
      verifyWith key verification hsToken `shouldBe` Left (AppleTokenUnsupportedAlgorithm "HS256")

    it "fails closed on malformed tokens" $ do
      verifyWith key verification "" `shouldSatisfy` isMalformed
      verifyWith key verification "a.b" `shouldSatisfy` isMalformed
      verifyWith key verification "a.b.c.d" `shouldSatisfy` isMalformed
      verifyWith key verification "not+base64.b.c" `shouldSatisfy` isMalformed
      verifyWith key verification (b64 "{\"alg\":\"RS256\"}" <> ".b.c") `shouldSatisfy` isMalformed
      verifyWith key verification (b64 "[]" <> ".b.c") `shouldSatisfy` isMalformed
      brokenClaims <- signToken key (rs256Header (skKid key)) (String "claims")
      verifyWith key verification brokenClaims `shouldSatisfy` isMalformed
      missingExp <- tokenWith key (withoutClaim "exp" baseClaims)
      verifyWith key verification missingExp `shouldSatisfy` isMalformed
      stringExp <- tokenWith key (withClaims [("exp", String "4102444800")])
      verifyWith key verification stringExp `shouldSatisfy` isMalformed
      oddFlag <- tokenWith key (withClaims [("email_verified", String "yes")])
      verifyWith key verification oddFlag `shouldSatisfy` isMalformed

    it "rejects blank or unsafe subjects" $ do
      blank <- tokenWith key (withClaims [("sub", String "   ")])
      verifyWith key verification blank `shouldBe` Left AppleTokenInvalidSubject
      spaced <- tokenWith key (withClaims [("sub", String "apple sub")])
      verifyWith key verification spaced `shouldBe` Left AppleTokenInvalidSubject
      long <- tokenWith key (withClaims [("sub", String (T.replicate 256 "a"))])
      verifyWith key verification long `shouldBe` Left AppleTokenInvalidSubject

  describe "JWKS handling" $ do
    it "decodes Apple's key document and converts RSA signing keys" $ do
      let document = BL.toStrict (encode (object
            [ "keys" .=
                [ object
                    [ "kty" .= ("RSA" :: Text), "kid" .= skKid key, "use" .= ("sig" :: Text)
                    , "alg" .= ("RS256" :: Text), "n" .= ajN (jwkFor key), "e" .= ajE (jwkFor key)
                    ]
                ]
            ]))
      case decodeAppleJwks document of
        Left err -> expectationFailure (T.unpack err)
        Right (AppleJwks keys) -> do
          map ajKid keys `shouldBe` [skKid key]
          fmap RSA.public_n (appleJwkPublicKey (head keys)) `shouldBe` Right (RSA.public_n (skPublic key))
          fmap RSA.public_e (appleJwkPublicKey (head keys)) `shouldBe` Right 65537
      decodeAppleJwks "{\"keys\":\"nope\"}" `shouldSatisfy` isLeft
      appleJwkPublicKey (jwkFor key) { ajKty = "EC" } `shouldSatisfy` isLeft
      appleJwkPublicKey (jwkFor key) { ajUse = Just "enc" } `shouldSatisfy` isLeft
      appleJwkPublicKey (jwkFor key) { ajN = "!!" } `shouldSatisfy` isLeft

    it "decodes base64url strictly" $ do
      base64UrlDecode (b64 "hello") `shouldBe` Right "hello"
      base64UrlDecode "" `shouldSatisfy` isLeft
      base64UrlDecode "aGVsbG8=" `shouldSatisfy` isLeft
      base64UrlDecode "aGVs bG8" `shouldSatisfy` isLeft

    it "reads the header key id before any fetch decision" $ do
      token <- tokenWith key baseClaims
      parseAppleTokenHeaderKid token `shouldBe` Right (skKid key)
      parseAppleTokenHeaderKid "x.y" `shouldSatisfy` isLeft

  describe "verifyAppleIdentityToken (cached keys, injected fetch)" $ do
    it "fetches keys once, serves cached keys, and refetches on an unknown kid only after the cooldown" $ do
      rotated <- newSigningKey "apple-kid-2"
      calls <- newIORef (0 :: Int)
      published <- newIORef [jwkFor key]
      cache <- newAppleKeyCache
      let fetchKeys = do
            modifyIORef' calls (+ 1)
            AppleJwks <$> readIORef published
      first <- tokenWith key baseClaims
      verifyAppleIdentityToken cache (Right <$> fetchKeys) verification first
        >>= (`shouldSatisfy` either (const False) (const True))
      readIORef calls `shouldReturn` 1
      verifyAppleIdentityToken cache (Right <$> fetchKeys) verification first
        >>= (`shouldSatisfy` either (const False) (const True))
      readIORef calls `shouldReturn` 1

      -- Apple rotates its keys: the new kid is unknown, but a refetch within
      -- the cooldown is refused so forged kids cannot amplify requests.
      modifyIORef' published (jwkFor rotated :)
      second <- tokenWith rotated baseClaims
      verifyAppleIdentityToken cache (Right <$> fetchKeys) verification second
        `shouldReturn` Left (AppleTokenUnknownKey "apple-kid-2")
      readIORef calls `shouldReturn` 1

      let later = verification { avNow = addUTCTime 61 now }
          laterClaims = withClaims [("exp", Number (fromInteger (epochSeconds now + 700)))]
      secondLater <- tokenWith rotated laterClaims
      verifyAppleIdentityToken cache (Right <$> fetchKeys) later secondLater
        >>= (`shouldSatisfy` either (const False) (const True))
      readIORef calls `shouldReturn` 2

    it "fails closed when the key document cannot be fetched or holds no usable keys" $ do
      cache <- newAppleKeyCache
      token <- tokenWith key baseClaims
      verifyAppleIdentityToken cache (pure (Left "offline")) verification token
        `shouldReturn` Left (AppleTokenKeysUnavailable "offline")
      verifyAppleIdentityToken cache (pure (Right (AppleJwks []))) verification token
        `shouldReturn` Left (AppleTokenKeysUnavailable "no usable RSA keys")
      verifyAppleIdentityToken cache (pure (Right (AppleJwks [(jwkFor key) { ajKty = "EC" }]))) verification token
        `shouldReturn` Left (AppleTokenKeysUnavailable "no usable RSA keys")

    it "never fetches for tokens whose header cannot be parsed" $ do
      cache <- newAppleKeyCache
      calls <- newIORef (0 :: Int)
      let fetchKeys = modifyIORef' calls (+ 1) >> pure (Right (AppleJwks [jwkFor key]))
      verifyAppleIdentityToken cache fetchKeys verification "garbage"
        >>= (`shouldSatisfy` isMalformed)
      readIORef calls `shouldReturn` 0

    it "still verifies signatures with cached keys from a map lookup" $ do
      token <- tokenWith key baseClaims
      let keys = Map.fromList [(skKid key, skPublic key)]
      verifyAppleIdentityTokenWithKeys verification (`Map.lookup` keys) token
        `shouldSatisfy` either (const False) (const True)
  where
    isMalformed (Left (AppleTokenMalformed _)) = True
    isMalformed _ = False
