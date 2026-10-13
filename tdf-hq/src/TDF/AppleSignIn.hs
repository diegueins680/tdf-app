{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Sign in with Apple identity-token verification.
--
-- Apple returns an RS256-signed JWT from AuthenticationServices. There is no
-- token-introspection endpoint, so the backend verifies the signature itself
-- against Apple's published JWKS (https://appleid.apple.com/auth/keys).
--
-- Every parsing problem fails closed: a token that cannot be decoded, whose
-- header does not name a known RSA key, whose signature does not verify, or
-- whose claims do not match this deployment is rejected before any database
-- effect. JWKS retrieval is injected so tests never touch the network.
module TDF.AppleSignIn
  ( AppleIdentity (..)
  , AppleTokenError (..)
  , AppleVerification (..)
  , AppleJwk (..)
  , AppleJwks (..)
  , AppleKeyCache
  , AppleJwksFetch
  , appleIssuer
  , defaultAppleJwksUrl
  , defaultAppleAudiences
  , appleNonceDigest
  , newAppleKeyCache
  , verifyAppleIdentityToken
  , verifyAppleIdentityTokenWithKeys
  , fetchAppleJwks
  , decodeAppleJwks
  , appleJwkPublicKey
  , parseAppleTokenHeaderKid
  , base64UrlDecode
  , describeAppleTokenError
  ) where

import Control.Exception.Safe (SomeException, try)
import Crypto.Hash (Digest, SHA256 (..), hashWith)
import Crypto.Number.Serialize (os2ip)
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Crypto.PubKey.RSA.Types (PublicKey (..))
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict', withObject, (.:), (.:?))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii, isControl, isSpace)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (Manager, Request, Response, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)

-- | Issuer claim Apple places in every identity token.
appleIssuer :: Text
appleIssuer = "https://appleid.apple.com"

-- | Apple's public signing keys.
defaultAppleJwksUrl :: Text
defaultAppleJwksUrl = "https://appleid.apple.com/auth/keys"

-- | The iOS bundle identifier is the audience for native Sign in with Apple.
defaultAppleAudiences :: [Text]
defaultAppleAudiences = ["com.tdfrecords.app"]

-- | Claims the backend trusts after verification.
data AppleIdentity = AppleIdentity
  { aiSubject :: Text
  , aiEmail :: Maybe Text
  , aiEmailVerified :: Bool
  , aiIsPrivateEmail :: Bool
  } deriving (Eq, Show)

-- | Precise rejection reasons. They are logged or asserted in tests; the HTTP
-- layer collapses them into one generic 401 so callers cannot probe verification.
data AppleTokenError
  = AppleTokenMalformed Text
  | AppleTokenUnsupportedAlgorithm Text
  | AppleTokenUnknownKey Text
  | AppleTokenBadSignature
  | AppleTokenWrongIssuer
  | AppleTokenWrongAudience
  | AppleTokenExpired
  | AppleTokenNotYetValid
  | AppleTokenNonceMismatch
  | AppleTokenNonceRequired
  | AppleTokenInvalidSubject
  | AppleTokenKeysUnavailable Text
  deriving (Eq, Show)

describeAppleTokenError :: AppleTokenError -> Text
describeAppleTokenError err = case err of
  AppleTokenMalformed detail -> "malformed token: " <> detail
  AppleTokenUnsupportedAlgorithm alg -> "unsupported algorithm: " <> alg
  AppleTokenUnknownKey kid -> "unknown signing key: " <> kid
  AppleTokenBadSignature -> "signature verification failed"
  AppleTokenWrongIssuer -> "issuer mismatch"
  AppleTokenWrongAudience -> "audience mismatch"
  AppleTokenExpired -> "token expired"
  AppleTokenNotYetValid -> "token issued in the future"
  AppleTokenNonceMismatch -> "nonce mismatch"
  AppleTokenNonceRequired -> "token carries a nonce but the request did not"
  AppleTokenInvalidSubject -> "invalid subject"
  AppleTokenKeysUnavailable detail -> "Apple signing keys unavailable: " <> detail

-- | What this deployment expects from a token.
data AppleVerification = AppleVerification
  { avAudiences :: [Text]
    -- ^ Accepted @aud@ values (bundle identifiers or Services IDs).
  , avNow :: UTCTime
  , avExpectedNonce :: Maybe Text
    -- ^ Raw nonce the client generated. The token must carry its SHA-256 hex digest.
  } deriving (Show)

-- | One RSA key from Apple's JWKS document.
data AppleJwk = AppleJwk
  { ajKid :: Text
  , ajKty :: Text
  , ajAlg :: Maybe Text
  , ajUse :: Maybe Text
  , ajN :: Text
  , ajE :: Text
  } deriving (Eq, Show)

instance FromJSON AppleJwk where
  parseJSON = withObject "AppleJwk" $ \o ->
    AppleJwk
      <$> o .: "kid"
      <*> o .: "kty"
      <*> o .:? "alg"
      <*> o .:? "use"
      <*> o .: "n"
      <*> o .: "e"

newtype AppleJwks = AppleJwks { ajwKeys :: [AppleJwk] }
  deriving (Eq, Show)

instance FromJSON AppleJwks where
  parseJSON = withObject "AppleJwks" $ \o -> AppleJwks <$> o .: "keys"

decodeAppleJwks :: ByteString -> Either Text AppleJwks
decodeAppleJwks raw = either (Left . T.pack) Right (eitherDecodeStrict' raw)

-- | Convert a JWK into an RSA public key. Only RSA signing keys are usable.
appleJwkPublicKey :: AppleJwk -> Either Text PublicKey
appleJwkPublicKey jwk
  | ajKty jwk /= "RSA" = Left "non-RSA key"
  | maybe False (/= "sig") (ajUse jwk) = Left "non-signing key"
  | maybe False (/= "RS256") (ajAlg jwk) = Left "non-RS256 key"
  | otherwise = do
      nBytes <- base64UrlDecode (ajN jwk)
      eBytes <- base64UrlDecode (ajE jwk)
      let modulus = os2ip nBytes
          exponent' = os2ip eBytes
      if BS.null nBytes || BS.null eBytes || modulus <= 1 || exponent' <= 1
        then Left "degenerate RSA parameters"
        else
          Right
            PublicKey
              { public_size = BS.length (BS.dropWhile (== 0) nBytes)
              , public_n = modulus
              , public_e = exponent'
              }

-- | Strict base64url decoding (RFC 7515 section 2: no padding, no whitespace).
base64UrlDecode :: Text -> Either Text ByteString
base64UrlDecode raw
  | T.null raw = Left "empty base64url segment"
  | T.any (not . isBase64UrlChar) raw = Left "invalid base64url character"
  | otherwise = either (const (Left "invalid base64url segment")) Right (B64URL.decodeUnpadded (TE.encodeUtf8 raw))
  where
    isBase64UrlChar ch =
      (ch >= 'a' && ch <= 'z')
        || (ch >= 'A' && ch <= 'Z')
        || (ch >= '0' && ch <= '9')
        || ch == '-'
        || ch == '_'

-- | Lowercase hex SHA-256 of the raw client nonce, as Apple embeds it.
appleNonceDigest :: Text -> Text
appleNonceDigest rawNonce =
  T.pack (show (hashWith SHA256 (TE.encodeUtf8 rawNonce) :: Digest SHA256))

data AppleHeader = AppleHeader
  { ahAlg :: Text
  , ahKid :: Text
  }

parseAppleHeader :: Text -> Either AppleTokenError AppleHeader
parseAppleHeader segment = do
  raw <- either (Left . AppleTokenMalformed) Right (base64UrlDecode segment)
  value <- either (const (Left (AppleTokenMalformed "header is not JSON"))) Right (eitherDecodeStrict' raw)
  case value of
    Object o -> do
      alg <- requireText "alg" o
      kid <- requireText "kid" o
      if alg /= "RS256"
        then Left (AppleTokenUnsupportedAlgorithm alg)
        else
          if T.null kid || T.length kid > 256 || T.any (\c -> not (isAscii c) || isSpace c || isControl c) kid
            then Left (AppleTokenMalformed "invalid kid")
            else Right AppleHeader { ahAlg = alg, ahKid = kid }
    _ -> Left (AppleTokenMalformed "header is not an object")
  where
    requireText key o = case KeyMap.lookup key o of
      Just (String t) -> Right t
      Just _ -> Left (AppleTokenMalformed (T.pack (show key) <> " is not a string"))
      Nothing -> Left (AppleTokenMalformed (T.pack (show key) <> " missing"))

-- | Key identifier named by the token header; used to decide whether the JWKS
-- cache already holds the signing key before any fetch.
parseAppleTokenHeaderKid :: Text -> Either AppleTokenError Text
parseAppleTokenHeaderKid token = do
  (headerSegment, _, _) <- splitToken token
  ahKid <$> parseAppleHeader headerSegment

data AppleClaims = AppleClaims
  { acIss :: Text
  , acAud :: [Text]
  , acSub :: Text
  , acExp :: Integer
  , acIat :: Integer
  , acNonce :: Maybe Text
  , acEmail :: Maybe Text
  , acEmailVerified :: Bool
  , acIsPrivateEmail :: Bool
  }

parseAppleClaims :: Text -> Either AppleTokenError AppleClaims
parseAppleClaims segment = do
  raw <- either (Left . AppleTokenMalformed) Right (base64UrlDecode segment)
  value <- either (const (Left (AppleTokenMalformed "claims are not JSON"))) Right (eitherDecodeStrict' raw)
  case value of
    Object o -> do
      iss <- requiredText "iss" o
      aud <- audience o
      sub <- requiredText "sub" o
      expiry <- requiredInteger "exp" o
      issuedAt <- requiredInteger "iat" o
      nonce <- optionalText "nonce" o
      email <- optionalText "email" o
      emailVerified <- optionalFlag "email_verified" o
      privateEmail <- optionalFlag "is_private_email" o
      Right
        AppleClaims
          { acIss = iss
          , acAud = aud
          , acSub = sub
          , acExp = expiry
          , acIat = issuedAt
          , acNonce = nonce
          , acEmail = email
          , acEmailVerified = fromMaybe False emailVerified
          , acIsPrivateEmail = fromMaybe False privateEmail
          }
    _ -> Left (AppleTokenMalformed "claims are not an object")
  where
    malformed key detail = Left (AppleTokenMalformed (T.pack (show key) <> " " <> detail))
    requiredText key o = case KeyMap.lookup key o of
      Just (String t) -> Right t
      Just _ -> malformed key "is not a string"
      Nothing -> malformed key "missing"
    optionalText key o = case KeyMap.lookup key o of
      Nothing -> Right Nothing
      Just Null -> Right Nothing
      Just (String t) -> Right (Just t)
      Just _ -> malformed key "is not a string"
    requiredInteger key o = case KeyMap.lookup key o of
      Just (Number n) -> case floatingOrInteger n of
        Right (i :: Integer) -> Right i
        Left (_ :: Double) -> malformed key "is not an integer"
      Just _ -> malformed key "is not a number"
      Nothing -> malformed key "missing"
    optionalFlag key o = case KeyMap.lookup key o of
      Nothing -> Right Nothing
      Just Null -> Right Nothing
      Just (Bool b) -> Right (Just b)
      Just (String t) -> case T.toLower (T.strip t) of
        "true" -> Right (Just True)
        "false" -> Right (Just False)
        _ -> malformed key "is not a boolean"
      Just _ -> malformed key "is not a boolean"
    audience o = case KeyMap.lookup "aud" o of
      Just (String t) -> Right [t]
      Just (Array items)
        | not (null items)
        , Just strings <- traverse asString (toList items) -> Right strings
      Just _ -> malformed ("aud" :: Text) "is not a string or string array"
      Nothing -> malformed ("aud" :: Text) "missing"
    asString (String t) = Just t
    asString _ = Nothing

splitToken :: Text -> Either AppleTokenError (Text, Text, Text)
splitToken token =
  case T.splitOn "." token of
    [h, p, s]
      | not (T.null h) && not (T.null p) && not (T.null s) -> Right (h, p, s)
    parts -> Left (AppleTokenMalformed ("expected 3 segments, found " <> T.pack (show (length parts))))

-- | Clock skew tolerated for @exp@ and @iat@.
allowedClockSkewSeconds :: Integer
allowedClockSkewSeconds = 60

maxAppleSubjectChars :: Int
maxAppleSubjectChars = 255

-- | Pure verification against an in-memory key lookup. The lookup receives the
-- header @kid@; returning 'Nothing' rejects the token.
verifyAppleIdentityTokenWithKeys
  :: AppleVerification
  -> (Text -> Maybe PublicKey)
  -> Text
  -> Either AppleTokenError AppleIdentity
verifyAppleIdentityTokenWithKeys verification lookupKey token = do
  (headerSegment, claimsSegment, signatureSegment) <- splitToken token
  header <- parseAppleHeader headerSegment
  publicKey <- maybe (Left (AppleTokenUnknownKey (ahKid header))) Right (lookupKey (ahKid header))
  signature <- either (Left . AppleTokenMalformed) Right (base64UrlDecode signatureSegment)
  let signingInput = TE.encodeUtf8 (headerSegment <> "." <> claimsSegment)
  if not (PKCS15.verify (Just SHA256) publicKey signingInput signature)
    then Left AppleTokenBadSignature
    else do
      claims <- parseAppleClaims claimsSegment
      validateAppleClaims verification claims

validateAppleClaims :: AppleVerification -> AppleClaims -> Either AppleTokenError AppleIdentity
validateAppleClaims AppleVerification { avAudiences, avNow, avExpectedNonce } claims
  | acIss claims /= appleIssuer = Left AppleTokenWrongIssuer
  | null avAudiences || not (any (`elem` avAudiences) (acAud claims)) = Left AppleTokenWrongAudience
  | acExp claims + allowedClockSkewSeconds <= nowSeconds = Left AppleTokenExpired
  | acIat claims > nowSeconds + allowedClockSkewSeconds = Left AppleTokenNotYetValid
  | T.null subject
      || T.length subject > maxAppleSubjectChars
      || T.any invalidSubjectChar subject = Left AppleTokenInvalidSubject
  | otherwise = do
      checkNonce
      Right
        AppleIdentity
          { aiSubject = subject
          , aiEmail = cleanEmail (acEmail claims)
          , aiEmailVerified = acEmailVerified claims
          , aiIsPrivateEmail = acIsPrivateEmail claims
          }
  where
    nowSeconds = floor (utcTimeToPOSIXSeconds avNow) :: Integer
    subject = T.strip (acSub claims)
    invalidSubjectChar ch = not (isAscii ch) || isSpace ch || isControl ch
    cleanEmail mEmail = do
      email <- T.strip <$> mEmail
      if T.null email then Nothing else Just email
    checkNonce = case (avExpectedNonce, acNonce claims) of
      (Nothing, Nothing) -> Right ()
      (Nothing, Just _) -> Left AppleTokenNonceRequired
      (Just _, Nothing) -> Left AppleTokenNonceMismatch
      (Just rawNonce, Just tokenNonce) ->
        let expected = TE.encodeUtf8 (appleNonceDigest rawNonce)
            actual = TE.encodeUtf8 (T.toLower (T.strip tokenNonce))
        in if BS.length expected == BS.length actual && BA.constEq expected actual
             then Right ()
             else Left AppleTokenNonceMismatch

-- | Process-wide cache of Apple's signing keys keyed by @kid@.
data AppleKeyCacheState = AppleKeyCacheState
  { akcKeys :: Map.Map Text PublicKey
  , akcLastFetch :: Maybe UTCTime
  }

newtype AppleKeyCache = AppleKeyCache (IORef AppleKeyCacheState)

newAppleKeyCache :: IO AppleKeyCache
newAppleKeyCache = AppleKeyCache <$> newIORef AppleKeyCacheState { akcKeys = Map.empty, akcLastFetch = Nothing }

-- | Injected JWKS retrieval; production uses 'fetchAppleJwks'.
type AppleJwksFetch = IO (Either Text AppleJwks)

-- | Minimum spacing between JWKS refetches triggered by unknown key ids, so a
-- flood of forged tokens cannot turn the login endpoint into a request amplifier.
minimumRefetchIntervalSeconds :: Double
minimumRefetchIntervalSeconds = 60

-- | Verify a token, consulting the cache first and refetching the JWKS at most
-- once when the header names a key the cache does not hold (Apple rotates keys).
verifyAppleIdentityToken
  :: AppleKeyCache
  -> AppleJwksFetch
  -> AppleVerification
  -> Text
  -> IO (Either AppleTokenError AppleIdentity)
verifyAppleIdentityToken (AppleKeyCache ref) fetchKeys verification token =
  case parseAppleTokenHeaderKid token of
    Left err -> pure (Left err)
    Right kid -> do
      state <- readIORef ref
      keys <-
        if Map.member kid (akcKeys state)
          then pure (Right (akcKeys state))
          else refresh state
      pure $ case keys of
        Left err -> Left err
        Right available -> verifyAppleIdentityTokenWithKeys verification (`Map.lookup` available) token
  where
    refresh state =
      let recentlyFetched = case akcLastFetch state of
            Just previous -> realToFrac (diffUTCTime (avNow verification) previous) < minimumRefetchIntervalSeconds
            Nothing -> False
      in if recentlyFetched && not (Map.null (akcKeys state))
           then pure (Right (akcKeys state))
           else do
             fetched <- fetchKeys
             case fetched of
               Left detail -> pure (Left (AppleTokenKeysUnavailable detail))
               Right (AppleJwks jwks) -> do
                 let usable = Map.fromList [(ajKid jwk, key) | jwk <- jwks, Right key <- [appleJwkPublicKey jwk]]
                 if Map.null usable
                   then pure (Left (AppleTokenKeysUnavailable "no usable RSA keys"))
                   else do
                     atomicModifyIORef' ref $ \current ->
                       ( current { akcKeys = usable, akcLastFetch = Just (avNow verification) }
                       , () )
                     pure (Right usable)

-- | Production JWKS retrieval over HTTPS.
fetchAppleJwks :: Manager -> Text -> AppleJwksFetch
fetchAppleJwks manager url = do
  requestResult <- try (parseRequest (T.unpack url)) :: IO (Either SomeException Request)
  case requestResult of
    Left _ -> pure (Left "invalid JWKS URL")
    Right request -> do
      responseResult <- try (httpLbs request manager) :: IO (Either SomeException (Response BL.ByteString))
      case responseResult of
        Left _ -> pure (Left "JWKS request failed")
        Right response ->
          let status = statusCode (responseStatus response)
              body = BL.toStrict (responseBody response)
          in if status /= 200
               then pure (Left ("JWKS request returned " <> T.pack (show status)))
               else
                 if BS.length body > maxJwksBytes
                   then pure (Left "JWKS document too large")
                   else pure (decodeAppleJwks body)
  where
    maxJwksBytes = 65536
