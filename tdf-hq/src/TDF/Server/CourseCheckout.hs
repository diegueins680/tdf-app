{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.CourseCheckout
  ( createCourseCheckoutRegistration
  , getPublicCourseCheckout
  , createPublicCourseDatafastCheckout
  , confirmPublicCourseDatafastStatus
  , createPublicCoursePaypalOrder
  , capturePublicCoursePaypalOrder
  ) where

import           Control.Exception
  ( SomeAsyncException, SomeException, fromException, throwIO, try )
import           Control.Monad (unless, void, when)
import           Control.Monad.Except (catchError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Aeson (encode, object, (.=))
import           Data.ByteArray (constEq)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAlphaNum, isAscii, isControl, isSpace)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, addUTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool
  , toSqlKey
  )
import           Database.PostgreSQL.Simple (SqlError(..))
import           Servant
import           System.Environment (lookupEnv)

import qualified TDF.API.Types as APITypes
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.CourseCheckout as CourseDomain
import           TDF.DB (Env(..), sharedTlsManager)
import qualified TDF.Internationalization as Internationalization
import qualified TDF.ModelsExtra as ME
import qualified TDF.Routes.Courses as Courses
import qualified TDF.Server.ServiceStorefront as ServiceStorefront
import qualified TDF.Trials.Models as Trials

type AppM = ReaderT Env Handler

data ApprovedCourseCheckoutPolicy = ApprovedCourseCheckoutPolicy
  { accpId              :: Text
  , accpVersion         :: Text
  , accpCurrency        :: Text
  , accpPriceMinor      :: Int64
  , accpTaxBps          :: Int
  , accpPaymentMode     :: CourseDomain.CoursePaymentMode
  , accpPaymentModeText :: Text
  , accpDepositBps      :: Int
  , accpHoldMinutes     :: Int
  , accpTermsVersion    :: Text
  } deriving (Eq, Show)

data CourseCheckoutRuntimeView = CourseCheckoutRuntimeView
  { ccrvRegistrationId   :: Int64
  , ccrvCourseSlug       :: Text
  , ccrvCheckoutId       :: Text
  , ccrvPaymentStatus    :: Text
  , ccrvEnrollmentStatus :: Text
  , ccrvHoldExpiresAt    :: UTCTime
  , ccrvPolicyVersion    :: Text
  , ccrvCurrency         :: Text
  , ccrvPriceMinor       :: Int64
  , ccrvTaxMinor         :: Int64
  , ccrvTotalMinor       :: Int64
  , ccrvDueNowMinor      :: Int64
  , ccrvBalanceMinor     :: Int64
  , ccrvPaymentSchedule  :: Text
  , ccrvTermsVersion     :: Text
  } deriving (Eq, Show)

data CoursePaymentContext = CoursePaymentContext
  { cpcRegistrationKey     :: ME.CourseRegistrationId
  , cpcCourseSlug          :: Text
  , cpcCheckout            :: Checkout.CheckoutReference
  , cpcCreateIdempotencyKey :: Text
  , cpcCheckoutStatus      :: Text
  , cpcEnvironment         :: Checkout.CheckoutEnvironment
  , cpcDueNowMinor         :: Int64
  , cpcCurrency            :: Text
  , cpcHoldExpiresAt       :: UTCTime
  , cpcBuyerName           :: Text
  , cpcBuyerEmail          :: Text
  , cpcBuyerPhone          :: Maybe Text
  } deriving (Eq, Show)

loadCheckoutEnvironment :: AppM Checkout.CheckoutEnvironment
loadCheckoutEnvironment = do
  rawEnvironment <- liftIO (lookupEnv "COMMERCE_CHECKOUT_ENV")
  either (throwError . internalError) pure $
    Checkout.resolveCheckoutEnvironment rawEnvironment

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  Env{ envPool } <- ask
  liftIO (runSqlPool action envPool)

internalError :: Text -> ServerError
internalError message = err500 { errBody = textBody message }

conflictError :: Text -> ServerError
conflictError message = err409 { errBody = textBody message }

badRequestError :: Text -> ServerError
badRequestError message = err400 { errBody = textBody message }

textBody :: Text -> BL.ByteString
textBody = BL.fromStrict . TE.encodeUtf8

sha256Text :: Text -> Text
sha256Text value = TE.decodeUtf8 $
  BAE.convertToBase BAE.Base16
    (hash (TE.encodeUtf8 value) :: Digest SHA256)

normalizeSlug :: Text -> Either ServerError Text
normalizeSlug raw
  | T.null clean = Left (badRequestError "course slug is required")
  | T.length clean > 120 = Left (badRequestError "course slug is too long")
  | T.any (\char -> not (isAlphaNum char || char == '-')) clean =
      Left (badRequestError "course slug contains unsupported characters")
  | otherwise = Right clean
  where
    clean = T.toLower (T.strip raw)

normalizeRequiredText :: Text -> Int -> Maybe Text -> Either ServerError Text
normalizeRequiredText fieldName maxLength raw =
  case T.strip <$> raw of
    Nothing -> Left (badRequestError (fieldName <> " is required"))
    Just clean
      | T.null clean -> Left (badRequestError (fieldName <> " is required"))
      | T.length clean > maxLength ->
          Left (badRequestError (fieldName <> " is too long"))
      | T.any unsafeTextCharacter clean ->
          Left (badRequestError (fieldName <> " contains unsupported characters"))
      | otherwise -> Right clean

normalizeOptionalText :: Text -> Int -> Maybe Text -> Either ServerError (Maybe Text)
normalizeOptionalText fieldName maxLength raw = case T.strip <$> raw of
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just clean
    | T.length clean > maxLength ->
        Left (badRequestError (fieldName <> " is too long"))
    | T.any unsafeTextCharacter clean ->
        Left (badRequestError (fieldName <> " contains unsupported characters"))
    | otherwise -> Right (Just clean)

unsafeTextCharacter :: Char -> Bool
unsafeTextCharacter char =
  isControl char || (not (isAscii char) && isSpace char)

normalizeEmail :: Maybe Text -> Either ServerError Text
normalizeEmail raw = do
  clean <- T.toLower <$> normalizeRequiredText "email" 254 raw
  let (localPart, domainWithAt) = T.breakOn "@" clean
      domain = T.drop 1 domainWithAt
  if T.null localPart
      || T.null domainWithAt
      || T.null domain
      || T.count "@" clean /= 1
      || not ("." `T.isInfixOf` domain)
      || T.any isSpace clean
    then Left (badRequestError "email is invalid")
    else Right clean

normalizeSource :: Text -> Either ServerError Text
normalizeSource raw
  | T.null clean || T.length clean > 40 =
      Left (badRequestError "registration source is invalid")
  | T.any (\char -> not (isAlphaNum char || char `elem` ['_','-'])) clean =
      Left (badRequestError "registration source is invalid")
  | otherwise = Right clean
  where
    clean = T.toLower (T.strip raw)

loadApprovedCourseCheckoutPolicy
  :: UTCTime
  -> Trials.CourseId
  -> SqlPersistT IO (Maybe ApprovedCourseCheckoutPolicy)
loadApprovedCourseCheckoutPolicy now courseKey = do
  rows <- (rawSql
    "SELECT policy.id::text, policy.policy_version, policy.currency,\
    \ policy.price_minor, policy.tax_bps, policy.payment_mode, policy.deposit_bps,\
    \ policy.hold_minutes, policy.terms_version\
    \ FROM course_checkout_policy policy\
    \ JOIN course course ON course.id = policy.course_id\
    \ WHERE policy.course_id = ? AND policy.active\
    \ AND policy.approval_status = 'approved'\
    \ AND policy.approved_at IS NOT NULL AND policy.approved_by IS NOT NULL\
    \ AND (policy.effective_from IS NULL OR policy.effective_from <= ?)\
    \ AND (policy.effective_until IS NULL OR policy.effective_until > ?)\
    \ AND policy.price_minor = course.price_cents\
    \ AND policy.currency = upper(course.currency)"
    [toPersistValue courseKey, PersistUTCTime now, PersistUTCTime now]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Int64, Single Int
       , Single Text, Single Int, Single Int, Single Text
       )])
  pure $ case rows of
    [( Single policyId, Single version, Single currency, Single priceMinor
     , Single taxBps, Single paymentModeText, Single depositBps
     , Single holdMinutes, Single termsVersion
     )] -> do
      paymentMode <- case paymentModeText of
        "full" -> Just CourseDomain.CourseFullPayment
        "deposit" -> Just CourseDomain.CourseDeposit
        _ -> Nothing
      Just ApprovedCourseCheckoutPolicy
        { accpId = policyId
        , accpVersion = version
        , accpCurrency = currency
        , accpPriceMinor = priceMinor
        , accpTaxBps = taxBps
        , accpPaymentMode = paymentMode
        , accpPaymentModeText = paymentModeText
        , accpDepositBps = depositBps
        , accpHoldMinutes = holdMinutes
        , accpTermsVersion = termsVersion
        }
    _ -> Nothing

courseCheckoutUnavailableResponse
  :: Text
  -> Courses.CourseRegistrationResponse
  -> Courses.CourseCheckoutResponse
courseCheckoutUnavailableResponse slugVal
    (Courses.CourseRegistrationResponse responseId _) =
  Courses.CourseCheckoutResponse
    { Courses.registrationId = responseId
    , Courses.courseSlug = slugVal
    , Courses.checkoutId = Nothing
    , Courses.lookupToken = Nothing
    , Courses.paymentStatus = "not_started"
    , Courses.fulfillmentStatus = "lead_received"
    , Courses.holdExpiresAt = Nothing
    , Courses.quote = Nothing
    , Courses.paymentMethods = []
    , Courses.checkoutAvailable = False
    }

createCourseCheckoutRegistration
  :: (Text -> Courses.CourseRegistrationRequest -> AppM Courses.CourseRegistrationResponse)
  -> Text
  -> Maybe Text
  -> Courses.CourseRegistrationRequest
  -> AppM Courses.CourseCheckoutResponse
createCourseCheckoutRegistration legacyRegistration rawSlug mIdempotency request = do
  let Courses.CourseRegistrationRequest
        _ _ _ registrationSource _ registrationUtm _ = request
  slugVal <- either throwError pure (normalizeSlug rawSlug)
  checkoutEnvironment <- loadCheckoutEnvironment
  domainEnabled <- runDB $
    Checkout.domainEnabledForEnvironment checkoutEnvironment "courses"
  if not domainEnabled
    then courseCheckoutUnavailableResponse slugVal
      <$> legacyRegistration rawSlug request
    else do
      unless (Courses.termsAccepted request == Just True) $
        throwError (badRequestError
          "Course checkout terms must be accepted before a seat can be held")
      idempotencyKey <- either (throwError . badRequestError) pure $
        ServiceStorefront.validateIdempotencyKey mIdempotency
      buyerName <- either throwError pure $
        normalizeRequiredText "fullName" 160 (Courses.fullName request)
      buyerEmail <- either throwError pure (normalizeEmail (Courses.email request))
      buyerPhone <- either throwError pure $
        normalizeOptionalText "phoneE164" 24 (Courses.phoneE164 request)
      sourceClean <- either throwError pure (normalizeSource registrationSource)
      howHeardClean <- either throwError pure $
        normalizeOptionalText "howHeard" 256 (Courses.howHeard request)
      (utmSourceVal, utmMediumVal, utmCampaignVal, utmContentVal) <-
        case registrationUtm of
          Nothing -> pure (Nothing, Nothing, Nothing, Nothing)
          Just (Courses.UTMTags utmSource utmMedium utmCampaign utmContent) ->
            (,,,)
              <$> either throwError pure (cleanUtm "utm.source" utmSource)
              <*> either throwError pure (cleanUtm "utm.medium" utmMedium)
              <*> either throwError pure (cleanUtm "utm.campaign" utmCampaign)
              <*> either throwError pure (cleanUtm "utm.content" utmContent)
      now <- liftIO getCurrentTime
      courseEntity@(Entity courseKey _) <- runDB (getBy (Trials.UniqueCourseSlug slugVal))
        >>= maybe (throwError err404 { errBody = "Course not found" }) pure
      policy <- runDB (loadApprovedCourseCheckoutPolicy now courseKey)
        >>= maybe (throwError (conflictError
              "This course has no approved active checkout price and policy")) pure
      price <- either (throwError . conflictError) pure $
        CourseDomain.calculateCoursePrice
          (accpPriceMinor policy)
          (accpTaxBps policy)
          (accpPaymentMode policy)
          (accpDepositBps policy)
      let requestHash = sha256Text . TE.decodeUtf8 . BL.toStrict . encode $ object
            [ "course_slug" .= slugVal
            , "full_name" .= buyerName
            , "email" .= buyerEmail
            , "phone" .= buyerPhone
            , "source" .= sourceClean
            , "how_heard" .= howHeardClean
            , "utm_source" .= utmSourceVal
            , "utm_medium" .= utmMediumVal
            , "utm_campaign" .= utmCampaignVal
            , "utm_content" .= utmContentVal
            , "policy_id" .= accpId policy
            , "policy_version" .= accpVersion policy
            , "terms_version" .= accpTermsVersion policy
            ]
          lookupToken = sha256Text ("course-order-lookup:" <> idempotencyKey)
          lookupHash = sha256Text lookupToken
          holdExpiresAt = addUTCTime (fromIntegral (accpHoldMinutes policy) * 60) now
      existing <- lookupCourseCheckoutIdempotency idempotencyKey
      case existing of
        Just (registrationKey, storedHash)
          | storedHash == requestHash ->
              loadCourseCheckoutDTO registrationKey (Just lookupToken)
          | otherwise -> throwError (conflictError
              "Idempotency key was already used for a different course checkout")
        Nothing -> do
          result <- createCourseCheckoutTransaction
            checkoutEnvironment now holdExpiresAt idempotencyKey requestHash
            lookupHash buyerName buyerEmail buyerPhone sourceClean howHeardClean
            utmSourceVal utmMediumVal utmCampaignVal utmContentVal
            courseEntity policy price
          registrationKey <- either throwError pure result
          loadCourseCheckoutDTO registrationKey (Just lookupToken)
  where
    cleanUtm fieldName = normalizeOptionalText fieldName 256

lookupCourseCheckoutIdempotency
  :: Text
  -> AppM (Maybe (ME.CourseRegistrationId, Text))
lookupCourseCheckoutIdempotency idempotencyKey = do
  rows <- runDB (rawSql
    "SELECT registration_id, create_request_sha256\
    \ FROM course_registration_checkout_runtime\
    \ WHERE create_idempotency_key = ?"
    [PersistText idempotencyKey]
    :: SqlPersistT IO [(Single Int64, Single Text)])
  pure $ case rows of
    [(Single registrationId, Single requestHash)] ->
      Just (toSqlKey registrationId, requestHash)
    _ -> Nothing

createCourseCheckoutTransaction
  :: Checkout.CheckoutEnvironment
  -> UTCTime
  -> UTCTime
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Entity Trials.Course
  -> ApprovedCourseCheckoutPolicy
  -> CourseDomain.CoursePriceBreakdown
  -> AppM (Either ServerError ME.CourseRegistrationId)
createCourseCheckoutTransaction
    checkoutEnvironment now holdExpiresAt idempotencyKey requestHash lookupHash
    buyerName buyerEmail buyerPhone sourceClean howHeardClean
    utmSourceVal utmMediumVal utmCampaignVal utmContentVal
    (Entity courseKey course) policy price = do
  Env{ envPool } <- ask
  result <- liftIO $
    (try (runSqlPool transactionBody envPool)
      :: IO (Either SomeException (Either ServerError ME.CourseRegistrationId)))
  case result of
    Right value -> pure value
    Left exception -> case fromException exception :: Maybe SomeAsyncException of
      Just _ -> liftIO (throwIO exception)
      Nothing -> case fromException exception :: Maybe SqlError of
        Just sqlError | sqlState sqlError == "23505" -> pure (Left (conflictError
          "This course checkout conflicts with an existing request"))
        _ -> liftIO (throwIO exception)
  where
    transactionBody = do
      _ <- (rawSql
        "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?, 0))) locked"
        [PersistText ("course-checkout:" <> idempotencyKey)]
        :: SqlPersistT IO [Single Int64])
      existing <- (rawSql
        "SELECT registration_id, create_request_sha256\
        \ FROM course_registration_checkout_runtime\
        \ WHERE create_idempotency_key = ?"
        [PersistText idempotencyKey]
        :: SqlPersistT IO [(Single Int64, Single Text)])
      case existing of
        [(Single registrationId, Single storedHash)]
          | storedHash == requestHash -> pure (Right (toSqlKey registrationId))
          | otherwise -> pure (Left (conflictError
              "Idempotency key was already used for a different course checkout"))
        [] -> createNew
        _ -> pure (Left (internalError
          "Course checkout idempotency lookup was ambiguous"))
    createNew = do
      _ <- (rawSql "SELECT id FROM course WHERE id = ? FOR UPDATE"
        [toPersistValue courseKey] :: SqlPersistT IO [Single Int64])
      _ <- (rawSql "SELECT id::text FROM course_checkout_policy WHERE id = ?::uuid FOR SHARE"
        [PersistText (accpId policy)] :: SqlPersistT IO [Single Text])
      _ <- (rawSql "SELECT course_checkout_expire_holds(?, ?)"
        [PersistUTCTime now, toPersistValue courseKey]
        :: SqlPersistT IO [Single Int])
      duplicates <- (rawSql
        "SELECT registration.id\
        \ FROM course_registration registration\
        \ JOIN course_registration_checkout_runtime runtime\
        \   ON runtime.registration_id = registration.id\
        \ WHERE runtime.course_id = ? AND lower(registration.email) = lower(?)\
        \ AND runtime.enrollment_status IN (\
        \   'seat_held','enrolled','transfer_requested','completed'\
        \ ) AND (runtime.enrollment_status <> 'seat_held' OR runtime.hold_expires_at > ?)\
        \ LIMIT 1"
        [toPersistValue courseKey, PersistText buyerEmail, PersistUTCTime now]
        :: SqlPersistT IO [Single Int64])
      if not (null duplicates)
        then pure (Left (conflictError
          "This attendee already has an active seat or enrollment for the course"))
        else do
          occupiedRows <- (rawSql
            "SELECT (\
            \ SELECT count(*) FROM course_registration_checkout_runtime runtime\
            \ WHERE runtime.course_id = ? AND (\
            \   runtime.enrollment_status IN ('enrolled','transfer_requested','completed')\
            \   OR (runtime.enrollment_status = 'seat_held' AND runtime.hold_expires_at > ?)\
            \ )) + (\
            \ SELECT count(*) FROM course_registration registration\
            \ WHERE registration.course_slug = ? AND registration.status = 'paid'\
            \ AND NOT EXISTS (SELECT 1 FROM course_registration_checkout_runtime runtime\
            \   WHERE runtime.registration_id = registration.id))"
            [ toPersistValue courseKey
            , PersistUTCTime now
            , PersistText (Trials.courseSlug course)
            ] :: SqlPersistT IO [Single Int64])
          let occupied = case occupiedRows of
                [Single value] -> value
                _ -> fromIntegral (Trials.courseCapacity course)
          if occupied >= fromIntegral (Trials.courseCapacity course)
            then pure (Left (conflictError "No course seats remain"))
            else createRegistration
    createRegistration = do
      registrationKey <- insert ME.CourseRegistration
        { ME.courseRegistrationCourseSlug = Trials.courseSlug course
        , ME.courseRegistrationPartyId = Nothing
        , ME.courseRegistrationFullName = Just buyerName
        , ME.courseRegistrationEmail = Just buyerEmail
        , ME.courseRegistrationPhoneE164 = buyerPhone
        , ME.courseRegistrationSource = sourceClean
        , ME.courseRegistrationStatus = "pending_payment"
        , ME.courseRegistrationAdminNotes = Nothing
        , ME.courseRegistrationHowHeard = howHeardClean
        , ME.courseRegistrationUtmSource = utmSourceVal
        , ME.courseRegistrationUtmMedium = utmMediumVal
        , ME.courseRegistrationUtmCampaign = utmCampaignVal
        , ME.courseRegistrationUtmContent = utmContentVal
        , ME.courseRegistrationStripePaymentIntentId = Nothing
        , ME.courseRegistrationStripeSubscriptionId = Nothing
        , ME.courseRegistrationSubscriptionStatus = Nothing
        , ME.courseRegistrationCreatedAt = now
        , ME.courseRegistrationUpdatedAt = now
        }
      let registrationIdText = T.pack (show (fromSqlKey registrationKey))
          snapshot = object
            [ "domain" .= ("course_registration" :: Text)
            , "registration_id" .= fromSqlKey registrationKey
            , "course_id" .= fromSqlKey courseKey
            , "course_slug" .= Trials.courseSlug course
            , "course_title" .= Trials.courseTitle course
            , "policy_id" .= accpId policy
            , "policy_version" .= accpVersion policy
            , "price_minor" .= CourseDomain.cpbSubtotalMinor price
            , "tax_bps" .= accpTaxBps policy
            , "tax_minor" .= CourseDomain.cpbTaxMinor price
            , "total_minor" .= CourseDomain.cpbTotalMinor price
            , "due_now_minor" .= CourseDomain.cpbDueNowMinor price
            , "balance_minor" .= CourseDomain.cpbBalanceMinor price
            , "payment_schedule" .= accpPaymentModeText policy
            , "terms_version" .= accpTermsVersion policy
            ]
      checkout <- Checkout.createCheckout Checkout.CheckoutCreation
        { Checkout.ccDomainType = "course_registration"
        , Checkout.ccDomainOrderId = registrationIdText
        , Checkout.ccEnvironment = checkoutEnvironment
        , Checkout.ccCurrency = accpCurrency policy
        , Checkout.ccAmountMinor = CourseDomain.cpbDueNowMinor price
        , Checkout.ccCustomerEmail = buyerEmail
        , Checkout.ccLookupTokenHash = lookupHash
        , Checkout.ccIdempotencyKey = idempotencyKey
        , Checkout.ccExpiresAt = holdExpiresAt
        , Checkout.ccProductType = "course_enrollment"
        , Checkout.ccProductId = T.pack (show (fromSqlKey courseKey))
        , Checkout.ccProductVersion = accpVersion policy
        , Checkout.ccDescription = Trials.courseTitle course
        , Checkout.ccSnapshot = snapshot
        , Checkout.ccCorrelationId = "course-checkout-create:" <> registrationIdText
        }
      rawExecute
        "INSERT INTO course_registration_checkout_runtime(\
        \ registration_id, course_id, checkout_id, policy_id, policy_version,\
        \ lookup_token_hash, create_idempotency_key, create_request_sha256,\
        \ enrollment_status, payment_schedule, payment_status, balance_status,\
        \ currency, price_minor, tax_bps, tax_minor, total_minor, due_now_minor,\
        \ balance_minor, terms_version, terms_accepted_at, hold_expires_at\
        \) VALUES (?, ?, ?::uuid, ?::uuid, ?, ?, ?, ?, 'seat_held', ?,\
        \ 'awaiting_payment', 'not_due', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        [ toPersistValue registrationKey
        , toPersistValue courseKey
        , PersistText (Checkout.checkoutReferenceId checkout)
        , PersistText (accpId policy)
        , PersistText (accpVersion policy)
        , PersistText lookupHash
        , PersistText idempotencyKey
        , PersistText requestHash
        , PersistText (accpPaymentModeText policy)
        , PersistText (accpCurrency policy)
        , PersistInt64 (CourseDomain.cpbSubtotalMinor price)
        , PersistInt64 (fromIntegral (accpTaxBps policy))
        , PersistInt64 (CourseDomain.cpbTaxMinor price)
        , PersistInt64 (CourseDomain.cpbTotalMinor price)
        , PersistInt64 (CourseDomain.cpbDueNowMinor price)
        , PersistInt64 (CourseDomain.cpbBalanceMinor price)
        , PersistText (accpTermsVersion policy)
        , PersistUTCTime now
        , PersistUTCTime holdExpiresAt
        ]
      rawExecute
        "INSERT INTO course_enrollment_event(\
        \ registration_id, from_status, to_status, actor_type, reason_code, notes\
        \) VALUES (?, NULL, 'seat_held', 'system', 'checkout_created',\
        \ 'Atomic expiring seat hold created; payment and enrollment remain separate')"
        [toPersistValue registrationKey]
      pure (Right registrationKey)

loadCourseCheckoutRuntimeView
  :: ME.CourseRegistrationId
  -> SqlPersistT IO (Maybe CourseCheckoutRuntimeView)
loadCourseCheckoutRuntimeView registrationKey = do
  rows <- (rawSql
    "SELECT runtime.registration_id, registration.course_slug, runtime.checkout_id::text,\
    \ checkout.status, runtime.enrollment_status, runtime.hold_expires_at,\
    \ runtime.policy_version, runtime.currency, runtime.price_minor, runtime.tax_minor,\
    \ runtime.total_minor, runtime.due_now_minor, runtime.balance_minor,\
    \ runtime.payment_schedule, runtime.terms_version\
    \ FROM course_registration_checkout_runtime runtime\
    \ JOIN course_registration registration ON registration.id = runtime.registration_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ WHERE runtime.registration_id = ?\
    \ AND checkout.domain_type = 'course_registration'\
    \ AND checkout.domain_order_id = runtime.registration_id::text\
    \ AND checkout.total_minor = runtime.due_now_minor\
    \ AND checkout.currency = runtime.currency"
    [toPersistValue registrationKey]
    :: SqlPersistT IO
      [( Single Int64, Single Text, Single Text, Single Text, Single Text
       , Single UTCTime, Single Text, Single Text, Single Int64, Single Int64
       , Single Int64, Single Int64, Single Int64, Single Text, Single Text
       )])
  pure $ case rows of
    [( Single ccrvRegistrationId, Single ccrvCourseSlug, Single ccrvCheckoutId
     , Single ccrvPaymentStatus, Single ccrvEnrollmentStatus
     , Single ccrvHoldExpiresAt, Single ccrvPolicyVersion, Single ccrvCurrency
     , Single ccrvPriceMinor, Single ccrvTaxMinor, Single ccrvTotalMinor
     , Single ccrvDueNowMinor, Single ccrvBalanceMinor
     , Single ccrvPaymentSchedule, Single ccrvTermsVersion
     )] -> Just CourseCheckoutRuntimeView{..}
    _ -> Nothing

loadCourseCheckoutDTO
  :: ME.CourseRegistrationId
  -> Maybe Text
  -> AppM Courses.CourseCheckoutResponse
loadCourseCheckoutDTO registrationKey lookupToken = do
  runtime <- runDB (loadCourseCheckoutRuntimeView registrationKey)
    >>= maybe (throwError courseLookupNotFound) pure
  paymentMethods <- loadPublicCoursePaymentMethods runtime
  pure Courses.CourseCheckoutResponse
    { Courses.registrationId = ccrvRegistrationId runtime
    , Courses.courseSlug = ccrvCourseSlug runtime
    , Courses.checkoutId = Just (ccrvCheckoutId runtime)
    , Courses.lookupToken = lookupToken
    , Courses.paymentStatus = ccrvPaymentStatus runtime
    , Courses.fulfillmentStatus = ccrvEnrollmentStatus runtime
    , Courses.holdExpiresAt = Just (ccrvHoldExpiresAt runtime)
    , Courses.quote = Just Courses.CourseCheckoutQuote
        { Courses.policyVersion = ccrvPolicyVersion runtime
        , Courses.currency = ccrvCurrency runtime
        , Courses.subtotalMinor = ccrvPriceMinor runtime
        , Courses.taxMinor = ccrvTaxMinor runtime
        , Courses.totalMinor = ccrvTotalMinor runtime
        , Courses.dueNowMinor = ccrvDueNowMinor runtime
        , Courses.balanceMinor = ccrvBalanceMinor runtime
        , Courses.paymentSchedule = ccrvPaymentSchedule runtime
        , Courses.termsVersion = ccrvTermsVersion runtime
        }
    , Courses.paymentMethods = paymentMethods
    , Courses.checkoutAvailable = True
    }

loadPublicCoursePaymentMethods :: CourseCheckoutRuntimeView -> AppM [Text]
loadPublicCoursePaymentMethods runtime = do
  now <- liftIO getCurrentTime
  if ccrvPaymentStatus runtime `notElem` ["awaiting_payment", "failed"]
      || ccrvHoldExpiresAt runtime <= now
    then pure []
    else do
      let checkout = Checkout.CheckoutReference (ccrvCheckoutId runtime)
      environmentResult <- runDB (Checkout.loadCheckoutEnvironment checkout)
      case environmentResult of
        Left _ -> pure []
        Right environment -> do
          domainEnabled <- runDB $
            Checkout.domainEnabledForEnvironment environment "courses"
          if not domainEnabled
            then pure []
            else do
              datafastEnabled <- ((\datafast -> do
                  if ServiceStorefront.sdfEnvironment datafast /= environment
                    then pure False
                    else runDB $ Checkout.providerEnabledForEnvironment
                      environment Checkout.ProviderDatafast)
                =<< ServiceStorefront.loadServiceDatafastEnv)
                `catchError` const (pure False)
              paypalEnabled <- ((\(_, _, _, paypalEnvironment, _) -> do
                  if paypalEnvironment /= environment
                    then pure False
                    else runDB $ Checkout.providerEnabledForEnvironment
                      environment Checkout.ProviderPayPal)
                =<< ServiceStorefront.loadPaypalEnvForService)
                `catchError` const (pure False)
              pure $ ["datafast" | datafastEnabled] <> ["paypal" | paypalEnabled]

courseLookupNotFound :: ServerError
courseLookupNotFound = err404 { errBody = "Course order not found" }

validateRegistrationId :: Int64 -> Either ServerError ME.CourseRegistrationId
validateRegistrationId raw
  | raw <= 0 = Left (badRequestError "registrationId must be positive")
  | otherwise = Right (toSqlKey raw)

requirePublicCourseLookupToken
  :: ME.CourseRegistrationId
  -> Maybe Text
  -> AppM ()
requirePublicCourseLookupToken registrationKey mLookupToken = do
  suppliedToken <- case T.strip <$> mLookupToken of
    Just value | not (T.null value) -> pure value
    _ -> throwError courseLookupNotFound
  storedHashes <- runDB (rawSql
    "SELECT lookup_token_hash FROM course_registration_checkout_runtime\
    \ WHERE registration_id = ?"
    [toPersistValue registrationKey] :: SqlPersistT IO [Single Text])
  storedHash <- case storedHashes of
    [Single value] -> pure value
    _ -> throwError courseLookupNotFound
  let suppliedHash = sha256Text suppliedToken
  unless (constEq (TE.encodeUtf8 storedHash) (TE.encodeUtf8 suppliedHash)) $
    throwError courseLookupNotFound

getPublicCourseCheckout
  :: Text
  -> Int64
  -> Maybe Text
  -> AppM Courses.CourseCheckoutResponse
getPublicCourseCheckout rawSlug rawRegistrationId mLookupToken = do
  slugVal <- either throwError pure (normalizeSlug rawSlug)
  registrationKey <- either throwError pure (validateRegistrationId rawRegistrationId)
  requirePublicCourseLookupToken registrationKey mLookupToken
  runtime <- runDB (loadCourseCheckoutRuntimeView registrationKey)
    >>= maybe (throwError courseLookupNotFound) pure
  unless (ccrvCourseSlug runtime == slugVal) $
    throwError courseLookupNotFound
  now <- liftIO getCurrentTime
  when (ccrvHoldExpiresAt runtime <= now
      && ccrvPaymentStatus runtime `elem` ["holding", "awaiting_payment", "failed"]) $
    void $ runDB (rawSql "SELECT course_checkout_expire_holds(?)"
      [PersistUTCTime now] :: SqlPersistT IO [Single Int])
  loadCourseCheckoutDTO registrationKey Nothing

loadCoursePaymentContext
  :: ME.CourseRegistrationId
  -> AppM CoursePaymentContext
loadCoursePaymentContext registrationKey = do
  rows <- runDB (rawSql
    "SELECT registration.course_slug, runtime.checkout_id::text,\
    \ runtime.create_idempotency_key, checkout.status, checkout.environment,\
    \ runtime.due_now_minor, runtime.currency, runtime.hold_expires_at,\
    \ registration.full_name, registration.email, registration.phone_e164\
    \ FROM course_registration_checkout_runtime runtime\
    \ JOIN course_registration registration ON registration.id = runtime.registration_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ WHERE runtime.registration_id = ?\
    \ AND checkout.domain_type = 'course_registration'\
    \ AND checkout.domain_order_id = runtime.registration_id::text\
    \ AND checkout.total_minor = runtime.due_now_minor\
    \ AND checkout.currency = runtime.currency"
    [toPersistValue registrationKey]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text
       , Single Int64, Single Text, Single UTCTime, Single (Maybe Text)
       , Single (Maybe Text), Single (Maybe Text)
       )])
  case rows of
    [( Single courseSlug, Single checkoutId, Single createKey
     , Single checkoutStatus, Single environmentText, Single dueNowMinor
     , Single currency, Single holdExpiresAt, Single mBuyerName
     , Single mBuyerEmail, Single buyerPhone
     )] -> do
      environment <- either (throwError . internalError) pure $
        Checkout.resolveCheckoutEnvironment (Just (T.unpack environmentText))
      buyerName <- maybe
        (throwError (internalError "Course checkout buyer name is missing")) pure
        mBuyerName
      buyerEmail <- maybe
        (throwError (internalError "Course checkout buyer email is missing")) pure
        mBuyerEmail
      pure CoursePaymentContext
        { cpcRegistrationKey = registrationKey
        , cpcCourseSlug = courseSlug
        , cpcCheckout = Checkout.CheckoutReference checkoutId
        , cpcCreateIdempotencyKey = createKey
        , cpcCheckoutStatus = checkoutStatus
        , cpcEnvironment = environment
        , cpcDueNowMinor = dueNowMinor
        , cpcCurrency = currency
        , cpcHoldExpiresAt = holdExpiresAt
        , cpcBuyerName = buyerName
        , cpcBuyerEmail = buyerEmail
        , cpcBuyerPhone = buyerPhone
        }
    [] -> throwError courseLookupNotFound
    _ -> throwError (internalError "Course payment context is ambiguous")

authorizeCoursePaymentContext
  :: Text
  -> Int64
  -> Maybe Text
  -> AppM CoursePaymentContext
authorizeCoursePaymentContext rawSlug rawRegistrationId mLookupToken = do
  slugVal <- either throwError pure (normalizeSlug rawSlug)
  registrationKey <- either throwError pure (validateRegistrationId rawRegistrationId)
  requirePublicCourseLookupToken registrationKey mLookupToken
  context <- loadCoursePaymentContext registrationKey
  unless (cpcCourseSlug context == slugVal) $
    throwError courseLookupNotFound
  pure context

requireCoursePaymentContext
  :: Text
  -> Int64
  -> Maybe Text
  -> AppM CoursePaymentContext
requireCoursePaymentContext rawSlug rawRegistrationId mLookupToken = do
  context <- authorizeCoursePaymentContext rawSlug rawRegistrationId mLookupToken
  now <- liftIO getCurrentTime
  when (cpcHoldExpiresAt context <= now
      && cpcCheckoutStatus context `elem` ["holding", "awaiting_payment", "failed"]) $ do
    void $ runDB (rawSql "SELECT course_checkout_expire_holds(?)"
      [PersistUTCTime now] :: SqlPersistT IO [Single Int])
    throwError (conflictError
      "This course seat hold expired; start a new registration")
  unless (cpcCheckoutStatus context `elem`
      ["holding", "awaiting_payment", "processing", "failed", "paid"]) $
    throwError (conflictError
      "This course checkout no longer accepts payment actions")
  pure context

requireCourseProvider
  :: CoursePaymentContext
  -> Checkout.CheckoutEnvironment
  -> Checkout.PaymentProvider
  -> AppM ()
requireCourseProvider context configuredEnvironment provider = do
  unless (configuredEnvironment == cpcEnvironment context) $
    throwError err503
      { errBody = "Configured provider environment does not match this immutable course checkout" }
  domainEnabled <- runDB $
    Checkout.domainEnabledForEnvironment configuredEnvironment "courses"
  unless domainEnabled $
    throwError err503 { errBody = "Public course checkout is disabled in this environment" }
  providerEnabled <- runDB $
    Checkout.providerEnabledForEnvironment configuredEnvironment provider
  unless providerEnabled $
    throwError err503 { errBody = "Payment provider is disabled for this checkout environment" }

coursePaymentIdempotencyKey
  :: CoursePaymentContext
  -> Checkout.PaymentProvider
  -> Checkout.PaymentOperation
  -> Text
coursePaymentIdempotencyKey context provider operation = sha256Text $
  "course-payment:" <> cpcCreateIdempotencyKey context
    <> ":" <> Checkout.paymentProviderText provider
    <> ":" <> case operation of
      Checkout.OperationCreate -> "create"
      Checkout.OperationAuthorize -> "authorize"
      Checkout.OperationCapture -> "capture"
      Checkout.OperationManualVerify -> "manual-verify"

coursePaymentCorrelationId
  :: CoursePaymentContext
  -> Checkout.PaymentProvider
  -> Text
  -> Text
coursePaymentCorrelationId context provider operation =
  "course:" <> T.pack (show (fromSqlKey (cpcRegistrationKey context)))
    <> ":" <> Checkout.paymentProviderText provider <> ":" <> operation

beginCoursePaymentAttempt
  :: CoursePaymentContext
  -> Checkout.PaymentProvider
  -> Checkout.PaymentOperation
  -> Text
  -> Text
  -> AppM Checkout.PaymentAttemptReference
beginCoursePaymentAttempt context provider operation merchantRef operationLabel = do
  now <- liftIO getCurrentTime
  result <- runDB $ Checkout.beginPaymentAttempt Checkout.PaymentAttemptCreation
    { Checkout.pacCheckout = cpcCheckout context
    , Checkout.pacProvider = provider
    , Checkout.pacEnvironment = cpcEnvironment context
    , Checkout.pacOperation = operation
    , Checkout.pacAmountMinor = cpcDueNowMinor context
    , Checkout.pacCurrency = cpcCurrency context
    , Checkout.pacMerchantRef = merchantRef
    , Checkout.pacIdempotencyKey = coursePaymentIdempotencyKey context provider operation
    , Checkout.pacCreatedAt = now
    , Checkout.pacCorrelationId = coursePaymentCorrelationId context provider operationLabel
    }
  either (throwError . conflictError) pure result

failCoursePaymentAttempt
  :: CoursePaymentContext
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> ServerError
  -> AppM a
failCoursePaymentAttempt context attempt provider failureCode providerError = do
  now <- liftIO getCurrentTime
  runDB $ Checkout.recordPaymentFailure
    (cpcCheckout context) attempt provider failureCode
    (coursePaymentCorrelationId context provider "provider-error") now
  throwError providerError

loadCourseProviderBinding
  :: CoursePaymentContext
  -> Checkout.PaymentProvider
  -> Text
  -> Text
  -> AppM (Maybe (Text, Maybe Text))
loadCourseProviderBinding context provider merchantRef resourceType = do
  rows <- runDB (rawSql
    "SELECT binding.provider_resource_id, binding.provider_resource_path\
    \ FROM commerce_provider_binding binding\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = binding.payment_attempt_id\
    \ WHERE attempt.checkout_id = ?::uuid AND attempt.provider = ?\
    \ AND attempt.environment = ? AND attempt.merchant_account_ref = ?\
    \ AND binding.provider = attempt.provider\
    \ AND binding.environment = attempt.environment\
    \ AND binding.merchant_account_ref = attempt.merchant_account_ref\
    \ AND binding.resource_type = ? AND binding.merchant_reference = ?\
    \ AND binding.amount_minor = ? AND binding.currency = ?"
    [ PersistText (Checkout.checkoutReferenceId (cpcCheckout context))
    , PersistText (Checkout.paymentProviderText provider)
    , PersistText (Checkout.checkoutEnvironmentText (cpcEnvironment context))
    , PersistText merchantRef
    , PersistText resourceType
    , PersistText (T.pack (show (fromSqlKey (cpcRegistrationKey context))))
    , PersistInt64 (cpcDueNowMinor context)
    , PersistText (cpcCurrency context)
    ] :: SqlPersistT IO [(Single Text, Single (Maybe Text))])
  case rows of
    [] -> pure Nothing
    [(Single resourceId, Single resourcePath)] -> pure (Just (resourceId, resourcePath))
    _ -> throwError (internalError "Course provider binding is ambiguous")

bindCourseProviderResource
  :: CoursePaymentContext
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Checkout.PaymentAttemptStage
  -> Text
  -> AppM ()
bindCourseProviderResource
    context attempt provider environment merchantRef resourceType resourceId resourcePath stage operationLabel = do
  now <- liftIO getCurrentTime
  result <- runDB $ Checkout.bindProviderResource Checkout.ProviderBindingCreation
    { Checkout.pbcAttempt = attempt
    , Checkout.pbcCheckout = cpcCheckout context
    , Checkout.pbcProvider = provider
    , Checkout.pbcEnvironment = environment
    , Checkout.pbcMerchantRef = merchantRef
    , Checkout.pbcResourceType = resourceType
    , Checkout.pbcProviderResource = resourceId
    , Checkout.pbcResourcePath = resourcePath
    , Checkout.pbcOrderReference = T.pack (show (fromSqlKey (cpcRegistrationKey context)))
    , Checkout.pbcAmountMinor = cpcDueNowMinor context
    , Checkout.pbcCurrency = cpcCurrency context
    , Checkout.pbcStage = stage
    , Checkout.pbcOccurredAt = now
    , Checkout.pbcCorrelationId = coursePaymentCorrelationId context provider operationLabel
    }
  either (throwError . conflictError) pure result

ensureNoOtherActiveCourseOnlineAttempt
  :: CoursePaymentContext
  -> Checkout.PaymentProvider
  -> AppM ()
ensureNoOtherActiveCourseOnlineAttempt context provider = do
  rows <- runDB (rawSql
    "SELECT EXISTS (SELECT 1 FROM commerce_payment_attempt\
    \ WHERE checkout_id = ?::uuid AND provider <> ?\
    \ AND status IN ('requires_customer_action','processing'))"
    [ PersistText (Checkout.checkoutReferenceId (cpcCheckout context))
    , PersistText (Checkout.paymentProviderText provider)
    ] :: SqlPersistT IO [Single Bool])
  when (rows == [Single True]) $
    throwError (conflictError
      "Another online payment rail is active for this course checkout")

createPublicCourseDatafastCheckout
  :: Text
  -> Int64
  -> Maybe Text
  -> AppM APITypes.DatafastCheckoutDTO
createPublicCourseDatafastCheckout rawSlug rawRegistrationId mLookupToken = do
  context <- requireCoursePaymentContext rawSlug rawRegistrationId mLookupToken
  when (cpcCheckoutStatus context == "paid") $
    throwError (conflictError "This course registration is already paid")
  ensureNoOtherActiveCourseOnlineAttempt context Checkout.ProviderDatafast
  datafast <- ServiceStorefront.loadServiceDatafastEnv
  requireCourseProvider context
    (ServiceStorefront.sdfEnvironment datafast) Checkout.ProviderDatafast
  attempt <- beginCoursePaymentAttempt context Checkout.ProviderDatafast
    Checkout.OperationCreate (ServiceStorefront.sdfEntityId datafast) "create"
  existing <- loadCourseProviderBinding context Checkout.ProviderDatafast
    (ServiceStorefront.sdfEntityId datafast) "checkout"
  (checkoutId, widgetUrl) <- case existing of
    Just (storedCheckoutId, _) -> pure
      ( storedCheckoutId
      , dropWhileEndSlash (ServiceStorefront.sdfBaseUrl datafast)
          <> "/v1/paymentWidgets.js?checkoutId=" <> T.unpack storedCheckoutId
      )
    Nothing -> ServiceStorefront.requestDatafastCheckoutForService
      (registrationReference context)
      (fromIntegral (cpcDueNowMinor context))
      (cpcCurrency context)
      (cpcBuyerName context)
      (cpcBuyerEmail context)
      (cpcBuyerPhone context)
      `catchError` failCoursePaymentAttempt context attempt
        Checkout.ProviderDatafast "datafast_checkout_create"
  let resourcePath = "/v1/checkouts/" <> checkoutId <> "/payment"
  bindCourseProviderResource context attempt Checkout.ProviderDatafast
    (ServiceStorefront.sdfEnvironment datafast)
    (ServiceStorefront.sdfEntityId datafast) "checkout" checkoutId
    (Just resourcePath) Checkout.AttemptRequiresCustomerAction "create"
  pure APITypes.DatafastCheckoutDTO
    { APITypes.dcOrderId = registrationReference context
    , APITypes.dcCheckoutId = checkoutId
    , APITypes.dcWidgetUrl = T.pack widgetUrl
    , APITypes.dcAmount = Internationalization.formatMinorUnitsDecimal
        (cpcCurrency context) (fromIntegral (cpcDueNowMinor context))
    , APITypes.dcCurrency = cpcCurrency context
    , APITypes.dcLookupToken = Nothing
    }

dropWhileEndSlash :: String -> String
dropWhileEndSlash = reverse . dropWhile (== '/') . reverse

registrationReference :: CoursePaymentContext -> Text
registrationReference = T.pack . show . fromSqlKey . cpcRegistrationKey

confirmPublicCourseDatafastStatus
  :: Text
  -> Int64
  -> Maybe Text
  -> Text
  -> AppM Courses.CourseCheckoutResponse
confirmPublicCourseDatafastStatus rawSlug rawRegistrationId mLookupToken rawResourcePath = do
  initialContext <- authorizeCoursePaymentContext rawSlug rawRegistrationId mLookupToken
  nowBeforeVerification <- liftIO getCurrentTime
  when (cpcHoldExpiresAt initialContext <= nowBeforeVerification
      && cpcCheckoutStatus initialContext `elem` ["holding", "awaiting_payment", "failed"]) $
    void $ runDB (rawSql "SELECT course_checkout_expire_holds(?)"
      [PersistUTCTime nowBeforeVerification] :: SqlPersistT IO [Single Int])
  context <- loadCoursePaymentContext (cpcRegistrationKey initialContext)
  if cpcCheckoutStatus context == "paid"
    then loadCourseCheckoutDTO (cpcRegistrationKey context) Nothing
    else do
      datafast <- ServiceStorefront.loadServiceDatafastEnv
      unless (ServiceStorefront.sdfEnvironment datafast == cpcEnvironment context) $
        throwError err503
          { errBody = "Configured Datafast environment does not match this immutable course checkout" }
      existing <- loadCourseProviderBinding context Checkout.ProviderDatafast
        (ServiceStorefront.sdfEntityId datafast) "checkout"
      (checkoutId, storedResourcePath) <- maybe
        (throwError (conflictError
          "This course registration has no bound Datafast checkout")) pure existing
      resourcePath <- either (throwError . badRequestError) pure $
        ServiceStorefront.validateDatafastOrderResourcePath
          (Just checkoutId) rawResourcePath
      unless (storedResourcePath == Just resourcePath) $
        throwError (conflictError
          "Datafast resource path does not match the immutable course binding")
      attempt <- beginCoursePaymentAttempt context Checkout.ProviderDatafast
        Checkout.OperationCreate (ServiceStorefront.sdfEntityId datafast) "create"
      providerStatus <- ServiceStorefront.checkDatafastPaymentStatus resourcePath
        `catchError` failCoursePaymentAttempt context attempt
          Checkout.ProviderDatafast "datafast_status_request"
      now <- liftIO getCurrentTime
      let resultCode = ServiceStorefront.sdfpsResultCode providerStatus
          success = ServiceStorefront.isDatafastPaymentSuccess
            (ServiceStorefront.sdfEnvironment datafast) resultCode
          pending = resultCode == "000.200.000"
          orderReference = registrationReference context
      if success
        then do
          case ServiceStorefront.validateDatafastSuccessfulPayment
              orderReference (fromIntegral (cpcDueNowMinor context))
              (cpcCurrency context) providerStatus of
            Left validationMessage -> do
              let actualAmount = ServiceStorefront.sdfpsAmount providerStatus
                    >>= either (const Nothing) Just . ServiceStorefront.parseDatafastCents
                  providerRef = fromMaybe checkoutId
                    (ServiceStorefront.sdfpsPaymentId providerStatus)
              runDB $ do
                Checkout.recordReconciliationException
                  Checkout.ProviderDatafast (cpcEnvironment context)
                  (ServiceStorefront.sdfEntityId datafast)
                  "provider_binding_mismatch" orderReference providerRef
                  (cpcDueNowMinor context) (fromIntegral <$> actualAmount)
                  (cpcCurrency context) now
                Checkout.recordPaymentFailure (cpcCheckout context) attempt
                  Checkout.ProviderDatafast "provider_binding_mismatch"
                  (coursePaymentCorrelationId context Checkout.ProviderDatafast "status") now
              throwError err502 { errBody = textBody validationMessage }
            Right () -> pure ()
          when (cpcCheckoutStatus context == "expired") $ do
            let actualAmount = ServiceStorefront.sdfpsAmount providerStatus
                  >>= either (const Nothing) Just . ServiceStorefront.parseDatafastCents
                providerRef = fromMaybe checkoutId
                  (ServiceStorefront.sdfpsPaymentId providerStatus)
            runDB $ Checkout.recordReconciliationException
              Checkout.ProviderDatafast (cpcEnvironment context)
              (ServiceStorefront.sdfEntityId datafast)
              "payment_after_course_hold_expiry" orderReference providerRef
              (cpcDueNowMinor context) (fromIntegral <$> actualAmount)
              (cpcCurrency context) now
            throwError (conflictError
              "Datafast reports payment after this seat hold expired; reconciliation is required and enrollment is not confirmed")
          paymentId <- maybe
            (throwError err502 { errBody = "Datafast payment ID is missing" }) pure
            (ServiceStorefront.sdfpsPaymentId providerStatus)
          bindCourseProviderResource context attempt Checkout.ProviderDatafast
            (cpcEnvironment context) (ServiceStorefront.sdfEntityId datafast)
            "payment" paymentId (Just resourcePath) Checkout.AttemptProcessing "status"
          verified <- runDB $ Checkout.recordVerifiedPayment Checkout.VerifiedPayment
            { Checkout.vpAttempt = attempt
            , Checkout.vpCheckout = cpcCheckout context
            , Checkout.vpProvider = Checkout.ProviderDatafast
            , Checkout.vpEnvironment = cpcEnvironment context
            , Checkout.vpMerchantRef = ServiceStorefront.sdfEntityId datafast
            , Checkout.vpResourceType = "checkout"
            , Checkout.vpProviderResource = checkoutId
            , Checkout.vpProviderResourcePath = Just resourcePath
            , Checkout.vpOrderReference = orderReference
            , Checkout.vpAmountMinor = cpcDueNowMinor context
            , Checkout.vpCurrency = cpcCurrency context
            , Checkout.vpEvidence = "server_to_server"
            , Checkout.vpOccurredAt = now
            , Checkout.vpCorrelationId = coursePaymentCorrelationId
                context Checkout.ProviderDatafast "status"
            }
          either (throwError . conflictError) (const (pure ())) verified
        else if cpcCheckoutStatus context == "expired"
          then pure ()
          else if pending
            then runDB $ Checkout.recordPaymentProcessing
              (cpcCheckout context) attempt Checkout.ProviderDatafast
              (coursePaymentCorrelationId context Checkout.ProviderDatafast "status") now
            else runDB $ Checkout.recordPaymentFailure
              (cpcCheckout context) attempt Checkout.ProviderDatafast resultCode
              (coursePaymentCorrelationId context Checkout.ProviderDatafast "status") now
      loadCourseCheckoutDTO (cpcRegistrationKey context) Nothing

createPublicCoursePaypalOrder
  :: Text
  -> Int64
  -> Maybe Text
  -> AppM APITypes.PaypalCreateDTO
createPublicCoursePaypalOrder rawSlug rawRegistrationId mLookupToken = do
  context <- requireCoursePaymentContext rawSlug rawRegistrationId mLookupToken
  when (cpcCheckoutStatus context == "paid") $
    throwError (conflictError "This course registration is already paid")
  ensureNoOtherActiveCourseOnlineAttempt context Checkout.ProviderPayPal
  (clientId, clientSecret, baseUrl, paypalEnvironment, merchantRef) <-
    ServiceStorefront.loadPaypalEnvForService
  requireCourseProvider context paypalEnvironment Checkout.ProviderPayPal
  attempt <- beginCoursePaymentAttempt context Checkout.ProviderPayPal
    Checkout.OperationCreate merchantRef "create"
  existing <- loadCourseProviderBinding context Checkout.ProviderPayPal merchantRef "order"
  (paypalOrderId, approvalUrl) <- case existing of
    Just (storedOrderId, _) -> pure (storedOrderId, Nothing)
    Nothing -> ServiceStorefront.createPaypalOrderRemoteForService
      sharedTlsManager clientId clientSecret baseUrl (registrationReference context)
      (fromIntegral (cpcDueNowMinor context)) (cpcCurrency context)
      (cpcBuyerName context) (cpcBuyerEmail context)
      `catchError` failCoursePaymentAttempt context attempt
        Checkout.ProviderPayPal "paypal_create_order"
  bindCourseProviderResource context attempt Checkout.ProviderPayPal paypalEnvironment
    merchantRef "order" paypalOrderId
    (Just ("/v2/checkout/orders/" <> paypalOrderId))
    Checkout.AttemptRequiresCustomerAction "create"
  pure APITypes.PaypalCreateDTO
    { APITypes.pcOrderId = registrationReference context
    , APITypes.pcPaypalOrderId = paypalOrderId
    , APITypes.pcApprovalUrl = approvalUrl
    , APITypes.pcLookupToken = Nothing
    }

validatePaypalOrderId :: Text -> Either ServerError Text
validatePaypalOrderId raw
  | T.length clean < 6 || T.length clean > 80 =
      Left (badRequestError "PayPal order ID is invalid")
  | T.any (\char -> not (isAlphaNum char || char `elem` ['-','_'])) clean =
      Left (badRequestError "PayPal order ID is invalid")
  | otherwise = Right clean
  where
    clean = T.strip raw

capturePublicCoursePaypalOrder
  :: Text
  -> Int64
  -> Maybe Text
  -> Courses.CoursePaypalCaptureRequest
  -> AppM Courses.CourseCheckoutResponse
capturePublicCoursePaypalOrder rawSlug rawRegistrationId mLookupToken request = do
  context <- requireCoursePaymentContext rawSlug rawRegistrationId mLookupToken
  if cpcCheckoutStatus context == "paid"
    then loadCourseCheckoutDTO (cpcRegistrationKey context) Nothing
    else do
      suppliedOrderId <- either throwError pure $
        validatePaypalOrderId (Courses.paypalOrderId request)
      (clientId, clientSecret, baseUrl, paypalEnvironment, merchantRef) <-
        ServiceStorefront.loadPaypalEnvForService
      requireCourseProvider context paypalEnvironment Checkout.ProviderPayPal
      existing <- loadCourseProviderBinding context Checkout.ProviderPayPal merchantRef "order"
      storedOrderId <- maybe
        (throwError (conflictError
          "This course registration has no bound PayPal order"))
        (pure . fst) existing
      unless (storedOrderId == suppliedOrderId) $
        throwError (conflictError
          "PayPal order does not match the immutable course binding")
      attempt <- beginCoursePaymentAttempt context Checkout.ProviderPayPal
        Checkout.OperationCapture merchantRef "capture"
      outcome <- ServiceStorefront.capturePaypalOrderRemoteForService
        sharedTlsManager clientId clientSecret baseUrl suppliedOrderId
        `catchError` failCoursePaymentAttempt context attempt
          Checkout.ProviderPayPal "paypal_capture_request"
      now <- liftIO getCurrentTime
      let orderReference = registrationReference context
      case ServiceStorefront.spcoStatus outcome of
        "COMPLETED" -> do
          case ServiceStorefront.validatePaypalSuccessfulCapture
              orderReference (fromIntegral (cpcDueNowMinor context))
              (cpcCurrency context) merchantRef outcome of
            Left validationMessage -> do
              let actualAmount = ServiceStorefront.spcoAmount outcome
                    >>= either (const Nothing) Just . ServiceStorefront.parseDatafastCents
              runDB $ do
                Checkout.recordReconciliationException Checkout.ProviderPayPal
                  paypalEnvironment merchantRef "provider_binding_mismatch"
                  orderReference suppliedOrderId (cpcDueNowMinor context)
                  (fromIntegral <$> actualAmount) (cpcCurrency context) now
                Checkout.recordPaymentFailure (cpcCheckout context) attempt
                  Checkout.ProviderPayPal "provider_binding_mismatch"
                  (coursePaymentCorrelationId context Checkout.ProviderPayPal "capture") now
              throwError err502 { errBody = textBody validationMessage }
            Right () -> pure ()
          when (cpcHoldExpiresAt context <= now || cpcCheckoutStatus context == "expired") $ do
            let actualAmount = ServiceStorefront.spcoAmount outcome
                  >>= either (const Nothing) Just . ServiceStorefront.parseDatafastCents
            runDB $ Checkout.recordReconciliationException Checkout.ProviderPayPal
              paypalEnvironment merchantRef "payment_after_course_hold_expiry"
              orderReference suppliedOrderId (cpcDueNowMinor context)
              (fromIntegral <$> actualAmount) (cpcCurrency context) now
            throwError (conflictError
              "PayPal captured after this seat hold expired; reconciliation is required and enrollment is not confirmed")
          captureId <- maybe
            (throwError err502 { errBody = "PayPal capture ID is missing" }) pure
            (ServiceStorefront.spcoCaptureId outcome)
          bindCourseProviderResource context attempt Checkout.ProviderPayPal paypalEnvironment
            merchantRef "capture" captureId
            (Just ("/v2/checkout/orders/" <> suppliedOrderId <> "/capture"))
            Checkout.AttemptProcessing "capture"
          verified <- runDB $ Checkout.recordVerifiedPayment Checkout.VerifiedPayment
            { Checkout.vpAttempt = attempt
            , Checkout.vpCheckout = cpcCheckout context
            , Checkout.vpProvider = Checkout.ProviderPayPal
            , Checkout.vpEnvironment = paypalEnvironment
            , Checkout.vpMerchantRef = merchantRef
            , Checkout.vpResourceType = "capture"
            , Checkout.vpProviderResource = captureId
            , Checkout.vpProviderResourcePath = Just
                ("/v2/checkout/orders/" <> suppliedOrderId <> "/capture")
            , Checkout.vpOrderReference = orderReference
            , Checkout.vpAmountMinor = cpcDueNowMinor context
            , Checkout.vpCurrency = cpcCurrency context
            , Checkout.vpEvidence = "server_to_server"
            , Checkout.vpOccurredAt = now
            , Checkout.vpCorrelationId = coursePaymentCorrelationId
                context Checkout.ProviderPayPal "capture"
            }
          either (throwError . conflictError) (const (pure ())) verified
        "APPROVED" -> runDB $ Checkout.recordPaymentProcessing
          (cpcCheckout context) attempt Checkout.ProviderPayPal
          (coursePaymentCorrelationId context Checkout.ProviderPayPal "capture") now
        "PENDING" -> runDB $ Checkout.recordPaymentProcessing
          (cpcCheckout context) attempt Checkout.ProviderPayPal
          (coursePaymentCorrelationId context Checkout.ProviderPayPal "capture") now
        providerStatus -> runDB $ Checkout.recordPaymentFailure
          (cpcCheckout context) attempt Checkout.ProviderPayPal
          ("paypal_" <> T.toLower providerStatus)
          (coursePaymentCorrelationId context Checkout.ProviderPayPal "capture") now
      loadCourseCheckoutDTO (cpcRegistrationKey context) Nothing
