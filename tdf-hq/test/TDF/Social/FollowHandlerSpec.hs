{-# LANGUAGE OverloadedStrings #-}

module TDF.Social.FollowHandlerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (chr)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Database.Persist (Entity (..), get, insert, insertKey)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (Handler, ServerError (errBody, errHTTPCode), (:<|>) (..))
import Servant.Multipart
    ( FileData (..)
    , FromMultipart (fromMultipart)
    , Input (..)
    , MultipartData (..)
    , Tmp
    )
import Servant.Server.Internal.Handler (runHandler)
import Test.Hspec

import TDF.API.SocialEventsAPI
    ( EventImageUploadForm (..)
    , validateEventImageUploadForm
    )
import TDF.DTO.SocialEventsDTO
    ( ArtistDTO
    , ArtistFollowerDTO (..)
    , EventDTO (..)
    , EventMetadataUpdateDTO (..)
    , EventMomentCreateDTO (..)
    , EventMomentDTO
    , EventUpdateDTO (..)
    , InvitationDTO (..)
    , NullableFieldUpdate (..)
    , RefundDTO (..)
    , RefundRequestDTO (..)
    , RsvpCreateDTO (..)
    , RsvpDTO
    , StripePaymentIntentDTO
    , TicketDTO
    , TicketPurchaseRequestDTO (..)
    , TicketPurchaseWithPromoDTO (..)
    , TicketTransferDTO
    )
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.DB (Env (..))
import TDF.Models (Party (..), RoleEnum (Fan))
import TDF.Models.SocialEventsModels
import TDF.Server.SocialEventsHandlers
    ( decodeStoredPromoCodeTierIds
    , followArtistDb
    , resolveExistingPartyIdText
    , resolveUniqueRsvpRow
    , socialEventsServer
    , validateEventImageUploadSize
    , validateEventMetadataUpdate
    , validateEventMetadataUrlField
    , validateInvitationFromPartyId
    , validateSocialEventsListFilter
    , validateSocialEventsListOffset
    , validateStoredEventFinanceMetadata
    , validateTicketPurchaseBuyerEmail
    , validateTicketPurchaseBuyerName
    , validateVenueCreateUpdateFields
    )

mkEventImageUploadMultipart :: [(T.Text, T.Text)] -> [FileData Tmp] -> MultipartData Tmp
mkEventImageUploadMultipart fields uploads =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = uploads
        }

mkEventImageUploadFile :: T.Text -> FileData Tmp
mkEventImageUploadFile fileName =
    FileData
        { fdInputName = "file"
        , fdFileName = fileName
        , fdFileCType = "image/png"
        , fdPayload = "/tmp/mock-event-image-upload"
        }

spec :: Spec
spec = describe "social event handler helpers" $ do
    it "rejects duplicate RSVP rows instead of updating an arbitrary existing match" $ do
        now <- getCurrentTime
        let rsvpRow rowId status =
                Entity
                    (toSqlKey rowId)
                    EventRsvp
                        { eventRsvpEventId = toSqlKey 7
                        , eventRsvpPartyId = "42"
                        , eventRsvpStatus = status
                        , eventRsvpMetadata = Nothing
                        , eventRsvpCreatedAt = now
                        , eventRsvpUpdatedAt = now
                        }

        case resolveUniqueRsvpRow [rsvpRow 1 "accepted", rsvpRow 2 "maybe"] of
            Left err -> do
                errHTTPCode err `shouldBe` 409
                BL8.unpack (errBody err) `shouldContain` "Multiple RSVP rows exist"
            Right value ->
                expectationFailure ("Expected duplicate RSVP rows to be rejected, got: " <> show value)

    it "rejects empty or oversized event image uploads before copying files" $ do
        case validateEventImageUploadSize 1 of
            Right () -> pure ()
            Left err ->
                expectationFailure
                    ("Expected valid event image upload size, got: " <> show err)

        let assertInvalid expectedMessage size =
                case validateEventImageUploadSize size of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected invalid event image upload size to be rejected, got: " <> show value)

        assertInvalid "event image upload size is invalid" (-1)
        assertInvalid "event image upload must not be empty" 0
        assertInvalid "event image upload must be 10 MB or smaller" (10 * 1024 * 1024 + 1)

    it "revalidates event image upload names before handler filename sanitization" $ do
        case fromMultipart
            ( mkEventImageUploadMultipart
                [("name", "  Poster.png  ")]
                [mkEventImageUploadFile "camera.png"]
            ) :: Either String EventImageUploadForm of
            Right form ->
                eiuName form `shouldBe` Just "Poster.png"
            Left err ->
                expectationFailure
                    ("Expected valid event image upload multipart data, got: " <> err)

        let assertInvalid expectedMessage form =
                case validateEventImageUploadForm form of
                    Left err -> T.unpack err `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ( "Expected forged event image upload form to be rejected, got: "
                                <> show (eiuName value)
                            )

        assertInvalid
            "Uploaded browser file name must not contain path separators"
            EventImageUploadForm
                { eiuFile = mkEventImageUploadFile "events/poster.png"
                , eiuName = Just "poster.png"
                }
        assertInvalid
            "Uploaded image name must not contain leading, trailing, or repeated dots"
            EventImageUploadForm
                { eiuFile = mkEventImageUploadFile "camera.png"
                , eiuName = Just "poster..png"
                }
        assertInvalid
            "Uploaded browser file name must not contain leading, trailing, or repeated dots"
            EventImageUploadForm
                { eiuFile = mkEventImageUploadFile "poster..png"
                , eiuName = Nothing
                }
        assertInvalid
            "Uploaded image extension must match its MIME type"
            EventImageUploadForm
                { eiuFile = mkEventImageUploadFile "camera.jpg"
                , eiuName = Just "poster.png"
                }

    it "rejects unsafe social event metadata URLs before storing public links" $ do
        validateEventMetadataUrlField
            "eventTicketUrl"
            (Just " https://tickets.example.com/event?id=42 ")
            `shouldBe` Right (Just "https://tickets.example.com/event?id=42")
        validateEventMetadataUrlField "eventImageUrl" (Just "   ")
            `shouldBe` Right Nothing

        let assertInvalid field raw expectedMessage =
                case validateEventMetadataUrlField field (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected unsafe event metadata URL to be rejected, got: " <> show value)

        assertInvalid
            "eventTicketUrl"
            "http://tickets.example.com/event"
            "eventTicketUrl must be an absolute https URL"
        assertInvalid
            "eventImageUrl"
            "https://localhost/event.jpg"
            "eventImageUrl must be an absolute https URL"
        assertInvalid
            "eventTicketUrl"
            ("https://tickets.example.com/event/" <> T.replicate 2049 "a")
            "eventTicketUrl must be 2048 characters or fewer"

        case validateEventMetadataUpdate
            emptyEventMetadataUpdate
                { emuTicketUrl = FieldValue " https://tickets.example.com/event "
                , emuImageUrl = FieldValue "javascript:alert(1)"
                } of
            Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err)
                    `shouldContain` "eventImageUrl must be an absolute https URL"
            Right value ->
                expectationFailure
                    ("Expected metadata update with unsafe image URL to fail, got: " <> show value)

    it "rejects malformed stored event finance metadata instead of falling back to USD" $ do
        let eventWithMetadata rawMetadata =
                (seedSocialEvent "1" "Finance event" socialEventStartFixture)
                    { socialEventMetadata = rawMetadata
                    }
            assertInvalid rawMetadata expectedMessage =
                case validateStoredEventFinanceMetadata "USD" (eventWithMetadata rawMetadata) of
                    Left message ->
                        T.unpack message `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected invalid stored event metadata to be rejected, got: " <> show value)

        validateStoredEventFinanceMetadata "USD" (eventWithMetadata Nothing)
            `shouldBe` Right ("USD", Nothing)
        validateStoredEventFinanceMetadata "USD"
            (eventWithMetadata (Just "{\"currency\":\"eur\",\"budgetCents\":2500}"))
            `shouldBe` Right ("EUR", Just 2500)
        assertInvalid (Just "not-json") "Stored event metadata is invalid JSON"
        assertInvalid
            (Just "{\"currency\":\"usd\",\"budgetCents\":2500,\"curency\":\"eur\"}")
            "Stored event metadata contains unknown fields: curency"
        assertInvalid (Just "{\"currency\":\"USDT\"}") "Stored event currency is invalid"
        assertInvalid (Just "{\"budgetCents\":-1}") "Stored event budget is invalid"

    it "decodes stored promo-code tier ids only from JSON text arrays" $ do
        decodeStoredPromoCodeTierIds Nothing `shouldBe` Nothing
        decodeStoredPromoCodeTierIds (Just "[\"tier-vip\",\"tier-general\"]")
            `shouldBe` Just ["tier-vip", "tier-general"]
        decodeStoredPromoCodeTierIds (Just "not-json") `shouldBe` Nothing
        decodeStoredPromoCodeTierIds (Just "{\"tierIds\":[\"tier-vip\"]}")
            `shouldBe` Nothing
        decodeStoredPromoCodeTierIds (Just "[1,2]")
            `shouldBe` Nothing

    it "rejects hidden venue name markers before venue create/update persistence" $ do
        validateVenueCreateUpdateFields " Teatro TDF " Nothing Nothing (Just 250)
            `shouldBe` Right ()

        let hiddenFormat = T.singleton (chr 0x200D)
            assertInvalid rawName =
                case validateVenueCreateUpdateFields rawName Nothing Nothing Nothing of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err)
                            `shouldContain` "hidden formatting characters"
                    Right value ->
                        expectationFailure
                            ("Expected unsafe venue name to be rejected, got: " <> show value)

        assertInvalid ("Teatro" <> hiddenFormat <> "TDF")
        assertInvalid ("Teatro" <> T.singleton (chr 0x2028) <> "TDF")

    it "rejects deep social event list offsets before running list fallbacks" $ do
        validateSocialEventsListOffset Nothing `shouldBe` Right 0
        validateSocialEventsListOffset (Just 10000) `shouldBe` Right 10000

        let assertInvalid expectedMessage result =
                case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ( "Expected social event offset to be rejected, got: "
                                <> show value
                            )

        assertInvalid
            "offset must be greater than or equal to 0"
            (validateSocialEventsListOffset (Just (-1)))
        assertInvalid
            "offset must be 10000 or fewer"
            (validateSocialEventsListOffset (Just 10001))

    it "rejects oversized or unsafe social event list filters before DB fallback scans" $ do
        validateSocialEventsListFilter "city" Nothing `shouldBe` Right Nothing
        validateSocialEventsListFilter "city" (Just "  Quito  ")
            `shouldBe` Right (Just "Quito")
        validateSocialEventsListFilter "q" (Just "   ")
            `shouldBe` Right Nothing

        let assertInvalid expectedMessage result =
                case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ( "Expected social event list filter to be rejected, got: "
                                <> show value
                            )

        assertInvalid
            "city must be 120 characters or fewer"
            (validateSocialEventsListFilter "city" (Just (T.replicate 121 "x")))
        assertInvalid
            "q must not contain control characters or hidden formatting characters"
            (validateSocialEventsListFilter "q" (Just ("Quito" <> T.singleton '\n')))
        assertInvalid
            "name must not contain control characters or hidden formatting characters"
            (validateSocialEventsListFilter "name" (Just ("DJ" <> T.singleton (chr 0x202E))))

    it "uses canonical visibility for imported events across read and mutation routes" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let hiddenEventKey :: SocialEventId
            hiddenEventKey = toSqlKey 13
            publicEventKey :: SocialEventId
            publicEventKey = toSqlKey 14
            malformedEventKey :: SocialEventId
            malformedEventKey = toSqlKey 15
            unsupportedMetadataEventKey :: SocialEventId
            unsupportedMetadataEventKey = toSqlKey 16
            decimalBudgetEventKey :: SocialEventId
            decimalBudgetEventKey = toSqlKey 17
            exponentBudgetEventKey :: SocialEventId
            exponentBudgetEventKey = toSqlKey 18
            fractionalBudgetEventKey :: SocialEventId
            fractionalBudgetEventKey = toSqlKey 19
            roundedFractionalBudgetEventKey :: SocialEventId
            roundedFractionalBudgetEventKey = toSqlKey 20
            hiddenTierKey :: EventTicketTierId
            hiddenTierKey = toSqlKey 21
            publicTierKey :: EventTicketTierId
            publicTierKey = toSqlKey 22
            hiddenOrderKey :: EventTicketOrderId
            hiddenOrderKey = toSqlKey 51
            foreignOrderKey :: EventTicketOrderId
            foreignOrderKey = toSqlKey 52
            pendingHiddenOrderKey :: EventTicketOrderId
            pendingHiddenOrderKey = toSqlKey 53
            hiddenTicketKey :: EventTicketId
            hiddenTicketKey = toSqlKey 31
            hiddenTransferKey :: TicketTransferId
            hiddenTransferKey = toSqlKey 41
            sourceRef provider externalId eventKey sourceStatus sourceUrl =
                ExternalEventRef
                    { externalEventRefProvider = provider
                    , externalEventRefExternalId = externalId
                    , externalEventRefEventId = eventKey
                    , externalEventRefCity = "Quito"
                    , externalEventRefCountryCode = Just "EC"
                    , externalEventRefSourceUrl = Just sourceUrl
                    , externalEventRefPriceCents = Nothing
                    , externalEventRefCurrency = Just "USD"
                    , externalEventRefLastSeenAt = now
                    , externalEventRefMissingRuns = if sourceStatus == "missing" then 2 else 0
                    , externalEventRefSourceStatus = sourceStatus
                    }
            ticketTier eventKey code name =
                EventTicketTier
                    { eventTicketTierEventId = eventKey
                    , eventTicketTierCode = code
                    , eventTicketTierName = name
                    , eventTicketTierDescription = Nothing
                    , eventTicketTierPriceCents = 1000
                    , eventTicketTierCurrency = "USD"
                    , eventTicketTierCurrencyId = Nothing
                    , eventTicketTierQuantityTotal = 10
                    , eventTicketTierQuantitySold = 0
                    , eventTicketTierSalesStart = Nothing
                    , eventTicketTierSalesEnd = Nothing
                    , eventTicketTierIsActive = True
                    , eventTicketTierPosition = Nothing
                    , eventTicketTierEnableWaitlist = False
                    , eventTicketTierAllowTransfers = True
                    , eventTicketTierRefundPolicy = "full"
                    , eventTicketTierRefundDeadline = Nothing
                    , eventTicketTierCreatedAt = now
                    , eventTicketTierUpdatedAt = now
                    }
            ticketOrder eventKey tierKey buyerId buyerName buyerEmail status =
                EventTicketOrder
                    { eventTicketOrderEventId = eventKey
                    , eventTicketOrderTierId = tierKey
                    , eventTicketOrderBuyerPartyId = Just buyerId
                    , eventTicketOrderBuyerName = Just buyerName
                    , eventTicketOrderBuyerEmail = Just buyerEmail
                    , eventTicketOrderQuantity = 1
                    , eventTicketOrderAmountCents = 1000
                    , eventTicketOrderCurrency = "USD"
                    , eventTicketOrderStatus = status
                    , eventTicketOrderMetadata = Nothing
                    , eventTicketOrderCheckoutIdempotencyKey = Nothing
                    , eventTicketOrderPurchasedAt = now
                    , eventTicketOrderStripePaymentIntentId = Nothing
                    , eventTicketOrderPromoCodeId = Nothing
                    , eventTicketOrderOriginalAmountCents = Nothing
                    , eventTicketOrderPaymentMethod = Just "stripe"
                    , eventTicketOrderCreatedAt = now
                    , eventTicketOrderUpdatedAt = now
                    }
        runSqlPool
            ( do
                insertKey
                    hiddenEventKey
                    ( (seedSocialEvent "system:event-discovery" "Reconciled private pilot event" now)
                        { socialEventMetadata = Just "{\"isPublic\":false,\"currency\":\"USD\"}"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                insertKey
                    publicEventKey
                    ( (seedSocialEvent "system:event-discovery" "Public canonical event" now)
                        { socialEventMetadata = Just "{\"isPublic\":true,\"currency\":\"USD\"}"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                insertKey
                    malformedEventKey
                    ( (seedSocialEvent "system:event-discovery" "Malformed imported event" now)
                        { socialEventMetadata = Just "not-json"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                insertKey
                    unsupportedMetadataEventKey
                    ( (seedSocialEvent "system:event-discovery" "Unsupported imported metadata" now)
                        { socialEventMetadata = Just "{\"isPublic\":true,\"unexpected\":1}"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                insertKey
                    decimalBudgetEventKey
                    ( (seedSocialEvent "system:event-discovery" "Decimal integral budget" now)
                        { socialEventMetadata = Just "{\"isPublic\":true,\"budgetCents\":1.0}"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                insertKey
                    exponentBudgetEventKey
                    ( (seedSocialEvent "system:event-discovery" "Exponent integral budget" now)
                        { socialEventMetadata = Just "{\"isPublic\":true,\"budgetCents\":1e0}"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                insertKey
                    fractionalBudgetEventKey
                    ( (seedSocialEvent "system:event-discovery" "Fractional budget" now)
                        { socialEventMetadata = Just "{\"isPublic\":true,\"budgetCents\":1.5}"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                insertKey
                    roundedFractionalBudgetEventKey
                    ( (seedSocialEvent "system:event-discovery" "Rounded fractional budget" now)
                        { socialEventMetadata = Just "{\"isPublic\":true,\"budgetCents\":9007199254740992.5}"
                        , socialEventWorkflowStateId = Just socialEventWorkflowStateFixtureId
                        }
                    )
                _ <- insert (sourceRef "ticketmaster" "pilot-private-13" hiddenEventKey "missing" "https://tickets.example.com/private-pilot")
                _ <- insert (sourceRef "ticketmaster" "public-14" publicEventKey "on_sale" "https://tickets.example.com/public")
                _ <- insert (sourceRef "buenplan" "draft-merge-14" publicEventKey "draft:on_sale" "https://tickets.example.com/draft-option")
                _ <- insert (sourceRef "ticketmaster" "malformed-15" malformedEventKey "on_sale" "https://tickets.example.com/malformed")
                _ <- insert (sourceRef "ticketmaster" "unsupported-16" unsupportedMetadataEventKey "on_sale" "https://tickets.example.com/unsupported")
                _ <- insert (sourceRef "ticketmaster" "decimal-budget-17" decimalBudgetEventKey "on_sale" "https://tickets.example.com/decimal-budget")
                _ <- insert (sourceRef "ticketmaster" "exponent-budget-18" exponentBudgetEventKey "on_sale" "https://tickets.example.com/exponent-budget")
                _ <- insert (sourceRef "ticketmaster" "fractional-budget-19" fractionalBudgetEventKey "on_sale" "https://tickets.example.com/fractional-budget")
                _ <- insert (sourceRef "ticketmaster" "rounded-fractional-budget-20" roundedFractionalBudgetEventKey "on_sale" "https://tickets.example.com/rounded-fractional-budget")
                insertKey hiddenTierKey (ticketTier hiddenEventKey "hidden-tier" "Hidden tier")
                insertKey publicTierKey (ticketTier publicEventKey "public-tier" "Public tier")
                insertKey
                    hiddenOrderKey
                    (ticketOrder hiddenEventKey hiddenTierKey "2" "Refund buyer" "refund-buyer@example.com" "paid")
                insertKey
                    foreignOrderKey
                    (ticketOrder publicEventKey publicTierKey "3" "Other buyer" "other-buyer@example.com" "paid")
                insertKey
                    pendingHiddenOrderKey
                    (ticketOrder hiddenEventKey hiddenTierKey "3" "Other buyer" "other-buyer@example.com" "pending")
                insertKey
                    hiddenTicketKey
                    EventTicket
                        { eventTicketEventId = hiddenEventKey
                        , eventTicketTierRefId = hiddenTierKey
                        , eventTicketOrderRefId = hiddenOrderKey
                        , eventTicketHolderName = Just "Original holder"
                        , eventTicketHolderEmail = Just "holder@example.com"
                        , eventTicketCode = "hidden-ticket"
                        , eventTicketStatus = "active"
                        , eventTicketCheckedInAt = Nothing
                        , eventTicketCurrentHolderPartyId = Just "2"
                        , eventTicketCurrentHolderEmail = Just "holder@example.com"
                        , eventTicketCurrentHolderName = Just "Original holder"
                        , eventTicketOriginalHolderPartyId = Just "2"
                        , eventTicketTransferHistory = Nothing
                        , eventTicketCreatedAt = now
                        , eventTicketUpdatedAt = now
                        }
                insertKey
                    hiddenTransferKey
                    TicketTransfer
                        { ticketTransferTicketId = hiddenTicketKey
                        , ticketTransferFromPartyId = Just "2"
                        , ticketTransferToPartyId = Nothing
                        , ticketTransferToEmail = Just "recipient@example.com"
                        , ticketTransferToName = Just "Recipient"
                        , ticketTransferStatus = "pending"
                        , ticketTransferTransferCode = "hidden-transfer-code"
                        , ticketTransferMessage = Nothing
                        , ticketTransferExpiresAt = Nothing
                        , ticketTransferAcceptedAt = Nothing
                        , ticketTransferCreatedAt = now
                        , ticketTransferUpdatedAt = now
                        }
                pure ()
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by hidden pilot draft tests"
                    }
            ordinaryUser = socialEventUser 2
        listResult <-
            runHandler $
                runReaderT
                    ( socialEventListHandlerFor
                        ordinaryUser
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                    )
                    env
        case listResult of
            Right events ->
                map eventId events
                    `shouldMatchList` [Just "14", Just "17", Just "18"]
            Left err ->
                expectationFailure
                    ("Expected canonical public event list to succeed, got: " <> show err)

        paginatedListResult <-
            runHandler $
                runReaderT
                    ( socialEventListHandlerFor
                        ordinaryUser
                        Nothing
                        Nothing
                        (Just "2025-01-01T00:00:00Z")
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        (Just 1)
                        (Just 0)
                    )
                    env
        case paginatedListResult of
            Right [event] ->
                eventId event `shouldSatisfy` (`elem` [Just "14", Just "17", Just "18"])
            Right events ->
                expectationFailure
                    ("Expected filtered and paginated list to contain only the public canonical event, got: " <> show events)
            Left err ->
                expectationFailure
                    ("Expected filtered and paginated pilot list to succeed, got: " <> show err)

        getResult <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor ordinaryUser "13")
                    env
        case getResult of
            Left err -> do
                errHTTPCode err `shouldBe` 404
                BL8.unpack (errBody err) `shouldContain` "Event not found"
            Right event ->
                expectationFailure
                    ("Expected direct pilot draft access to be hidden, got: " <> show event)

        publicGetResult <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor ordinaryUser "14")
                    env
        case publicGetResult of
            Right event -> eventId event `shouldBe` Just "14"
            Left err ->
                expectationFailure
                    ("Expected the canonical event with an active public source to remain visible, got: " <> show err)

        malformedGetResult <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor ordinaryUser "15")
                    env
        assertHiddenEventRoute "malformed imported event" malformedGetResult

        unsupportedMetadataGetResult <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor ordinaryUser "16")
                    env
        assertHiddenEventRoute "unsupported imported metadata" unsupportedMetadataGetResult

        mapM_
            ( \eventIdText -> do
                integralBudgetGetResult <-
                    runHandler $
                        runReaderT
                            (socialEventGetHandlerFor ordinaryUser eventIdText)
                            env
                case integralBudgetGetResult of
                    Right event -> eventId event `shouldBe` Just eventIdText
                    Left err ->
                        expectationFailure
                            ( "Expected integral JSON budget event "
                                <> T.unpack eventIdText
                                <> " to remain visible, got: "
                                <> show err
                            )
            )
            ["17", "18"]

        fractionalBudgetGetResult <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor ordinaryUser "19")
                    env
        assertHiddenEventRoute "fractional imported budget metadata" fractionalBudgetGetResult

        roundedFractionalBudgetGetResult <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor ordinaryUser "20")
                    env
        assertHiddenEventRoute "large fractional imported budget metadata" roundedFractionalBudgetGetResult

        let ( stripeHandler
                , createRefundHandler
                , listRefundsHandler
                , acceptTransferHandler
                , cancelTransferHandler
                ) =
                socialEventIndirectTicketHandlersFor ordinaryUser
        refundResult <-
            runHandler $
                runReaderT
                    (createRefundHandler "13" "51" (RefundRequestDTO (Just "Provider cancellation") Nothing))
                    env
        case refundResult of
            Right refund -> do
                refundOrderId refund `shouldBe` "51"
                refundStatus refund `shouldBe` "pending"
            Left err ->
                expectationFailure
                    ("Expected the hidden-event buyer to retain refund access, got: " <> show err)

        refundListResult <-
            runHandler $
                runReaderT
                    (listRefundsHandler "13")
                    env
        case refundListResult of
            Right [refund] -> refundOrderId refund `shouldBe` "51"
            Right refunds ->
                expectationFailure
                    ("Expected the buyer's hidden-event refund, got: " <> show refunds)
            Left err ->
                expectationFailure
                    ("Expected the hidden-event buyer to list refunds, got: " <> show err)

        let (_, otherBuyerCreateRefund, otherBuyerListRefunds, _, _) =
                socialEventIndirectTicketHandlersFor (socialEventUser 3)
        unauthorizedRefundResult <-
            runHandler $
                runReaderT
                    (otherBuyerCreateRefund "13" "51" (RefundRequestDTO Nothing Nothing))
                    env
        assertHiddenEventRoute "another buyer's refund" unauthorizedRefundResult
        missingOrderRefundResult <-
            runHandler $
                runReaderT
                    (otherBuyerCreateRefund "13" "999" (RefundRequestDTO Nothing Nothing))
                    env
        assertHiddenEventRoute "missing hidden-event order" missingOrderRefundResult
        foreignOrderRefundResult <-
            runHandler $
                runReaderT
                    (otherBuyerCreateRefund "13" "52" (RefundRequestDTO Nothing Nothing))
                    env
        assertHiddenEventRoute "another event's order" foreignOrderRefundResult
        pendingOrderRefundResult <-
            runHandler $
                runReaderT
                    (otherBuyerCreateRefund "13" "53" (RefundRequestDTO Nothing Nothing))
                    env
        assertHiddenEventRoute "pending hidden-event order" pendingOrderRefundResult
        unauthorizedRefundListResult <-
            runHandler $
                runReaderT
                    (otherBuyerListRefunds "13")
                    env
        assertHiddenEventRoute "another buyer's refund list" unauthorizedRefundListResult

        rsvpResult <-
            runHandler $
                runReaderT
                    (socialEventRsvpCreateHandlerFor ordinaryUser "13" (RsvpCreateDTO "2" "accepted"))
                    env
        assertHiddenEventRoute "RSVP" rsvpResult

        invitationResult <-
            runHandler $
                runReaderT
                    (socialEventInvitationCreateHandlerFor ordinaryUser "13" (invitationCreatePayload Nothing))
                    env
        assertHiddenEventRoute "invitation" invitationResult

        momentResult <-
            runHandler $
                runReaderT
                    ( socialEventMomentCreateHandlerFor
                        ordinaryUser
                        "13"
                        (EventMomentCreateDTO Nothing Nothing "https://cdn.example.com/moment.jpg" "image" (Just 800) (Just 600) Nothing)
                    )
                    env
        assertHiddenEventRoute "moment" momentResult

        stripeResult <-
            runHandler $
                runReaderT
                    ( stripeHandler
                        ( TicketPurchaseWithPromoDTO
                            (TicketPurchaseRequestDTO "21" 1 Nothing Nothing Nothing)
                            Nothing
                            Nothing
                            Nothing
                        )
                    )
                    env
        assertHiddenEventRoute "Stripe payment intent" stripeResult

        acceptTransferResult <-
            runHandler $
                runReaderT
                    (acceptTransferHandler "hidden-transfer-code")
                    env
        assertHiddenEventRoute "transfer acceptance" acceptTransferResult

        cancelTransferResult <-
            runHandler $
                runReaderT
                    (cancelTransferHandler "41")
                    env
        assertHiddenEventRoute "transfer cancellation" cancelTransferResult

        (transferAfter, ticketAfter) <-
            runSqlPool
                ((,) <$> get hiddenTransferKey <*> get hiddenTicketKey)
                pool
        fmap ticketTransferStatus transferAfter `shouldBe` Just "pending"
        fmap eventTicketCurrentHolderPartyId ticketAfter `shouldBe` Just (Just "2")

    it "rejects punctuation-only ticket buyer names before creating ticket orders" $ do
        validateTicketPurchaseBuyerName Nothing `shouldBe` Right Nothing
        validateTicketPurchaseBuyerName (Just "  Diego Saa  ")
            `shouldBe` Right (Just "Diego Saa")

        case validateTicketPurchaseBuyerName (Just "  ***  ") of
            Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err)
                    `shouldContain` "ticketPurchaseBuyerName must include letters or numbers"
            Right value ->
                expectationFailure
                    ("Expected punctuation-only buyer name to be rejected, got: " <> show value)

    it "rejects ambiguous ticket buyer email final domains before creating ticket orders" $ do
        validateTicketPurchaseBuyerEmail (Just "  Fan+Ticket@Example.COM  ")
            `shouldBe` Right (Just "fan+ticket@example.com")

        let assertInvalid rawEmail =
                case validateTicketPurchaseBuyerEmail (Just rawEmail) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err)
                            `shouldContain` "ticketPurchaseBuyerEmail must be a valid email address"
                    Right value ->
                        expectationFailure
                            ("Expected invalid buyer email to be rejected, got: " <> show value)

        assertInvalid "fan@example.123"
        assertInvalid "fan@example.c"

    it "rejects malformed stored event metadata before publishing event DTO fallbacks" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let eventKey :: SocialEventId
            eventKey = toSqlKey 11
        runSqlPool
            ( insertKey
                eventKey
                ( (seedSocialEvent "1" "Corrupt metadata event" now)
                    { socialEventMetadata = Just "not-json"
                    }
                )
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by social event get tests"
                    }
        result <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor (socialEventUser 1) "11")
                    env

        case result of
            Left err -> do
                errHTTPCode err `shouldBe` 500
                BL8.unpack (errBody err)
                    `shouldContain` "Stored event metadata is invalid JSON"
            Right value ->
                expectationFailure
                    ("Expected malformed stored event metadata to be rejected, got: " <> show value)

    it "rejects malformed stored artist social links before publishing artist DTO fallbacks" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let artistKey :: ArtistProfileId
            artistKey = toSqlKey 15
        runSqlPool
            ( insertKey
                artistKey
                ArtistProfile
                    { artistProfilePartyId = Nothing
                    , artistProfileName = "Corrupt Links"
                    , artistProfileBio = Nothing
                    , artistProfileAvatarUrl = Nothing
                    , artistProfileGenres = Nothing
                    , artistProfileSocialLinks = Just "{\"bandcamp\":\"https://artist.example\"}"
                    , artistProfileCountryCode = Nothing
                    , artistProfileCountryId = Nothing
                    , artistProfileCreatedAt = now
                    , artistProfileUpdatedAt = now
                    }
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by artist get tests"
                    }
        result <-
            runHandler $
                runReaderT
                    (artistGetHandlerFor (socialEventUser 1) "15")
                    env

        case result of
            Left err -> do
                errHTTPCode err `shouldBe` 500
                BL8.unpack (errBody err)
                    `shouldContain` "Stored artist social links are invalid"
            Right value ->
                expectationFailure
                    ("Expected malformed stored artist social links to be rejected, got: " <> show value)

    it "rejects unknown follower party ids before the handler can create orphan follows or RSVPs" $ do
        pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool

        unknownResult <- resolveExistingPartyIdText pool "followerPartyId" "42"
        case unknownResult of
            Left err -> do
                errHTTPCode err `shouldBe` 422
                BL8.unpack (errBody err) `shouldContain` "followerPartyId references an unknown party"
            Right value ->
                expectationFailure ("Expected missing follower party to be rejected, got: " <> show value)

        now <- liftIO getCurrentTime
        existingPartyId <-
            runSqlPool
                ( insert
                    Party
                        { partyLegalName = Nothing
                        , partyDisplayName = "Follower"
                        , partyIsOrg = False
                        , partyTaxId = Nothing
                        , partyPrimaryEmail = Just "follower@example.com"
                        , partyPrimaryPhone = Nothing
                        , partyWhatsapp = Nothing
                        , partyInstagram = Nothing
                        , partyEmergencyContact = Nothing
                        , partyNotes = Nothing
                        , partyStripeCustomerId = Nothing
                        , partyCountryCode = Nothing
                        , partyCountryId = Nothing
                        , partyCreatedAt = now
                        }
                )
                pool

        let existingPartyText = T.pack (show (fromSqlKey existingPartyId))
        resolveExistingPartyIdText pool "followerPartyId" (" 00" <> existingPartyText <> " ")
            `shouldReturn` Right existingPartyText

    it "requires the event organizer before updating event details" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let eventKey :: SocialEventId
            eventKey = toSqlKey 7
        runSqlPool
            ( insertKey
                eventKey
                (seedSocialEvent "1" "Original event" now)
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by social event update auth tests"
                    }
        result <-
            runHandler $
                runReaderT
                    ( socialEventUpdateHandlerFor
                        (socialEventUser 2)
                        "7"
                        (socialEventUpdatePayload "Hijacked event")
                    )
                    env

        case result of
            Left err -> do
                errHTTPCode err `shouldBe` 403
                BL8.unpack (errBody err)
                    `shouldContain` "Only the event organizer can manage this event"
            Right updated ->
                expectationFailure
                    ("Expected non-organizer event update to fail, got: " <> show updated)

        stored <- runSqlPool (get eventKey) pool
        fmap socialEventTitle stored `shouldBe` Just "Original event"

    it "rejects spoofed invitation senders before inserting social event invitations" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let eventKey :: SocialEventId
            eventKey = toSqlKey 9
        runSqlPool
            ( insertKey
                eventKey
                (seedSocialEvent "5" "Invitation event" now)
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by invitation auth tests"
                    }

        validateInvitationFromPartyId "5" Nothing `shouldBe` Right "5"
        validateInvitationFromPartyId "5" (Just " 005 ") `shouldBe` Right "5"

        spoofed <-
            runHandler $
                runReaderT
                    ( socialEventInvitationCreateHandlerFor
                        (socialEventUser 5)
                        "9"
                        (invitationCreatePayload (Just "999"))
                    )
                    env
        case spoofed of
            Left err -> do
                errHTTPCode err `shouldBe` 403
                BL8.unpack (errBody err)
                    `shouldContain` "invitationFromPartyId must match the authenticated party"
            Right value ->
                expectationFailure
                    ("Expected spoofed invitation sender to be rejected, got: " <> show value)

    it "creates a follow and is idempotent" $ do
        pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- liftIO getCurrentTime
        artistId <-
            runSqlPool
                ( insert
                    ArtistProfile
                        { artistProfilePartyId = Nothing
                        , artistProfileName = "Band X"
                        , artistProfileBio = Nothing
                        , artistProfileAvatarUrl = Nothing
                        , artistProfileGenres = Nothing
                        , artistProfileSocialLinks = Nothing
                        , artistProfileCountryCode = Nothing
                        , artistProfileCountryId = Nothing
                        , artistProfileCreatedAt = now
                        , artistProfileUpdatedAt = now
                        }
                )
                pool

        first <- followArtistDb pool artistId "carla"
        second <- followArtistDb pool artistId "carla"
        liftIO $ do
            (afFollowId first) `shouldSatisfy` (/= Nothing)
            (afFollowId second) `shouldSatisfy` (/= Nothing)

socialEventUpdateHandlerFor
    :: AuthedUser
    -> T.Text
    -> EventUpdateDTO
    -> ReaderT Env Handler EventDTO
socialEventUpdateHandlerFor user =
    case socialEventsServer user of
        eventsServer
            :<|> _cities
            :<|> _sources
            :<|> _research
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> _invitations
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case eventsServer of
                _listEvents
                    :<|> _createEvent
                    :<|> _getEvent
                    :<|> updateEventHandler
                    :<|> _uploadEventImage
                    :<|> _deleteEvent ->
                    updateEventHandler

socialEventListHandlerFor
    :: AuthedUser
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe Int
    -> Maybe Int
    -> ReaderT Env Handler [EventDTO]
socialEventListHandlerFor user =
    case socialEventsServer user of
        eventsServer :<|> _ ->
            case eventsServer of
                listEventsHandler :<|> _ -> listEventsHandler

socialEventGetHandlerFor
    :: AuthedUser
    -> T.Text
    -> ReaderT Env Handler EventDTO
socialEventGetHandlerFor user =
    case socialEventsServer user of
        eventsServer
            :<|> _cities
            :<|> _sources
            :<|> _research
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> _invitations
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case eventsServer of
                _listEvents
                    :<|> _createEvent
                    :<|> getEventHandler
                    :<|> _updateEvent
                    :<|> _uploadEventImage
                    :<|> _deleteEvent ->
                    getEventHandler

socialEventRsvpCreateHandlerFor
    :: AuthedUser
    -> T.Text
    -> RsvpCreateDTO
    -> ReaderT Env Handler RsvpDTO
socialEventRsvpCreateHandlerFor user =
    case socialEventsServer user of
        _events
            :<|> _cities
            :<|> _sources
            :<|> _research
            :<|> _venues
            :<|> _artists
            :<|> rsvpsServer
            :<|> _ ->
            case rsvpsServer of
                _listRsvps :<|> createRsvpHandler -> createRsvpHandler

socialEventMomentCreateHandlerFor
    :: AuthedUser
    -> T.Text
    -> EventMomentCreateDTO
    -> ReaderT Env Handler EventMomentDTO
socialEventMomentCreateHandlerFor user =
    case socialEventsServer user of
        _events
            :<|> _cities
            :<|> _sources
            :<|> _research
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> _invitations
            :<|> momentsServer
            :<|> _ ->
            case momentsServer of
                _listMoments
                    :<|> createMomentHandler
                    :<|> _uploadMomentImage
                    :<|> _reactToMoment
                    :<|> _commentOnMoment ->
                    createMomentHandler

socialEventIndirectTicketHandlersFor
    :: AuthedUser
    -> ( TicketPurchaseWithPromoDTO -> ReaderT Env Handler StripePaymentIntentDTO
       , T.Text -> T.Text -> RefundRequestDTO -> ReaderT Env Handler RefundDTO
       , T.Text -> ReaderT Env Handler [RefundDTO]
       , T.Text -> ReaderT Env Handler TicketDTO
       , T.Text -> ReaderT Env Handler TicketTransferDTO
       )
socialEventIndirectTicketHandlersFor user =
    case socialEventsServer user of
        _events
            :<|> _cities
            :<|> _sources
            :<|> _research
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> _invitations
            :<|> _moments
            :<|> _liveBroadcasts
            :<|> ticketsServer
            :<|> _ -> case ticketsServer of
                _listTicketTiers
                    :<|> _createTicketTier
                    :<|> _updateTicketTier
                    :<|> _listMyTicketOrders
                    :<|> _listTicketOrders
                    :<|> _createTicketOrder
                    :<|> _updateTicketOrderStatus
                    :<|> _listTickets
                    :<|> _checkInTicket
                    :<|> _listPromoCodes
                    :<|> _createPromoCode
                    :<|> _updatePromoCode
                    :<|> _validatePromoCode
                    :<|> createStripePaymentIntentHandler
                    :<|> createRefundRequestHandler
                    :<|> listRefundsHandler
                    :<|> _approveRefund
                    :<|> _rejectRefund
                    :<|> _createTransfer
                    :<|> _listTransfers
                    :<|> acceptTransferHandler
                    :<|> cancelTransferHandler
                    :<|> _remainingTicketHandlers ->
                        ( createStripePaymentIntentHandler
                        , createRefundRequestHandler
                        , listRefundsHandler
                        , acceptTransferHandler
                        , cancelTransferHandler
                        )

artistGetHandlerFor
    :: AuthedUser
    -> T.Text
    -> ReaderT Env Handler ArtistDTO
artistGetHandlerFor user =
    case socialEventsServer user of
        _events
            :<|> _cities
            :<|> _sources
            :<|> _research
            :<|> _venues
            :<|> artistsServer
            :<|> _rsvps
            :<|> _invitations
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case artistsServer of
                _listArtists
                    :<|> _createArtist
                    :<|> getArtistHandler
                    :<|> _updateArtist
                    :<|> _listArtistFollowers
                    :<|> _followArtist
                    :<|> _unfollowArtist ->
                    getArtistHandler

socialEventInvitationCreateHandlerFor
    :: AuthedUser
    -> T.Text
    -> InvitationDTO
    -> ReaderT Env Handler InvitationDTO
socialEventInvitationCreateHandlerFor user eventIdText =
    case socialEventsServer user of
        _events
            :<|> _cities
            :<|> _sources
            :<|> _research
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> invitationsServer
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case invitationsServer eventIdText of
                _listInvitations :<|> createInvitationHandler :<|> _updateInvitation ->
                    createInvitationHandler

assertHiddenEventRoute :: Show a => String -> Either ServerError a -> Expectation
assertHiddenEventRoute label result =
    case result of
        Left err -> do
            errHTTPCode err `shouldBe` 404
            BL8.unpack (errBody err) `shouldContain` "Event not found"
        Right value ->
            expectationFailure
                ("Expected hidden event " <> label <> " route to return 404, got: " <> show value)

socialEventUser :: Int64 -> AuthedUser
socialEventUser partyId =
    AuthedUser
        { auPartyId = toSqlKey partyId
        , auRoles = [Fan]
        , auModules = modulesForRoles [Fan]
        }

socialEventStartFixture :: UTCTime
socialEventStartFixture =
    UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

socialEventEndFixture :: UTCTime
socialEventEndFixture =
    UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 3600)

socialEventWorkflowStateFixtureId :: UUID.UUID
socialEventWorkflowStateFixtureId =
    case UUID.fromString "00000000-0000-4000-8000-000000000233" of
        Just workflowStateId -> workflowStateId
        Nothing -> error "Invalid social-event workflow-state fixture UUID"

seedSocialEvent :: T.Text -> T.Text -> UTCTime -> SocialEvent
seedSocialEvent owner title now =
    SocialEvent
        { socialEventOrganizerPartyId = Just owner
        , socialEventTitle = title
        , socialEventDescription = Nothing
        , socialEventVenueId = Nothing
        , socialEventStartTime = socialEventStartFixture
        , socialEventEndTime = Just socialEventEndFixture
        , socialEventPriceCents = Nothing
        , socialEventCapacity = Nothing
        , socialEventMetadata = Nothing
        , socialEventEventTypeId = Nothing
        , socialEventWorkflowStateId = Nothing
        , socialEventTimezone = Nothing
        , socialEventCurrencyId = Nothing
        , socialEventCreatedAt = now
        , socialEventUpdatedAt = now
        }

socialEventUpdatePayload :: T.Text -> EventUpdateDTO
socialEventUpdatePayload title =
    EventUpdateDTO
        { eudEvent =
            EventDTO
                { eventId = Just "7"
                , eventOrganizerPartyId = Nothing
                , eventTitle = title
                , eventDescription = Nothing
                , eventStart = socialEventStartFixture
                , eventEnd = Just socialEventEndFixture
                , eventTimezone = Just "America/Guayaquil"
                , eventVenueId = Nothing
                , eventPriceCents = Nothing
                , eventCapacity = Nothing
                , eventTicketUrl = Nothing
                , eventImageUrl = Nothing
                , eventIsPublic = Nothing
                , eventTypeId = Nothing
                , eventWorkflowStateId = Nothing
                , eventWorkflowStateCode = Nothing
                , eventWorkflowStateNameEs = Nothing
                , eventWorkflowStateNameEn = Nothing
                , eventPublicListable = Nothing
                , eventTicketPurchaseEnabled = Nothing
                , eventCurrency = Nothing
                , eventBudgetCents = Nothing
                , eventSources = Nothing
                , eventCreatedAt = Nothing
                , eventUpdatedAt = Nothing
                , eventArtists = []
                }
        , eudWorkflowStateIdUpdate = FieldMissing
        , eudMetadataUpdate = emptyEventMetadataUpdate
        }

invitationCreatePayload :: Maybe T.Text -> InvitationDTO
invitationCreatePayload mFromPartyId =
    InvitationDTO
        { invitationId = Nothing
        , invitationEventId = Nothing
        , invitationFromPartyId = mFromPartyId
        , invitationToPartyId = "2"
        , invitationStatus = Just "pending"
        , invitationMessage = Just "Join us"
        , invitationCreatedAt = Nothing
        , invitationUpdatedAt = Nothing
        }

emptyEventMetadataUpdate :: EventMetadataUpdateDTO
emptyEventMetadataUpdate =
    EventMetadataUpdateDTO
        { emuTicketUrl = FieldMissing
        , emuImageUrl = FieldMissing
        , emuIsPublic = FieldMissing
        , emuCurrency = FieldMissing
        , emuBudgetCents = FieldMissing
        }

initializeSocialSchema :: SqlPersistT IO ()
initializeSocialSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"party\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"legal_name\" VARCHAR NULL,\
        \\"display_name\" VARCHAR NOT NULL,\
        \\"is_org\" BOOLEAN NOT NULL,\
        \\"tax_id\" VARCHAR NULL,\
        \\"primary_email\" VARCHAR NULL,\
        \\"primary_phone\" VARCHAR NULL,\
        \\"whatsapp\" VARCHAR NULL,\
        \\"instagram\" VARCHAR NULL,\
        \\"emergency_contact\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"stripe_customer_id\" VARCHAR NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"social_artist_profile\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" VARCHAR NULL,\
        \\"name\" VARCHAR NOT NULL,\
        \\"bio\" VARCHAR NULL,\
        \\"avatar_url\" VARCHAR NULL,\
        \\"genres\" VARCHAR NULL,\
        \\"social_links\" VARCHAR NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_genre\" (\
        \\"artist_id\" INTEGER NOT NULL,\
        \\"genre\" VARCHAR NOT NULL,\
        \\"genre_id\" VARCHAR NULL,\
        \PRIMARY KEY (\"artist_id\", \"genre\"),\
        \FOREIGN KEY(\"artist_id\") REFERENCES \"social_artist_profile\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_genre_membership\" (\
        \\"artist_id\" INTEGER NOT NULL,\
        \\"genre_id\" VARCHAR NOT NULL,\
        \\"sort_order\" INTEGER NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \PRIMARY KEY (\"artist_id\", \"genre_id\"),\
        \FOREIGN KEY(\"artist_id\") REFERENCES \"social_artist_profile\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"venue\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"address\" VARCHAR NULL,\
        \\"city\" VARCHAR NULL,\
        \\"country\" VARCHAR NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"city_id\" VARCHAR NULL,\
        \\"timezone\" VARCHAR NULL,\
        \\"latitude\" REAL NULL,\
        \\"longitude\" REAL NULL,\
        \\"capacity\" INTEGER NULL,\
        \\"contact\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"social_event\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"organizer_party_id\" VARCHAR NULL,\
        \\"title\" VARCHAR NOT NULL,\
        \\"description\" VARCHAR NULL,\
        \\"venue_id\" INTEGER NULL,\
        \\"event_type_id\" VARCHAR NULL,\
        \\"workflow_state_id\" VARCHAR NULL,\
        \\"timezone\" VARCHAR NULL,\
        \\"start_time\" TIMESTAMP NOT NULL,\
        \\"end_time\" TIMESTAMP NOT NULL,\
        \\"price_cents\" INTEGER NULL,\
        \\"currency_id\" VARCHAR NULL,\
        \\"capacity\" INTEGER NULL,\
        \\"metadata\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"workflow_definition\" (\"id\" VARCHAR PRIMARY KEY,\"code\" VARCHAR NOT NULL UNIQUE,\"active\" BOOLEAN NOT NULL)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"workflow_state\" (\"id\" VARCHAR PRIMARY KEY,\"workflow_id\" VARCHAR NOT NULL,\"code\" VARCHAR NOT NULL,\"name_es\" VARCHAR NOT NULL,\"name_en\" VARCHAR NOT NULL,\"active\" BOOLEAN NOT NULL)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"workflow_state_capability\" (\"state_id\" VARCHAR NOT NULL,\"capability_code\" VARCHAR NOT NULL,\"enabled\" BOOLEAN NOT NULL,PRIMARY KEY (\"state_id\",\"capability_code\"))"
        []
    rawExecute
        "INSERT INTO \"workflow_definition\" (\"id\",\"code\",\"active\") VALUES ('00000000-0000-4000-8000-000000000104','social-event-lifecycle',1)"
        []
    rawExecute
        "INSERT INTO \"workflow_state\" (\"id\",\"workflow_id\",\"code\",\"name_es\",\"name_en\",\"active\") VALUES ('00000000-0000-4000-8000-000000000233','00000000-0000-4000-8000-000000000104','on_sale','En venta','On sale',1)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"event_discovery_source\" (\"id\" INTEGER PRIMARY KEY,\"source_key\" VARCHAR NOT NULL,\"name\" VARCHAR NOT NULL,\"source_type\" VARCHAR NOT NULL,\"feed_url\" VARCHAR NULL,\"city_id\" INTEGER NULL,\"enabled\" BOOLEAN NOT NULL DEFAULT 1,\"priority\" INTEGER NOT NULL DEFAULT 100,\"configuration\" VARCHAR NULL,\"etag\" VARCHAR NULL,\"last_modified\" VARCHAR NULL,\"consecutive_failures\" INTEGER NOT NULL DEFAULT 0,\"last_success_at\" TIMESTAMP NULL,\"last_error\" VARCHAR NULL,\"created_at\" TIMESTAMP NOT NULL,\"updated_at\" TIMESTAMP NOT NULL,UNIQUE (\"source_key\"))"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"event_ticket_tier\" (\"id\" INTEGER PRIMARY KEY,\"event_id\" INTEGER NOT NULL,\"code\" VARCHAR NOT NULL,\"name\" VARCHAR NOT NULL,\"description\" VARCHAR NULL,\"price_cents\" INTEGER NOT NULL,\"currency\" VARCHAR NOT NULL,\"currency_id\" VARCHAR NULL,\"quantity_total\" INTEGER NOT NULL,\"quantity_sold\" INTEGER NOT NULL,\"sales_start\" TIMESTAMP NULL,\"sales_end\" TIMESTAMP NULL,\"is_active\" BOOLEAN NOT NULL,\"position\" INTEGER NULL,\"enable_waitlist\" BOOLEAN NOT NULL,\"allow_transfers\" BOOLEAN NOT NULL,\"refund_policy\" VARCHAR NOT NULL,\"refund_deadline\" TIMESTAMP NULL,\"created_at\" TIMESTAMP NOT NULL,\"updated_at\" TIMESTAMP NOT NULL,UNIQUE (\"event_id\",\"code\"))"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"event_ticket_order\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"event_id\" INTEGER NOT NULL,\
        \\"tier_id\" INTEGER NOT NULL,\
        \\"buyer_party_id\" VARCHAR NULL,\
        \\"buyer_name\" VARCHAR NULL,\
        \\"buyer_email\" VARCHAR NULL,\
        \\"quantity\" INTEGER NOT NULL,\
        \\"amount_cents\" INTEGER NOT NULL,\
        \\"currency\" VARCHAR NOT NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"metadata\" VARCHAR NULL,\
        \\"checkout_idempotency_key\" VARCHAR NULL,\
        \\"purchased_at\" TIMESTAMP NOT NULL,\
        \\"stripe_payment_intent_id\" VARCHAR NULL,\
        \\"promo_code_id\" INTEGER NULL,\
        \\"original_amount_cents\" INTEGER NULL,\
        \\"payment_method\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \UNIQUE (\"buyer_party_id\", \"checkout_idempotency_key\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"event_ticket\" (\"id\" INTEGER PRIMARY KEY,\"event_id\" INTEGER NOT NULL,\"tier_ref_id\" INTEGER NOT NULL,\"order_ref_id\" INTEGER NOT NULL,\"holder_name\" VARCHAR NULL,\"holder_email\" VARCHAR NULL,\"code\" VARCHAR NOT NULL,\"status\" VARCHAR NOT NULL,\"checked_in_at\" TIMESTAMP NULL,\"current_holder_party_id\" VARCHAR NULL,\"current_holder_email\" VARCHAR NULL,\"current_holder_name\" VARCHAR NULL,\"original_holder_party_id\" VARCHAR NULL,\"transfer_history\" VARCHAR NULL,\"created_at\" TIMESTAMP NOT NULL,\"updated_at\" TIMESTAMP NOT NULL,UNIQUE (\"code\"))"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"ticket_refund_request\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"order_id\" INTEGER NOT NULL,\
        \\"requested_by_party_id\" VARCHAR NULL,\
        \\"reason\" VARCHAR NULL,\
        \\"amount_cents\" INTEGER NOT NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"approved_by_party_id\" VARCHAR NULL,\
        \\"approved_at\" TIMESTAMP NULL,\
        \\"rejection_reason\" VARCHAR NULL,\
        \\"stripe_refund_id\" VARCHAR NULL,\
        \\"processed_at\" TIMESTAMP NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"ticket_transfer\" (\"id\" INTEGER PRIMARY KEY,\"ticket_id\" INTEGER NOT NULL,\"from_party_id\" VARCHAR NULL,\"to_party_id\" VARCHAR NULL,\"to_email\" VARCHAR NULL,\"to_name\" VARCHAR NULL,\"status\" VARCHAR NOT NULL,\"transfer_code\" VARCHAR NOT NULL,\"message\" VARCHAR NULL,\"expires_at\" TIMESTAMP NULL,\"accepted_at\" TIMESTAMP NULL,\"created_at\" TIMESTAMP NOT NULL,\"updated_at\" TIMESTAMP NOT NULL,UNIQUE (\"transfer_code\"))"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"external_event_ref\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"provider\" VARCHAR NOT NULL,\
        \\"external_id\" VARCHAR NOT NULL,\
        \\"event_id\" INTEGER NOT NULL,\
        \\"city\" VARCHAR NOT NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"source_url\" VARCHAR NULL,\
        \\"price_cents\" INTEGER NULL,\
        \\"currency\" VARCHAR NULL,\
        \\"last_seen_at\" TIMESTAMP NOT NULL,\
        \\"missing_runs\" INTEGER NOT NULL DEFAULT 0,\
        \\"source_status\" VARCHAR NOT NULL DEFAULT 'active',\
        \UNIQUE (\"provider\", \"external_id\"),\
        \FOREIGN KEY(\"event_id\") REFERENCES \"social_event\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"event_artist\" (\
        \\"event_id\" INTEGER NOT NULL,\
        \\"artist_id\" INTEGER NOT NULL,\
        \\"role\" VARCHAR NULL,\
        \PRIMARY KEY (\"event_id\", \"artist_id\"),\
        \FOREIGN KEY(\"event_id\") REFERENCES \"social_event\"(\"id\") ON DELETE CASCADE,\
        \FOREIGN KEY(\"artist_id\") REFERENCES \"social_artist_profile\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_follow\" (\
        \\"artist_id\" INTEGER NOT NULL,\
        \\"follower_party_id\" VARCHAR NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \PRIMARY KEY (\"artist_id\", \"follower_party_id\"),\
        \FOREIGN KEY(\"artist_id\") REFERENCES \"social_artist_profile\"(\"id\") ON DELETE CASCADE\
        \)"
        []
