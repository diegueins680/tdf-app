{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.Merch
  ( merchPublicServer
  , merchProtectedServer
  ) where

import           Codec.Picture (DynamicImage(..), Image, PixelRGB8, convertRGB8, decodeImage, generateImage, imageHeight, imageWidth, pixelAt, saveJpgImage)
import           Control.Exception (SomeException, try)
import           Control.Monad (forM, forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, asks)
import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Aeson (ToJSON, Value(..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isControl)
import           Data.Int (Int64)
import           Data.List (nub)
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, addUTCTime, getCurrentTime)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..), toPersistValue)
import           Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import           Servant
import           Servant.Multipart (FileData(..))
import           System.Directory (createDirectoryIfMissing, getFileSize, removeFile)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>), takeExtension)

import           TDF.API.Merch
import           TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.CMS.Models as CMS
import           TDF.Commerce.Merch
import           TDF.Config (assetsRootDir)
import           TDF.DB (Env(..))

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

runCheckoutDB :: SqlPersistT IO a -> AppM a
runCheckoutDB action = do
  pool <- asks envPool
  outcome <- liftIO (tryAny (runSqlPool action pool))
  either (const (throwError err409 { errBody = "Cart, price, or stock changed; refresh and retry" })) pure outcome

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

scaleImageNearest :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
scaleImageNearest targetWidth targetHeight source = generateImage sample targetWidth targetHeight
  where
    sourceWidth = imageWidth source
    sourceHeight = imageHeight source
    sample x y = pixelAt source
      (min (sourceWidth - 1) (x * sourceWidth `div` targetWidth))
      (min (sourceHeight - 1) (y * sourceHeight `div` targetHeight))

jsonRows :: Text -> [PersistValue] -> AppM [Value]
jsonRows statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue])
  pure [CMS.unAesonValue value | Single value <- rows]

jsonOne :: ServerError -> Text -> [PersistValue] -> AppM Value
jsonOne missing statement params =
  jsonRows statement params >>= maybe (throwError missing) pure . listToMaybe

jsonText :: ToJSON value => value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

hashText :: Text -> Text
hashText input = T.pack (show (hash (TE.encodeUtf8 input) :: Digest SHA256))

uuidText :: UUID -> Text
uuidText = UUID.toText

currentPartyId :: AuthedUser -> Int64
currentPartyId = fromSqlKey . auPartyId

optionalText :: Maybe Text -> PersistValue
optionalText = maybe PersistNull PersistText

optionalInt :: Maybe Int -> PersistValue
optionalInt = maybe PersistNull (PersistInt64 . fromIntegral)

optionalInt64 :: Maybe Int64 -> PersistValue
optionalInt64 = maybe PersistNull PersistInt64

optionalUuid :: Maybe UUID -> PersistValue
optionalUuid = maybe PersistNull toPersistValue

appendMerchAudit
  :: AuthedUser -> Text -> Maybe UUID -> Text -> Text -> Text -> Value -> SqlPersistT IO ()
appendMerchAudit user actorType storeId action entityType entityId afterState =
  rawExecute
    "INSERT INTO merch_audit_event(store_id,actor_party_id,actor_type,action,entity_type,entity_id,correlation_id,after_state) VALUES(?::uuid,?,?,?,?,?,gen_random_uuid()::text,?::jsonb)"
    [ optionalUuid storeId, PersistInt64 (currentPartyId user), PersistText actorType
    , PersistText action, PersistText entityType, PersistText entityId
    , PersistText (jsonText afterState)
    ]

requiredSafeText :: Text -> Int -> Text -> AppM Text
requiredSafeText field maxLength raw =
  either (throwError . badRequest) pure (validateCheckoutText field maxLength raw)

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

conflict :: Text -> ServerError
conflict message = err409 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

requireIdempotencyKey :: Maybe Text -> AppM Text
requireIdempotencyKey raw = do
  key <- requiredSafeText "Idempotency-Key" 200 (fromMaybe "" raw)
  when (T.length key < 8) $ throwError (badRequest "Idempotency-Key must contain at least 8 characters")
  pure key

featureEnvironment :: IO Text
featureEnvironment = do
  raw <- lookupEnv "APP_ENV"
  pure $ case fmap (T.toLower . T.strip . T.pack) raw of
    Just "production" -> "production"
    Just "prod" -> "production"
    Just "staging" -> "staging"
    _ -> "sandbox"

featureEnabled :: Text -> AppM Bool
featureEnabled key = do
  environment <- liftIO featureEnvironment
  rows <- runDB (rawSql
    "SELECT enabled FROM revenue_feature_flag WHERE flag_key=? AND environment=?"
    [PersistText key, PersistText environment] :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

requireFeature :: Text -> AppM ()
requireFeature key = do
  enabled <- featureEnabled key
  unless enabled $ throwError err503
    { errBody = BL.fromStrict . TE.encodeUtf8 $
        "Feature " <> key <> " is not enabled in this environment"
    }

credentialsPresent :: [String] -> IO Bool
credentialsPresent names = and <$> mapM present names
  where
    present name = maybe False (not . T.null . T.strip . T.pack) <$> lookupEnv name

merchPaymentMethodAvailability :: AppM (Bool, Bool, Bool)
merchPaymentMethodAvailability = do
  checkoutEnabled <- featureEnabled "merch.checkout"
  runtimeReady <- featureEnabled "merch.checkout.runtime_ready"
  datafastFlag <- featureEnabled "merch.checkout.datafast"
  paypalFlag <- featureEnabled "merch.checkout.paypal"
  manualFlag <- featureEnabled "merch.checkout.manual"
  datafastCredentials <- liftIO $ credentialsPresent
    ["DATAFAST_ENTITY_ID","DATAFAST_BEARER_TOKEN","DATAFAST_BASE_URL"]
  paypalCredentials <- liftIO $ credentialsPresent
    ["PAYPAL_CLIENT_ID","PAYPAL_CLIENT_SECRET","PAYPAL_WEBHOOK_ID","PAYPAL_MERCHANT_ID"]
  manualConfiguration <- liftIO $ credentialsPresent ["MERCH_BANK_TRANSFER_INSTRUCTIONS"]
  pure
    ( checkoutEnabled && runtimeReady && datafastFlag && datafastCredentials
    , checkoutEnabled && runtimeReady && paypalFlag && paypalCredentials
    , checkoutEnabled && runtimeReady && manualFlag && manualConfiguration
    )

requireConfiguredPaymentMethod :: AppM ()
requireConfiguredPaymentMethod = do
  (datafastAvailable, paypalAvailable, manualAvailable) <- merchPaymentMethodAvailability
  unless (datafastAvailable || paypalAvailable || manualAvailable) $
    throwError err503
      { errBody = "Merch checkout has no enabled and fully configured payment method"
      }

merchCapabilities :: AppM Value
merchCapabilities = do
  environment <- liftIO featureEnvironment
  pairs <- forM capabilityFlags $ \key -> do
    enabled <- featureEnabled key
    pure (key, enabled)
  (datafastAvailable, paypalAvailable, manualAvailable) <- merchPaymentMethodAvailability
  let enabled key = fromMaybe False (lookup key pairs)
      checkoutAvailable = datafastAvailable || paypalAvailable || manualAvailable
  pure $ object
    [ "environment" .= environment
    , "market" .= object ["countryCode" .= ("EC" :: Text), "currency" .= ("USD" :: Text)]
    , "features" .= object
        [ "storefronts" .= enabled "merch.storefronts"
        , "sellerApplications" .= enabled "merch.seller_applications"
        , "publicCatalog" .= enabled "merch.public_catalog"
        , "checkout" .= checkoutAvailable
        , "reviews" .= enabled "merch.reviews"
        , "notifications" .= enabled "merch.notifications"
        , "experimental" .= enabled "merch.experimental"
        ]
    , "paymentMethods" .= object
        [ "datafast" .= datafastAvailable
        , "paypal" .= paypalAvailable
        , "bankTransfer" .= manualAvailable
        ]
    , "automaticPayouts" .= False
    , "message" .= ("Artist merch is pilot-gated. Availability reflects both flags and configured provider capabilities." :: Text)
    ]
  where
    capabilityFlags =
      [ "merch.storefronts", "merch.seller_applications", "merch.public_catalog"
      , "merch.checkout", "merch.checkout.runtime_ready", "merch.checkout.datafast", "merch.checkout.paypal"
      , "merch.checkout.manual", "merch.reviews", "merch.notifications"
      , "merch.experimental"
      ]

merchPublicServer :: ServerT MerchPublicAPI AppM
merchPublicServer =
       merchCapabilities
  :<|> listPublicStorefronts
  :<|> getPublicStorefront
  :<|> getPublicProduct
  :<|> createCart
  :<|> getCart
  :<|> putCartItem
  :<|> deleteCartItem
  :<|> checkoutCart
  :<|> getPublicOrder
  :<|> createPublicIssue

listPublicStorefronts :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM [Value]
listPublicStorefronts rawQuery rawCategory rawLimit rawOffset = do
  requireFeature "merch.storefronts"
  requireFeature "merch.public_catalog"
  let query = T.take 160 (T.strip (fromMaybe "" rawQuery))
      category = T.toLower . T.strip <$> rawCategory
      limit = min 50 (max 1 (fromMaybe 24 rawLimit))
      offset = min 5000 (max 0 (fromMaybe 0 rawOffset))
  when (maybe False (`Set.notMember` allowedProductCategories) category) $
    throwError (badRequest "Unsupported merch category")
  jsonRows
    "SELECT jsonb_build_object(\
    \ 'id',store.id,'slug',store.slug,'displayName',store.display_name,\
    \ 'description',store.description,'coverImageUrl',store.cover_image_url,\
    \ 'logoImageUrl',store.logo_image_url,'countryCode',store.country_code,\
    \ 'currency',store.currency,'profile',jsonb_build_object(\
    \   'id',store.directory_profile_id,'slug',store.profile_slug,'name',store.profile_name,\
    \   'url','/directorio/'||store.profile_slug),\
    \ 'products',(SELECT coalesce(jsonb_agg(jsonb_build_object(\
    \   'id',product.id,'slug',product.slug,'name',product.name,'category',product.category,\
    \   'status',product.status,'availabilityMode',product.availability_mode,\
    \   'priceFromMinor',product.price_from_minor,'currency',product.currency,\
    \   'available',coalesce(product.available,FALSE),'imageUrl',CASE WHEN product.primary_image_object_key IS NULL THEN NULL ELSE '/assets/serve/'||product.primary_image_object_key END\
    \ ) ORDER BY product.published_at DESC,product.id),'[]'::jsonb)\
    \ FROM merch_public_product product WHERE product.store_id=store.id\
    \   AND (?::text IS NULL OR product.category=?::text)),\
    \ 'community',jsonb_build_object('profileUrl','/directorio/'||store.profile_slug,'canFollow',TRUE,'canRequestCollaboration',TRUE)\
    \) FROM merch_public_storefront store\
    \ WHERE (?='' OR directory_normalize_text(store.display_name||' '||coalesce(store.description,''))\
    \   LIKE '%'||directory_normalize_text(?)||'%')\
    \ AND (?::text IS NULL OR EXISTS(SELECT 1 FROM merch_public_product product WHERE product.store_id=store.id AND product.category=?::text))\
    \ ORDER BY store.display_name,store.id LIMIT ? OFFSET ?"
    [ optionalText category, optionalText category, PersistText query, PersistText query
    , optionalText category, optionalText category, PersistInt64 (fromIntegral limit), PersistInt64 (fromIntegral offset)
    ]

getPublicStorefront :: Text -> AppM Value
getPublicStorefront rawSlug = do
  requireFeature "merch.storefronts"
  requireFeature "merch.public_catalog"
  slug <- either (throwError . badRequest) pure (validateMerchSlug rawSlug)
  jsonOne err404
    "SELECT jsonb_build_object(\
    \ 'id',store.id,'slug',store.slug,'displayName',store.display_name,'description',store.description,\
    \ 'coverImageUrl',store.cover_image_url,'logoImageUrl',store.logo_image_url,\
    \ 'countryCode',store.country_code,'currency',store.currency,\
    \ 'canonicalUrl','/tienda/'||store.slug,\
    \ 'profile',jsonb_build_object('id',store.directory_profile_id,'slug',store.profile_slug,'name',store.profile_name,'url','/directorio/'||store.profile_slug),\
    \ 'policies',(SELECT jsonb_build_object('shipping',policy.shipping_policy,'returns',policy.return_policy,'preorder',policy.preorder_policy,'version',policy.version) FROM merch_store_policy policy WHERE policy.store_id=store.id AND policy.status='active'),\
    \ 'shippingZones',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',zone.id,'name',zone.name,'deliveryMethod',zone.delivery_method,'rateMinor',zone.rate_minor,'freeShippingMinMinor',zone.free_shipping_min_minor,'estimatedMinDays',zone.estimated_min_days,'estimatedMaxDays',zone.estimated_max_days) ORDER BY zone.name),'[]'::jsonb) FROM merch_shipping_zone zone WHERE zone.store_id=store.id AND zone.active),\
    \ 'products',(SELECT coalesce(jsonb_agg(jsonb_build_object(\
    \   'id',product.id,'storeId',product.store_id,'slug',product.slug,'name',product.name,\
    \   'description',product.description,'category',product.category,'status',product.status,\
    \   'availabilityMode',product.availability_mode,'preorderReleaseAt',product.preorder_release_at,\
    \   'buyerLimit',product.buyer_limit,'priceFromMinor',product.price_from_minor,\
    \   'currency',product.currency,'available',coalesce(product.available,FALSE),\
    \   'imageUrl',CASE WHEN product.primary_image_object_key IS NULL THEN NULL ELSE '/assets/serve/'||product.primary_image_object_key END\
    \ ) ORDER BY product.published_at DESC,product.id),'[]'::jsonb) FROM merch_public_product product WHERE product.store_id=store.id),\
    \ 'community',jsonb_build_object('profileUrl','/directorio/'||store.profile_slug,'canFollow',TRUE,'canRequestCollaboration',TRUE)\
    \) FROM merch_public_storefront store WHERE store.slug=?"
    [PersistText slug]

getPublicProduct :: Text -> Text -> AppM Value
getPublicProduct rawStoreSlug rawProductSlug = do
  requireFeature "merch.storefronts"
  requireFeature "merch.public_catalog"
  storeSlug <- either (throwError . badRequest) pure (validateMerchSlug rawStoreSlug)
  productSlug <- either (throwError . badRequest) pure (validateMerchSlug rawProductSlug)
  reviewsEnabled <- featureEnabled "merch.reviews"
  jsonOne err404
    "SELECT jsonb_build_object(\
    \ 'id',product.id,'storeId',product.store_id,'storeSlug',store.slug,'storeName',store.display_name,\
    \ 'slug',product.slug,'name',product.name,'description',product.description,'category',product.category,\
    \ 'status',product.status,'availabilityMode',product.availability_mode,'preorderReleaseAt',product.preorder_release_at,\
    \ 'buyerLimit',product.buyer_limit,'canonicalUrl','/tienda/'||store.slug||'/producto/'||product.slug,\
    \ 'images',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',image.id,'url','/assets/serve/'||image.object_key,'variants',image.variants,'altText',image.alt_text,'sortOrder',image.sort_order) ORDER BY image.sort_order,image.id),'[]'::jsonb) FROM merch_product_image image WHERE image.product_id=product.id AND image.deleted_at IS NULL AND image.scan_status='clean' AND image.moderation_status='allowed'),\
    \ 'variants',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',variant.id,'sku',variant.sku,'name',variant.name,'optionValues',variant.option_values,'priceMinor',variant.price_minor,'compareAtPriceMinor',variant.compare_at_price_minor,'currency',variant.currency,'weightGrams',variant.weight_grams,'stockMode',variant.stock_mode,'availableQuantity',CASE WHEN variant.stock_mode='made_to_order' THEN NULL ELSE greatest(0,variant.stock_on_hand-variant.stock_sold-variant.stock_reserved) END,'available',variant.stock_mode='made_to_order' OR variant.stock_on_hand-variant.stock_sold-variant.stock_reserved>0) ORDER BY variant.created_at,variant.id),'[]'::jsonb) FROM merch_product_variant variant WHERE variant.product_id=product.id AND variant.active),\
    \ 'related',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',related.id,'slug',related.slug,'name',related.name,'priceFromMinor',related.price_from_minor,'currency',related.currency,'imageUrl',CASE WHEN related.primary_image_object_key IS NULL THEN NULL ELSE '/assets/serve/'||related.primary_image_object_key END) ORDER BY related.published_at DESC),'[]'::jsonb) FROM (SELECT candidate.* FROM merch_public_product candidate WHERE candidate.store_id=product.store_id AND candidate.id<>product.id ORDER BY (candidate.category=product.category) DESC,candidate.published_at DESC LIMIT 4) related),\
    \ 'reviews',CASE WHEN ? THEN (SELECT coalesce(jsonb_agg(jsonb_build_object('id',review.id,'rating',review.rating,'body',review.body,'createdAt',review.created_at) ORDER BY review.created_at DESC),'[]'::jsonb) FROM merch_review review WHERE review.product_id=product.id AND review.status='published') ELSE '[]'::jsonb END,\
    \ 'reviewsEnabled',?\
    \) FROM merch_public_product product JOIN merch_public_storefront store ON store.id=product.store_id WHERE store.slug=? AND product.slug=?"
    [PersistBool reviewsEnabled, PersistBool reviewsEnabled, PersistText storeSlug, PersistText productSlug]

createCart :: MerchCartCreateRequest -> AppM Value
createCart MerchCartCreateRequest{..} = do
  requireFeature "merch.storefronts"
  requireFeature "merch.public_catalog"
  storeSlug <- either (throwError . badRequest) pure (validateMerchSlug mccStoreSlug)
  storeRows <- runDB (rawSql
    "SELECT id::text,currency FROM merch_public_storefront WHERE slug=?"
    [PersistText storeSlug] :: SqlPersistT IO [(Single Text,Single Text)])
  (storeId,currency) <- case storeRows of
    [(Single storeId,Single currency)] -> pure (storeId,currency)
    _ -> throwError err404
  cartId <- liftIO nextRandom
  token <- uuidText <$> liftIO nextRandom
  runDB $ rawExecute
    "INSERT INTO merch_cart(id,store_id,lookup_token_hash,currency) VALUES(?::uuid,?::uuid,?,?)"
    [PersistText (uuidText cartId),PersistText storeId,PersistText (hashText token),PersistText currency]
  loadCart cartId token (Just token)

requireLookupToken :: Text -> Maybe Text -> AppM Text
requireLookupToken label raw =
  requiredSafeText label 300 (fromMaybe "" raw)

loadCart :: UUID -> Text -> Maybe Text -> AppM Value
loadCart cartId token exposedToken =
  jsonOne err404
    "SELECT jsonb_build_object(\
    \ 'id',cart.id,'storeId',cart.store_id,'storeSlug',store.slug,'storeName',store.display_name,\
    \ 'status',cart.status,'currency',cart.currency,'expiresAt',cart.expires_at,'lookupToken',?::text,\
    \ 'items',(SELECT coalesce(jsonb_agg(jsonb_build_object('variantId',variant.id,'productId',product.id,'productSlug',product.slug,'productName',product.name,'variantName',variant.name,'sku',variant.sku,'quantity',item.quantity,'unitPriceMinor',variant.price_minor,'subtotalMinor',item.quantity::bigint*variant.price_minor,'available',variant.stock_mode='made_to_order' OR variant.stock_on_hand-variant.stock_sold-variant.stock_reserved>=item.quantity) ORDER BY item.created_at,item.variant_id),'[]'::jsonb) FROM merch_cart_item item JOIN merch_product_variant variant ON variant.id=item.variant_id JOIN merch_product product ON product.id=variant.product_id WHERE item.cart_id=cart.id),\
    \ 'productSubtotalMinor',(SELECT coalesce(sum(item.quantity::bigint*variant.price_minor),0) FROM merch_cart_item item JOIN merch_product_variant variant ON variant.id=item.variant_id WHERE item.cart_id=cart.id),\
    \ 'totalMinor',(SELECT coalesce(sum(item.quantity::bigint*variant.price_minor),0) FROM merch_cart_item item JOIN merch_product_variant variant ON variant.id=item.variant_id WHERE item.cart_id=cart.id)\
    \) FROM merch_cart cart JOIN merch_store store ON store.id=cart.store_id\
    \ WHERE cart.id=?::uuid AND cart.lookup_token_hash=?"
    [optionalText exposedToken,PersistText (uuidText cartId),PersistText (hashText token)]

getCart :: UUID -> Maybe Text -> AppM Value
getCart cartId rawToken = do
  token <- requireLookupToken "X-Cart-Lookup-Token" rawToken
  loadCart cartId token Nothing

putCartItem :: UUID -> Maybe Text -> MerchCartItemRequest -> AppM Value
putCartItem cartId rawToken MerchCartItemRequest{..} = do
  requireFeature "merch.storefronts"
  requireFeature "merch.public_catalog"
  token <- requireLookupToken "X-Cart-Lookup-Token" rawToken
  quantity <- either (throwError . badRequest) pure (validateQuantity mciQuantity)
  updated <- runDB (rawSql
    "WITH cart AS (SELECT id,store_id FROM merch_cart WHERE id=?::uuid AND lookup_token_hash=? AND status='active' AND expires_at>now()),\
    \ eligible AS (SELECT variant.id FROM cart JOIN merch_product product ON product.store_id=cart.store_id JOIN merch_product_variant variant ON variant.product_id=product.id WHERE variant.id=?::uuid AND variant.active AND product.status='published' AND (variant.stock_mode='made_to_order' OR variant.stock_on_hand-variant.stock_sold-variant.stock_reserved>=?)),\
    \ inserted AS (INSERT INTO merch_cart_item(cart_id,variant_id,quantity) SELECT cart.id,eligible.id,? FROM cart CROSS JOIN eligible ON CONFLICT(cart_id,variant_id) DO UPDATE SET quantity=EXCLUDED.quantity,updated_at=now() RETURNING 1) SELECT count(*) FROM inserted"
    [PersistText (uuidText cartId),PersistText (hashText token),PersistText (uuidText mciVariantId),PersistInt64 (fromIntegral quantity),PersistInt64 (fromIntegral quantity)]
    :: SqlPersistT IO [Single Int64])
  unless (updated == [Single 1]) $ throwError (conflict "Variant is unavailable, belongs to another seller, or stock changed")
  loadCart cartId token Nothing

deleteCartItem :: UUID -> UUID -> Maybe Text -> AppM Value
deleteCartItem cartId variantId rawToken = do
  token <- requireLookupToken "X-Cart-Lookup-Token" rawToken
  runDB $ rawExecute
    "DELETE FROM merch_cart_item item USING merch_cart cart WHERE item.cart_id=cart.id AND cart.id=?::uuid AND cart.lookup_token_hash=? AND item.variant_id=?::uuid AND cart.status='active'"
    [PersistText (uuidText cartId),PersistText (hashText token),PersistText (uuidText variantId)]
  loadCart cartId token Nothing

type CheckoutLineRow =
  ( Single Text, Single Text, Single Text, Single Text, Single Text
  , Single Int64, Single Int64, Single Int64, Single Int64, Single (Maybe Int)
  )

checkoutCart :: UUID -> Maybe Text -> Maybe Text -> MerchCheckoutRequest -> AppM Value
checkoutCart cartId rawToken rawIdempotency request@MerchCheckoutRequest{..} = do
  requireFeature "merch.storefronts"
  requireFeature "merch.public_catalog"
  requireFeature "merch.checkout"
  requireConfiguredPaymentMethod
  token <- requireLookupToken "X-Cart-Lookup-Token" rawToken
  idempotencyKey <- requireIdempotencyKey rawIdempotency
  recipient <- validateRecipient mcoRecipient
  when (fromMaybe False mcoCreateAccount) $
    throwError (badRequest "Account creation is offered after checkout and requires explicit password or federated identity consent")
  when (maybe False (`notElem` ["es","en"]) mcoLocale) $
    throwError (badRequest "locale must be es or en")
  now <- liftIO getCurrentTime
  environment <- liftIO featureEnvironment
  orderId <- liftIO nextRandom
  checkoutId <- liftIO nextRandom
  let expiresAt = addUTCTime (20 * 60) now
      requestHash = hashText (uuidText cartId <> ":" <> idempotencyKey <> ":" <> jsonText request)
  result <- runCheckoutDB $ do
    cartRows <- (rawSql
      "SELECT cart.store_id::text,cart.currency,cart.status FROM merch_cart cart WHERE cart.id=?::uuid AND cart.lookup_token_hash=? AND cart.expires_at>now() FOR UPDATE"
      [PersistText (uuidText cartId),PersistText (hashText token)]
      :: SqlPersistT IO [(Single Text,Single Text,Single Text)])
    case cartRows of
      [(Single storeId,Single currency,Single cartStatus)] | cartStatus `elem` ["active","checkout_started"] -> do
        existing <- (rawSql
          "SELECT id::text,create_request_sha256 FROM merch_order WHERE store_id=?::uuid AND create_idempotency_key=?"
          [PersistText storeId,PersistText idempotencyKey]
          :: SqlPersistT IO [(Single Text,Single Text)])
        case existing of
          [(Single existingId,Single existingHash)]
            | existingHash == requestHash -> pure (Right existingId)
            | otherwise -> pure (Left "Idempotency-Key conflicts with another checkout request")
          [] -> createCheckoutRows now expiresAt environment cartId token mcoShippingZoneId orderId checkoutId storeId currency recipient requestHash idempotencyKey
          _ -> pure (Left "Checkout idempotency state is ambiguous")
      _ -> pure (Left "Cart is missing, expired, or no longer editable")
  orderIdText <- either (throwError . conflict) pure result
  loadedId <- maybe (throwError (conflict "Stored checkout order id is invalid")) pure (UUID.fromText orderIdText)
  loadOrder loadedId token (Just token)

validateRecipient :: MerchRecipientRequest -> AppM MerchRecipientRequest
validateRecipient recipient@MerchRecipientRequest{..} = do
  _ <- requiredSafeText "recipient.name" 200 mrrName
  email <- requiredSafeText "recipient.email" 320 mrrEmail
  unless ("@" `T.isInfixOf` email && "." `T.isInfixOf` T.dropWhile (/='@') email) $
    throwError (badRequest "recipient.email is invalid")
  country <- requiredSafeText "recipient.countryCode" 2 mrrCountryCode
  when (T.toUpper country /= "EC") $ throwError (badRequest "The merch pilot currently supports delivery in Ecuador only")
  _ <- requiredSafeText "recipient.city" 120 mrrCity
  _ <- requiredSafeText "recipient.addressLine1" 300 mrrAddressLine1
  forM_ [mrrPhone,mrrSubdivision,mrrAddressLine2,mrrPostalCode,mrrDeliveryNote] $ \value ->
    forM_ value $ \txt -> when (T.length txt > 500 || T.any isControl txt) $
      throwError (badRequest "Recipient optional fields contain unsupported content")
  pure recipient { mrrEmail = T.toLower email, mrrCountryCode = "EC" }

createCheckoutRows
  :: UTCTime -> UTCTime -> Text -> UUID -> Text -> UUID -> UUID -> UUID -> Text -> Text -> MerchRecipientRequest
  -> Text -> Text -> SqlPersistT IO (Either Text Text)
createCheckoutRows now expiresAt environment cartId token shippingZoneId orderId checkoutId storeId currency recipient requestHash idempotencyKey = do
  -- Serialise buyer-limit evaluation for this seller/email pair. This prevents
  -- concurrent checkout intents from each observing the same remaining limit.
  _ <- (rawSql "SELECT 1 FROM pg_advisory_xact_lock(hashtextextended(?,0))"
    [PersistText (storeId <> ":" <> T.toLower (mrrEmail recipient))]
    :: SqlPersistT IO [Single Int])
  linesFound <- (rawSql
    "SELECT variant.id::text,product.id::text,product.name,variant.name,variant.sku,variant.price_minor,item.quantity::bigint,variant.version,product.version,product.buyer_limit\
    \ FROM merch_cart_item item JOIN merch_product_variant variant ON variant.id=item.variant_id\
    \ JOIN merch_product product ON product.id=variant.product_id\
    \ JOIN merch_store store ON store.id=product.store_id\
    \ WHERE item.cart_id=?::uuid AND product.store_id=?::uuid AND product.status='published' AND variant.active\
    \ AND store.application_status='approved' AND store.operational_status='active'\
    \ ORDER BY variant.id FOR UPDATE OF variant"
    [PersistText (uuidText cartId),PersistText storeId] :: SqlPersistT IO [CheckoutLineRow])
  -- The cart is locked by checkoutCart; bind lines to its unique active cart by
  -- the caller's idempotency transaction. A second seller can never enter due
  -- to merch_cart_item_store_trigger and the repeated store predicate.
  if null linesFound then pure (Left "Cart has no purchasable items") else do
    buyerLimitExceeded <- (rawSql
      "SELECT EXISTS(\
      \ SELECT 1 FROM merch_cart_item item\
      \ JOIN merch_product_variant variant ON variant.id=item.variant_id\
      \ JOIN merch_product product ON product.id=variant.product_id\
      \ WHERE item.cart_id=?::uuid AND product.buyer_limit IS NOT NULL AND (\
      \   item.quantity + coalesce((SELECT sum(line.quantity) FROM merch_order_line line\
      \     JOIN merch_order prior_order ON prior_order.id=line.order_id\
      \     WHERE line.product_id=product.id AND lower(prior_order.customer_email)=lower(?)\
      \       AND prior_order.commercial_status<>'cancelled'\
      \       AND prior_order.payment_status NOT IN ('failed','cancelled')),0)\
      \ ) > product.buyer_limit)"
      [PersistText (uuidText cartId), PersistText (mrrEmail recipient)]
      :: SqlPersistT IO [Single Bool])
    when (buyerLimitExceeded == [Single True]) $
      liftIO (ioError (userError "A per-buyer product limit would be exceeded"))
    let subtotal = sum [price * quantity | (_,_,_,_,_,Single price,Single quantity,_,_,_) <- linesFound]
    zoneRows <- (rawSql
      "SELECT zone.delivery_method,CASE WHEN zone.free_shipping_min_minor IS NOT NULL AND ? >= zone.free_shipping_min_minor THEN 0 ELSE zone.rate_minor END,zone.id::text\
      \ FROM merch_shipping_zone zone WHERE zone.id=?::uuid AND zone.store_id=?::uuid AND zone.active"
      [PersistInt64 subtotal,PersistText (uuidText shippingZoneId),PersistText storeId]
      :: SqlPersistT IO [(Single Text,Single Int64,Single Text)])
    policyRows <- (rawSql
      "SELECT id::text,version,shipping_policy,return_policy,preorder_policy FROM merch_store_policy WHERE store_id=?::uuid AND status='active'"
      [PersistText storeId]
      :: SqlPersistT IO [(Single Text,Single Int64,Single Text,Single Text,Single (Maybe Text))])
    commissionRows <- (rawSql "SELECT merch_calculate_commission_bps(?::uuid,?)"
      [PersistText storeId,PersistUTCTime now] :: SqlPersistT IO [Single Int])
    case (zoneRows,policyRows,commissionRows) of
      ([(Single shippingMethod,Single shipping,Single zoneId)],[(Single policyId,Single policyVersion,Single shippingPolicy,Single returnPolicy,Single preorderPolicy)],[Single commissionBps]) -> do
        case calculateMerchMoney subtotal 0 0 shipping 0 commissionBps of
          Left message -> pure (Left message)
          Right money -> do
            let orderIdText = uuidText orderId
                checkoutIdText = uuidText checkoutId
                orderNumber = "TDF-MERCH-" <> T.toUpper (T.take 12 (T.filter (/='-') orderIdText))
                recipientSnapshot = toJSON recipient
                shippingSnapshot = object ["zoneId" .= zoneId,"deliveryMethod" .= shippingMethod,"rateMinor" .= shipping]
                policySnapshot = object ["id" .= policyId,"version" .= policyVersion,"shipping" .= shippingPolicy,"returns" .= returnPolicy,"preorder" .= preorderPolicy]
                commissionSnapshot = object ["basis" .= ("product_subtotal_after_discount" :: Text),"commissionBps" .= commissionBps]
            rawExecute
              "INSERT INTO merch_order(id,order_number,store_id,cart_id,customer_email,customer_name,customer_phone,lookup_token_hash,currency,product_subtotal_minor,discount_minor,tax_minor,shipping_minor,processor_fee_minor,tdf_commission_bps,tdf_commission_minor,seller_net_minor,total_minor,shipping_method,shipping_zone_snapshot,recipient_snapshot,policy_snapshot,commission_snapshot,create_idempotency_key,create_request_sha256)\
              \ VALUES(?::uuid,?,?::uuid,?::uuid,?,?,?,?,?, ?,0,0,?,0,?,?,?,?,?,?::jsonb,?::jsonb,?::jsonb,?::jsonb,?,?)"
              [ PersistText orderIdText,PersistText orderNumber,PersistText storeId,PersistText (uuidText cartId)
              , PersistText (mrrEmail recipient),PersistText (mrrName recipient),optionalText (mrrPhone recipient)
              , PersistText (hashText token),PersistText currency,PersistInt64 (merchProductSubtotalMinor money)
              , PersistInt64 (merchShippingMinor money),PersistInt64 (fromIntegral (merchCommissionBps money))
              , PersistInt64 (merchCommissionMinor money),PersistInt64 (merchSellerNetMinor money),PersistInt64 (merchTotalMinor money)
              , PersistText shippingMethod,PersistText (jsonText shippingSnapshot),PersistText (jsonText recipientSnapshot)
              , PersistText (jsonText policySnapshot),PersistText (jsonText commissionSnapshot)
              , PersistText idempotencyKey,PersistText requestHash
              ]
            forM_ (zip [1 :: Int64 ..] linesFound) $ \(lineNumber,(Single variantId,Single productId,Single productName,Single variantName,Single sku,Single price,Single quantity,Single variantVersion,Single productVersion,_)) -> do
              let productSnapshot = object ["id" .= productId,"name" .= productName,"version" .= productVersion]
                  variantSnapshot = object ["id" .= variantId,"name" .= variantName,"sku" .= sku,"version" .= variantVersion]
                  lineSubtotal = price * quantity
              rawExecute
                "INSERT INTO merch_order_line(order_id,line_number,product_id,variant_id,quantity,unit_price_minor,subtotal_minor,total_minor,product_snapshot,variant_snapshot,policy_snapshot) VALUES(?::uuid,?,?::uuid,?::uuid,?,?,?,?,?::jsonb,?::jsonb,?::jsonb)"
                [PersistText orderIdText,PersistInt64 lineNumber,PersistText productId,PersistText variantId,PersistInt64 quantity,PersistInt64 price,PersistInt64 lineSubtotal,PersistInt64 lineSubtotal,PersistText (jsonText productSnapshot),PersistText (jsonText variantSnapshot),PersistText (jsonText policySnapshot)]
            rawExecute
              "INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,fee_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at) VALUES(?::uuid,'merch_order',?,'holding',?,?,?,?,?,?,?,?,?)"
              [PersistText checkoutIdText,PersistText orderIdText,PersistText environment,PersistText currency,PersistInt64 (merchProductSubtotalMinor money),PersistInt64 (merchShippingMinor money),PersistInt64 (merchTotalMinor money),PersistText (mrrEmail recipient),PersistText (hashText token),PersistText idempotencyKey,PersistUTCTime expiresAt]
            forM_ (zip [1 :: Int64 ..] linesFound) $ \(lineNumber,(Single variantId,_,Single productName,Single variantName,Single sku,Single price,Single quantity,Single variantVersion,_,_)) -> do
              let lineSubtotal = price * quantity
              rawExecute
                "INSERT INTO commerce_checkout_line_item(checkout_id,line_number,product_type,product_id,product_version,description,quantity,unit_amount_minor,subtotal_minor,total_minor,snapshot) VALUES(?::uuid,?,'merch_variant',?,?,?, ?,?,?,?,?::jsonb)"
                [PersistText checkoutIdText,PersistInt64 lineNumber,PersistText variantId,PersistText (T.pack (show variantVersion)),PersistText (productName<>" — "<>variantName),PersistInt64 quantity,PersistInt64 price,PersistInt64 lineSubtotal,PersistInt64 lineSubtotal,PersistText (jsonText (object ["sku" .= sku,"storeId" .= storeId]))]
            when (shipping > 0) $ rawExecute
              "INSERT INTO commerce_checkout_line_item(checkout_id,line_number,product_type,product_id,product_version,description,quantity,unit_amount_minor,subtotal_minor,total_minor,snapshot) VALUES(?::uuid,?,'merch_shipping',?,'1','Envío',1,?,?,?,?::jsonb)"
              [PersistText checkoutIdText,PersistInt64 (fromIntegral (length linesFound+1)),PersistText zoneId,PersistInt64 shipping,PersistInt64 shipping,PersistInt64 shipping,PersistText (jsonText shippingSnapshot)]
            rawExecute "UPDATE merch_order SET checkout_id=?::uuid WHERE id=?::uuid" [PersistText checkoutIdText,PersistText orderIdText]
            let reservations = [object ["variantId" .= variantId,"quantity" .= quantity] | (Single variantId,_,_,_,_,_,Single quantity,_,_,_) <- linesFound]
            _ <- (rawSql "SELECT id::text FROM merch_reserve_stock(?::uuid,?::uuid,?::jsonb,?)"
              [PersistText orderIdText,PersistText checkoutIdText,PersistText (jsonText reservations),PersistUTCTime expiresAt]
              :: SqlPersistT IO [Single Text])
            rawExecute "UPDATE merch_cart SET status='converted',updated_at=now() WHERE id IN (SELECT cart_id FROM merch_order WHERE id=?::uuid)" [PersistText orderIdText]
            rawExecute "INSERT INTO merch_fulfillment_event(order_id,event_type,metadata) VALUES(?::uuid,'order_created',?::jsonb)" [PersistText orderIdText,PersistText (jsonText (object ["paymentStatus" .= ("pending"::Text)]))]
            pure (Right orderIdText)
      _ -> pure (Left "An active shipping zone, store policy, and commission policy are required")

loadOrder :: UUID -> Text -> Maybe Text -> AppM Value
loadOrder orderId token exposedToken =
  jsonOne err404
    "SELECT jsonb_build_object(\
    \ 'id',order_record.id,'orderNumber',order_record.order_number,'storeId',order_record.store_id,\
    \ 'storeName',store.display_name,'storeSlug',store.slug,'currency',order_record.currency,\
    \ 'productSubtotalMinor',order_record.product_subtotal_minor,'discountMinor',order_record.discount_minor,\
    \ 'taxMinor',order_record.tax_minor,'shippingMinor',order_record.shipping_minor,'totalMinor',order_record.total_minor,\
    \ 'commercialStatus',order_record.commercial_status,'paymentStatus',order_record.payment_status,\
    \ 'fulfillmentStatus',order_record.fulfillment_status,'refundStatus',order_record.refund_status,\
    \ 'disputeStatus',order_record.dispute_status,'shippingMethod',order_record.shipping_method,\
    \ 'recipient',order_record.recipient_snapshot,'policies',order_record.policy_snapshot,\
    \ 'lookupToken',?::text,'createdAt',order_record.created_at,'updatedAt',order_record.updated_at,\
    \ 'lines',(SELECT jsonb_agg(jsonb_build_object('id',line.id,'quantity',line.quantity,'unitPriceMinor',line.unit_price_minor,'subtotalMinor',line.subtotal_minor,'totalMinor',line.total_minor,'product',line.product_snapshot,'variant',line.variant_snapshot) ORDER BY line.line_number) FROM merch_order_line line WHERE line.order_id=order_record.id),\
    \ 'shipment',(SELECT jsonb_build_object('carrier',shipment.carrier,'trackingNumber',shipment.tracking_number,'trackingUrl',shipment.tracking_url,'status',shipment.status,'shippedAt',shipment.shipped_at,'deliveredAt',shipment.delivered_at) FROM merch_shipment shipment WHERE shipment.order_id=order_record.id ORDER BY shipment.created_at DESC LIMIT 1),\
    \ 'timeline',(SELECT coalesce(jsonb_agg(jsonb_build_object('eventType',event.event_type,'fromStatus',event.from_status,'toStatus',event.to_status,'publicNote',event.public_note,'createdAt',event.created_at) ORDER BY event.created_at,event.id),'[]'::jsonb) FROM merch_fulfillment_event event WHERE event.order_id=order_record.id)\
    \) FROM merch_order order_record JOIN merch_store store ON store.id=order_record.store_id\
    \ WHERE order_record.id=?::uuid AND order_record.lookup_token_hash=?"
    [optionalText exposedToken,PersistText (uuidText orderId),PersistText (hashText token)]

getPublicOrder :: UUID -> Maybe Text -> AppM Value
getPublicOrder orderId rawToken = do
  token <- requireLookupToken "X-Order-Lookup-Token" rawToken
  loadOrder orderId token Nothing

createPublicIssue :: UUID -> Maybe Text -> Maybe Text -> MerchIssueRequest -> AppM Value
createPublicIssue orderId rawToken rawIdempotency MerchIssueRequest{..} = do
  token <- requireLookupToken "X-Order-Lookup-Token" rawToken
  idempotency <- requireIdempotencyKey rawIdempotency
  message <- requiredSafeText "message" 5000 mirMessage
  let allowed = ["general","address","stock","shipping","damaged","missing","cancellation","return","refund","dispute"]
      issueType = T.toLower (T.strip mirIssueType)
      fingerprint = hashText (issueType <> ":" <> message)
  unless (issueType `elem` allowed) $ throwError (badRequest "Unsupported issueType")
  issueId <- liftIO nextRandom
  created <- runDB (rawSql
    "INSERT INTO merch_order_issue(id,order_id,opened_by_type,issue_type,public_message,idempotency_key,request_sha256)\
    \ SELECT ?::uuid,order_record.id,'buyer',?,?,?,? FROM merch_order order_record WHERE order_record.id=?::uuid AND order_record.lookup_token_hash=?\
    \ ON CONFLICT(order_id,idempotency_key) DO UPDATE SET idempotency_key=EXCLUDED.idempotency_key\
    \ WHERE merch_order_issue.request_sha256=EXCLUDED.request_sha256\
    \ RETURNING jsonb_build_object('id',id,'orderId',order_id,'issueType',issue_type,'status',status,'message',public_message,'createdAt',created_at)"
    [PersistText (uuidText issueId),PersistText issueType,PersistText message,PersistText idempotency,PersistText fingerprint,PersistText (uuidText orderId),PersistText (hashText token)]
    :: SqlPersistT IO [Single CMS.AesonValue])
  case created of
    [Single value] -> pure (CMS.unAesonValue value)
    _ -> do
      authorized <- runDB (rawSql "SELECT TRUE FROM merch_order WHERE id=?::uuid AND lookup_token_hash=?"
        [PersistText (uuidText orderId), PersistText (hashText token)] :: SqlPersistT IO [Single Bool])
      if null authorized then throwError err404 else throwError (conflict "Idempotency-Key conflicts with another issue request")

merchProtectedServer :: AuthedUser -> ServerT MerchProtectedAPI AppM
merchProtectedServer user =
       addFavorite user
  :<|> removeFavorite user
  :<|> listSellerStores user
  :<|> submitStoreApplication user
  :<|> updateStore user
  :<|> listMembers user
  :<|> inviteMember user
  :<|> updateMember user
  :<|> createPolicy user
  :<|> createShippingZone user
  :<|> listManagedProducts user
  :<|> createProduct user
  :<|> updateProduct user
  :<|> updateProductStatus user
  :<|> uploadProductImage user
  :<|> updateVariantStock user
  :<|> listSellerOrders user
  :<|> updateFulfillment user
  :<|> listAdminStores user
  :<|> reviewStore user
  :<|> listAdminProducts user
  :<|> reviewProduct user
  :<|> createSettlement user
  :<|> updateSettlementStatus user

addFavorite :: AuthedUser -> UUID -> AppM NoContent
addFavorite user productId = do
  requireFeature "merch.public_catalog"
  runDB $ rawExecute
    "INSERT INTO merch_favorite(party_id,product_id) SELECT ?,product.id FROM merch_public_product product WHERE product.id=?::uuid ON CONFLICT DO NOTHING"
    [PersistInt64 (currentPartyId user),PersistText (uuidText productId)]
  pure NoContent

removeFavorite :: AuthedUser -> UUID -> AppM NoContent
removeFavorite user productId = do
  runDB $ rawExecute "DELETE FROM merch_favorite WHERE party_id=? AND product_id=?::uuid"
    [PersistInt64 (currentPartyId user),PersistText (uuidText productId)]
  pure NoContent

requireAdmin :: AuthedUser -> AppM ()
requireAdmin user = unless (hasStrictAdminAccess user) (throwError err403)

requireStorePermission :: AuthedUser -> UUID -> Text -> AppM ()
requireStorePermission user storeId permission = do
  allowed <- hasStorePermission user storeId permission
  unless allowed $ throwError err403

hasStorePermission :: AuthedUser -> UUID -> Text -> AppM Bool
hasStorePermission user storeId permission = do
  column <- case permission of
    "catalog" -> pure "can_catalog"
    "stock" -> pure "can_stock"
    "orders" -> pure "can_orders"
    "fulfillment" -> pure "can_fulfillment"
    "finance" -> pure "can_finance"
    "settings" -> pure "can_settings"
    _ -> throwError err500 { errBody = "Unknown merch permission" }
  rows <- runDB (rawSql
    ("SELECT TRUE FROM merch_store_member WHERE store_id=?::uuid AND party_id=? AND invitation_status='accepted' AND "<>column<>"=TRUE")
    [PersistText (uuidText storeId),PersistInt64 (currentPartyId user)] :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

listSellerStores :: AuthedUser -> AppM [Value]
listSellerStores user = jsonRows
  "SELECT jsonb_build_object('id',store.id,'profileId',store.directory_profile_id,'slug',store.slug,'displayName',store.display_name,'description',store.description,'applicationStatus',store.application_status,'operationalStatus',store.operational_status,'reviewerNotes',store.reviewer_notes,'version',store.version,'permissions',jsonb_build_object('catalog',member.can_catalog,'stock',member.can_stock,'orders',member.can_orders,'fulfillment',member.can_fulfillment,'finance',member.can_finance,'settings',member.can_settings)) FROM merch_store_member member JOIN merch_store store ON store.id=member.store_id WHERE member.party_id=? AND member.invitation_status='accepted' ORDER BY store.updated_at DESC"
  [PersistInt64 (currentPartyId user)]

submitStoreApplication :: AuthedUser -> Maybe Text -> MerchStoreApplicationRequest -> AppM Value
submitStoreApplication user rawIdempotency request@MerchStoreApplicationRequest{..} = do
  requireFeature "merch.seller_applications"
  idempotency <- requireIdempotencyKey rawIdempotency
  slug <- either (throwError . badRequest) pure (validateMerchSlug msaSlug)
  name <- requiredSafeText "displayName" 160 msaDisplayName
  note <- requiredSafeText "applicationNote" 2000 msaApplicationNote
  when (T.length note < 10) $ throwError (badRequest "applicationNote must contain at least 10 characters")
  forM_ msaDescription $ \description -> when (T.length description > 2000 || T.any isControl description) $
    throwError (badRequest "description must contain no more than 2000 safe characters")
  storeId <- liftIO nextRandom
  let fingerprint = hashText (jsonText request)
  existing <- findStoreApplication msaProfileId
  storeIdText <- case existing of
    [(Single existingId, Single existingKey, Single existingFingerprint)]
      | existingKey == idempotency && existingFingerprint == fingerprint -> pure existingId
      | otherwise -> throwError (conflict "This profile already has a different store application")
    [] -> do
      pool <- asks envPool
      inserted <- liftIO $ tryAny $ runSqlPool (rawSql
        "INSERT INTO merch_store(id,directory_profile_id,seller_party_id,primary_owner_party_id,slug,display_name,description,application_note,application_idempotency_key,application_request_sha256) \
        \SELECT ?::uuid,profile.id,profile.subject_party_id,?,?,?,?,?,?,? \
        \FROM directory_profile profile \
        \JOIN directory_profile_manager manager ON manager.profile_id=profile.id AND manager.account_party_id=? AND manager.active AND manager.can_manage \
        \WHERE profile.id=?::uuid \
        \RETURNING id::text"
        [ PersistText (uuidText storeId), PersistInt64 (currentPartyId user), PersistText slug, PersistText name
        , optionalText msaDescription, PersistText note, PersistText idempotency, PersistText fingerprint
        , PersistInt64 (currentPartyId user), PersistText (uuidText msaProfileId)
        ] :: SqlPersistT IO [Single Text]) pool
      case inserted of
        Right [Single value] -> pure value
        Right _ -> throwError (conflict "The selected profile is not eligible for a merch store")
        Left _ -> do
          raced <- findStoreApplication msaProfileId
          case raced of
            [(Single existingId, Single existingKey, Single existingFingerprint)]
              | existingKey == idempotency && existingFingerprint == fingerprint -> pure existingId
            _ -> throwError (conflict "The profile or store slug is already in use, or the profile is not eligible")
    _ -> throwError err500 { errBody = "Store application invariant violated" }
  loadManagedStore storeIdText

findStoreApplication :: UUID -> AppM [(Single Text, Single Text, Single Text)]
findStoreApplication profileId = runDB (rawSql
  "SELECT id::text,application_idempotency_key,application_request_sha256 FROM merch_store WHERE directory_profile_id=?::uuid"
  [PersistText (uuidText profileId)])

loadManagedStore :: Text -> AppM Value
loadManagedStore storeId = jsonOne err404
  "SELECT jsonb_build_object('id',store.id,'profileId',store.directory_profile_id,'sellerPartyId',store.seller_party_id,'primaryOwnerPartyId',store.primary_owner_party_id,'slug',store.slug,'displayName',store.display_name,'description',store.description,'coverImageUrl',store.cover_image_url,'logoImageUrl',store.logo_image_url,'countryCode',store.country_code,'currency',store.currency,'applicationStatus',store.application_status,'operationalStatus',store.operational_status,'applicationNote',store.application_note,'reviewerNotes',store.reviewer_notes,'requestedAt',store.requested_at,'reviewedAt',store.reviewed_at,'activatedAt',store.activated_at,'suspendedAt',store.suspended_at,'suspensionReason',store.suspension_reason,'version',store.version) FROM merch_store store WHERE store.id=?::uuid"
  [PersistText storeId]

updateStore :: AuthedUser -> UUID -> MerchStoreUpdateRequest -> AppM Value
updateStore user storeId MerchStoreUpdateRequest{..} = do
  requireStorePermission user storeId "settings"
  name <- requiredSafeText "displayName" 160 msuDisplayName
  forM_ (catMaybes [msuDescription]) $ \value ->
    when (T.length value > 2000 || T.any isControl value) $ throwError (badRequest "Store settings contain unsupported content")
  forM_ [msuCoverImageUrl, msuLogoImageUrl] $ \candidate ->
    forM_ candidate $ \value -> unless (("/assets/serve/merch/" <> uuidText storeId <> "/") `T.isPrefixOf` value) $
      throwError (badRequest "Store images must use a durable TDF merch asset reference")
  runDB $ do
    rawExecute
      "UPDATE merch_store SET display_name=?,description=?,cover_image_url=?,logo_image_url=?,updated_at=now(),version=version+1 WHERE id=?::uuid"
      [PersistText name,optionalText msuDescription,optionalText msuCoverImageUrl,optionalText msuLogoImageUrl,PersistText (uuidText storeId)]
    appendMerchAudit user "seller" (Just storeId) "store.settings_updated" "store" (uuidText storeId)
      (object ["displayName" .= name])
  loadManagedStore (uuidText storeId)

listMembers :: AuthedUser -> UUID -> AppM [Value]
listMembers user storeId = do
  requireStorePermission user storeId "settings"
  jsonRows
    "SELECT jsonb_build_object('id',member.id,'partyId',member.party_id,'displayName',party.display_name,'username',credential.username,'role',member.member_role,'status',member.invitation_status,'permissions',jsonb_build_object('catalog',member.can_catalog,'stock',member.can_stock,'orders',member.can_orders,'fulfillment',member.can_fulfillment,'finance',member.can_finance,'settings',member.can_settings),'expiresAt',member.expires_at,'version',member.version) FROM merch_store_member member JOIN party ON party.id=member.party_id LEFT JOIN LATERAL(SELECT username FROM user_credential WHERE party_id=party.id AND active ORDER BY id LIMIT 1) credential ON TRUE WHERE member.store_id=?::uuid ORDER BY member.member_role,party.display_name,member.id"
    [PersistText (uuidText storeId)]

permissionValues :: MerchMemberPermissions -> [PersistValue]
permissionValues MerchMemberPermissions{..} =
  map PersistBool [mmpCatalog,mmpStock,mmpOrders,mmpFulfillment,mmpFinance,mmpSettings]

inviteMember :: AuthedUser -> UUID -> Maybe Text -> MerchMemberInviteRequest -> AppM Value
inviteMember user storeId rawIdempotency MerchMemberInviteRequest{..} = do
  requireStorePermission user storeId "settings"
  idempotency <- requireIdempotencyKey rawIdempotency
  when (mmiPartyId == currentPartyId user) $ throwError (badRequest "Owner is already a store member")
  memberId <- liftIO nextRandom
  let permissions = permissionValues mmiPermissions
      fingerprint = hashText (jsonText (object ["partyId" .= mmiPartyId, "permissions" .= mmiPermissions]))
  inserted <- runDB $ do
    rows <- (rawSql
      "INSERT INTO merch_store_member(id,store_id,party_id,member_role,invitation_status,can_catalog,can_stock,can_orders,can_fulfillment,can_finance,can_settings,invited_by,invitation_idempotency_key,invitation_request_sha256,expires_at)\
    \ SELECT ?::uuid,?::uuid,party.id,'collaborator','pending',?,?,?,?,?,?,?,?,?,now()+interval '7 days' FROM party JOIN user_credential credential ON credential.party_id=party.id AND credential.active WHERE party.id=?\
    \ ON CONFLICT(store_id,party_id) DO UPDATE SET invitation_status='pending',can_catalog=EXCLUDED.can_catalog,can_stock=EXCLUDED.can_stock,can_orders=EXCLUDED.can_orders,can_fulfillment=EXCLUDED.can_fulfillment,can_finance=EXCLUDED.can_finance,can_settings=EXCLUDED.can_settings,invited_by=EXCLUDED.invited_by,invitation_idempotency_key=EXCLUDED.invitation_idempotency_key,invitation_request_sha256=EXCLUDED.invitation_request_sha256,expires_at=EXCLUDED.expires_at,revoked_at=NULL,revoke_reason=NULL,updated_at=now(),version=merch_store_member.version+1\
    \ WHERE (merch_store_member.invitation_idempotency_key=EXCLUDED.invitation_idempotency_key AND merch_store_member.invitation_request_sha256=EXCLUDED.invitation_request_sha256) OR merch_store_member.invitation_status IN ('declined','expired','revoked') RETURNING id::text"
      ([PersistText (uuidText memberId),PersistText (uuidText storeId)]<>permissions<>
       [PersistInt64 (currentPartyId user),PersistText idempotency,PersistText fingerprint,PersistInt64 mmiPartyId])
      :: SqlPersistT IO [Single Text])
    unless (null rows) $ appendMerchAudit user "seller" (Just storeId) "store.member_invited" "member" (T.pack (show mmiPartyId))
      (object ["partyId" .= mmiPartyId, "permissions" .= mmiPermissions])
    pure rows
  when (null inserted) $ throwError (conflict "This collaborator already has an active invitation; update or revoke it before sending another")
  jsonOne err404
    "SELECT jsonb_build_object('id',member.id,'partyId',member.party_id,'displayName',party.display_name,'status',member.invitation_status,'expiresAt',member.expires_at) FROM merch_store_member member JOIN party ON party.id=member.party_id WHERE member.store_id=?::uuid AND member.party_id=?"
    [PersistText (uuidText storeId),PersistInt64 mmiPartyId]

updateMember :: AuthedUser -> UUID -> UUID -> MerchMemberUpdateRequest -> AppM Value
updateMember user storeId memberId MerchMemberUpdateRequest{..} = do
  let status = T.toLower (T.strip mmuStatus)
  memberRows <- runDB (rawSql
    "SELECT party_id,member_role,invitation_status FROM merch_store_member WHERE id=?::uuid AND store_id=?::uuid"
    [PersistText (uuidText memberId),PersistText (uuidText storeId)]
    :: SqlPersistT IO [(Single Int64,Single Text,Single Text)])
  (memberParty,role,oldStatus) <- case memberRows of
    [row] -> pure row
    _ -> throwError err404
  let self = memberParty == Single (currentPartyId user)
  if self && oldStatus == Single "pending" && status `elem` ["accepted","declined"]
    then runDB $ rawExecute
      "UPDATE merch_store_member SET invitation_status=?,accepted_at=CASE WHEN ?='accepted' THEN now() ELSE accepted_at END,updated_at=now(),version=version+1 WHERE id=?::uuid"
      [PersistText status,PersistText status,PersistText (uuidText memberId)]
    else do
      requireStorePermission user storeId "settings"
      when (role == Single "owner") $ throwError (conflict "Primary owner cannot be revoked through collaborator controls")
      unless (status `elem` ["accepted","revoked"]) $ throwError (badRequest "Manager status must be accepted or revoked")
      let permissions = maybe (replicate 6 PersistNull) permissionValues mmuPermissions
      runDB $ rawExecute
        "UPDATE merch_store_member SET invitation_status=?,can_catalog=coalesce(?::boolean,can_catalog),can_stock=coalesce(?::boolean,can_stock),can_orders=coalesce(?::boolean,can_orders),can_fulfillment=coalesce(?::boolean,can_fulfillment),can_finance=coalesce(?::boolean,can_finance),can_settings=coalesce(?::boolean,can_settings),accepted_at=CASE WHEN ?='accepted' THEN coalesce(accepted_at,now()) ELSE accepted_at END,revoked_at=CASE WHEN ?='revoked' THEN now() ELSE NULL END,revoke_reason=CASE WHEN ?='revoked' THEN ? ELSE NULL END,updated_at=now(),version=version+1 WHERE id=?::uuid"
        ([PersistText status]<>permissions<>
         [PersistText status,PersistText status,PersistText status,optionalText mmuReason,PersistText (uuidText memberId)])
  runDB $ appendMerchAudit user "seller" (Just storeId) "store.member_updated" "member" (uuidText memberId)
    (object ["status" .= status])
  jsonOne err404 "SELECT jsonb_build_object('id',id,'partyId',party_id,'role',member_role,'status',invitation_status,'version',version) FROM merch_store_member WHERE id=?::uuid" [PersistText (uuidText memberId)]

createPolicy :: AuthedUser -> UUID -> MerchPolicyRequest -> AppM Value
createPolicy user storeId MerchPolicyRequest{..} = do
  requireStorePermission user storeId "settings"
  shipping <- requiredSafeText "shippingPolicy" 5000 mprShippingPolicy
  returns <- requiredSafeText "returnPolicy" 5000 mprReturnPolicy
  when (T.length shipping < 20 || T.length returns < 20) $ throwError (badRequest "Shipping and return policies must contain at least 20 characters")
  policyId <- liftIO nextRandom
  runDB $ do
    rawExecute "UPDATE merch_store_policy SET status='superseded' WHERE store_id=?::uuid AND status='active'" [PersistText (uuidText storeId)]
    rawExecute
      "INSERT INTO merch_store_policy(id,store_id,version,shipping_policy,return_policy,preorder_policy,support_email,status,effective_at,created_by) SELECT ?::uuid,?::uuid,coalesce(max(version),0)+1,?,?,?,?,'active',now(),? FROM merch_store_policy WHERE store_id=?::uuid"
      [PersistText (uuidText policyId),PersistText (uuidText storeId),PersistText shipping,PersistText returns,optionalText mprPreorderPolicy,optionalText mprSupportEmail,PersistInt64 (currentPartyId user),PersistText (uuidText storeId)]
  jsonOne err404 "SELECT jsonb_build_object('id',id,'storeId',store_id,'version',version,'shippingPolicy',shipping_policy,'returnPolicy',return_policy,'preorderPolicy',preorder_policy,'supportEmail',support_email,'status',status,'effectiveAt',effective_at) FROM merch_store_policy WHERE id=?::uuid" [PersistText (uuidText policyId)]

createShippingZone :: AuthedUser -> UUID -> MerchShippingZoneRequest -> AppM Value
createShippingZone user storeId MerchShippingZoneRequest{..} = do
  requireStorePermission user storeId "settings"
  name <- requiredSafeText "name" 120 mszName
  unless (T.toUpper mszCountryCode == "EC") $ throwError (badRequest "The pilot currently supports Ecuador shipping zones only")
  unless (mszDeliveryMethod `elem` ["coordinated_pickup","national_shipping"]) $ throwError (badRequest "Unsupported deliveryMethod")
  when (mszRateMinor < 0 || maybe False (<=0) mszFreeShippingMinMinor) $ throwError (badRequest "Shipping rates are invalid")
  zoneId <- liftIO nextRandom
  runDB $ rawExecute
    "INSERT INTO merch_shipping_zone(id,store_id,name,country_code,subdivision_codes,delivery_method,rate_minor,free_shipping_min_minor,estimated_min_days,estimated_max_days,active) VALUES(?::uuid,?::uuid,?,'EC',?::text[],?,?,?,?,?,?)"
    [PersistText (uuidText zoneId),PersistText (uuidText storeId),PersistText name,PersistArray (map PersistText mszSubdivisionCodes),PersistText mszDeliveryMethod,PersistInt64 mszRateMinor,optionalInt64 mszFreeShippingMinMinor,optionalInt mszEstimatedMinDays,optionalInt mszEstimatedMaxDays,PersistBool mszActive]
  jsonOne err404 "SELECT jsonb_build_object('id',id,'storeId',store_id,'name',name,'countryCode',country_code,'subdivisionCodes',subdivision_codes,'deliveryMethod',delivery_method,'rateMinor',rate_minor,'freeShippingMinMinor',free_shipping_min_minor,'estimatedMinDays',estimated_min_days,'estimatedMaxDays',estimated_max_days,'active',active) FROM merch_shipping_zone WHERE id=?::uuid" [PersistText (uuidText zoneId)]

listManagedProducts :: AuthedUser -> UUID -> AppM [Value]
listManagedProducts user storeId = do
  catalogAllowed <- hasStorePermission user storeId "catalog"
  stockAllowed <- hasStorePermission user storeId "stock"
  unless (catalogAllowed || stockAllowed) $ throwError err403
  jsonRows managedProductSql [PersistText (uuidText storeId)]

managedProductSql :: Text
managedProductSql =
  "SELECT jsonb_build_object('id',product.id,'storeId',product.store_id,'slug',product.slug,'name',product.name,'description',product.description,'category',product.category,'status',product.status,'visibility',product.visibility,'availabilityMode',product.availability_mode,'preorderReleaseAt',product.preorder_release_at,'publishAt',product.publish_at,'unpublishAt',product.unpublish_at,'buyerLimit',product.buyer_limit,'rejectionReason',product.rejection_reason,'version',product.version,'variants',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',variant.id,'sku',variant.sku,'name',variant.name,'optionValues',variant.option_values,'priceMinor',variant.price_minor,'compareAtPriceMinor',variant.compare_at_price_minor,'currency',variant.currency,'weightGrams',variant.weight_grams,'stockMode',variant.stock_mode,'stockOnHand',variant.stock_on_hand,'stockReserved',variant.stock_reserved,'stockSold',variant.stock_sold,'reorderThreshold',variant.reorder_threshold,'active',variant.active,'version',variant.version) ORDER BY variant.created_at,variant.id),'[]'::jsonb) FROM merch_product_variant variant WHERE variant.product_id=product.id),'images',(SELECT coalesce(jsonb_agg(jsonb_build_object('id',image.id,'url','/assets/serve/'||image.object_key,'variants',image.variants,'altText',image.alt_text,'sortOrder',image.sort_order,'scanStatus',image.scan_status,'moderationStatus',image.moderation_status) ORDER BY image.sort_order,image.id),'[]'::jsonb) FROM merch_product_image image WHERE image.product_id=product.id AND image.deleted_at IS NULL)) FROM merch_product product WHERE product.store_id=?::uuid ORDER BY product.updated_at DESC,product.id"

validateProductRequest :: MerchProductRequest -> AppM ()
validateProductRequest MerchProductRequest{..} = do
  _ <- either (throwError . badRequest) pure (validateMerchSlug mpuSlug)
  _ <- requiredSafeText "name" 180 mpuName
  _ <- requiredSafeText "description" 10000 mpuDescription
  unless (mpuCategory `Set.member` allowedProductCategories) $ throwError (badRequest "Unsupported product category")
  unless (mpuVisibility `elem` ["public","unlisted","hidden"]) $ throwError (badRequest "Unsupported visibility")
  unless (mpuAvailabilityMode `elem` ["in_stock","preorder","made_to_order"]) $ throwError (badRequest "Unsupported availabilityMode")
  when (mpuAvailabilityMode == "preorder" && mpuPreorderReleaseAt == Nothing) $ throwError (badRequest "preorderReleaseAt is required for preorder")
  when (null mpuVariants || length mpuVariants > 100) $ throwError (badRequest "A product requires 1 to 100 variants")
  when (length (nub (map (T.toLower . T.strip . mvrSku) mpuVariants)) /= length mpuVariants) $ throwError (badRequest "Variant SKUs must be unique")
  forM_ mpuVariants $ \MerchVariantRequest{..} -> do
    _ <- either (throwError . badRequest) pure (validateSku mvrSku)
    _ <- requiredSafeText "variant.name" 180 mvrName
    when (mvrPriceMinor <= 0 || mvrWeightGrams < 1 || mvrWeightGrams > 100000 || mvrStockOnHand < 0 || mvrReorderThreshold < 0) $
      throwError (badRequest "Variant price, weight, or stock is invalid")
    unless (mvrCurrency == "USD") $ throwError (badRequest "The pilot currently supports USD only")
    unless (mvrStockMode `elem` ["finite","made_to_order"]) $ throwError (badRequest "Unsupported stockMode")

createProduct :: AuthedUser -> UUID -> Maybe Text -> MerchProductRequest -> AppM Value
createProduct user storeId rawIdempotency request = do
  requireStorePermission user storeId "catalog"
  requireStorePermission user storeId "stock"
  idempotency <- requireIdempotencyKey rawIdempotency
  validateProductRequest request
  productId <- liftIO nextRandom
  let fingerprint = hashText (jsonText request)
  existing <- runDB (rawSql
    "SELECT id::text,create_request_sha256 FROM merch_product WHERE store_id=?::uuid AND create_idempotency_key=?"
    [PersistText (uuidText storeId),PersistText idempotency]
    :: SqlPersistT IO [(Single Text,Single Text)])
  resolvedId <- case existing of
    [(Single existingId,Single existingFingerprint)]
      | existingFingerprint == fingerprint -> pure existingId
      | otherwise -> throwError (conflict "Idempotency-Key conflicts with another product request")
    [] -> do
      insertProduct user storeId productId idempotency fingerprint request
      pure (uuidText productId)
    _ -> throwError (conflict "Product idempotency state is ambiguous")
  loadProduct resolvedId

insertProduct :: AuthedUser -> UUID -> UUID -> Text -> Text -> MerchProductRequest -> AppM ()
insertProduct user storeId productId idempotency fingerprint MerchProductRequest{..} = runDB $ do
  rawExecute
    "INSERT INTO merch_product(id,store_id,slug,name,description,category,visibility,availability_mode,preorder_release_at,publish_at,unpublish_at,buyer_limit,policy_id,created_by,create_idempotency_key,create_request_sha256) VALUES(?::uuid,?::uuid,?,?,?,?,?,?,?::timestamptz,?::timestamptz,?::timestamptz,?,?::uuid,?,?,?)"
    [PersistText (uuidText productId),PersistText (uuidText storeId),PersistText mpuSlug,PersistText mpuName,PersistText mpuDescription,PersistText mpuCategory,PersistText mpuVisibility,PersistText mpuAvailabilityMode,optionalText mpuPreorderReleaseAt,optionalText mpuPublishAt,optionalText mpuUnpublishAt,optionalInt mpuBuyerLimit,optionalUuid mpuPolicyId,PersistInt64 (currentPartyId user),PersistText idempotency,PersistText fingerprint]
  insertOrUpdateVariants storeId productId mpuVariants
  appendMerchAudit user "seller" (Just storeId) "product.created" "product" (uuidText productId)
    (object ["status" .= ("draft" :: Text), "variantCount" .= length mpuVariants])

insertOrUpdateVariants :: UUID -> UUID -> [MerchVariantRequest] -> SqlPersistT IO ()
insertOrUpdateVariants storeId productId variants = forM_ variants $ \MerchVariantRequest{..} -> do
  variantId <- maybe (liftIO nextRandom) pure mvrId
  rawExecute
    "INSERT INTO merch_product_variant(id,store_id,product_id,sku,name,option_values,price_minor,compare_at_price_minor,currency,weight_grams,customs_description,stock_mode,stock_on_hand,reorder_threshold,active) VALUES(?::uuid,?::uuid,?::uuid,?,?,?::jsonb,?,?,?,?,?,?,?, ?,?)\
    \ ON CONFLICT(id) DO UPDATE SET sku=EXCLUDED.sku,name=EXCLUDED.name,option_values=EXCLUDED.option_values,price_minor=EXCLUDED.price_minor,compare_at_price_minor=EXCLUDED.compare_at_price_minor,currency=EXCLUDED.currency,weight_grams=EXCLUDED.weight_grams,customs_description=EXCLUDED.customs_description,stock_mode=EXCLUDED.stock_mode,stock_on_hand=EXCLUDED.stock_on_hand,reorder_threshold=EXCLUDED.reorder_threshold,active=EXCLUDED.active,updated_at=now(),version=merch_product_variant.version+1\
    \ WHERE merch_product_variant.store_id=EXCLUDED.store_id AND merch_product_variant.product_id=EXCLUDED.product_id"
    [ PersistText (uuidText variantId),PersistText (uuidText storeId),PersistText (uuidText productId)
    , PersistText mvrSku,PersistText mvrName,PersistText (jsonText mvrOptionValues),PersistInt64 mvrPriceMinor
    , optionalInt64 mvrCompareAtPriceMinor,PersistText mvrCurrency,PersistInt64 (fromIntegral mvrWeightGrams)
    , optionalText mvrCustomsDescription,PersistText mvrStockMode,PersistInt64 (fromIntegral mvrStockOnHand)
    , PersistInt64 (fromIntegral mvrReorderThreshold),PersistBool mvrActive
    ]

loadProduct :: Text -> AppM Value
loadProduct productId = jsonOne err404
  (T.replace "WHERE product.store_id=?::uuid" "WHERE product.id=?::uuid" managedProductSql)
  [PersistText productId]

updateProduct :: AuthedUser -> UUID -> UUID -> MerchProductRequest -> AppM Value
updateProduct user storeId productId request@MerchProductRequest{..} = do
  requireStorePermission user storeId "catalog"
  requireStorePermission user storeId "stock"
  validateProductRequest request
  current <- runDB (rawSql "SELECT status FROM merch_product WHERE id=?::uuid AND store_id=?::uuid FOR UPDATE"
    [PersistText (uuidText productId),PersistText (uuidText storeId)] :: SqlPersistT IO [Single Text])
  status <- case current of [Single value] -> pure value; _ -> throwError err404
  when (status `elem` ["pending_review","archived"]) $ throwError (conflict "Product cannot be edited in its current state")
  runDB $ do
    rawExecute
      "UPDATE merch_product SET slug=?,name=?,description=?,category=?,visibility=?,availability_mode=?,preorder_release_at=?::timestamptz,publish_at=?::timestamptz,unpublish_at=?::timestamptz,buyer_limit=?,policy_id=?::uuid,status=CASE WHEN status='rejected' THEN 'draft' ELSE status END,rejection_reason=NULL,updated_at=now(),version=version+1 WHERE id=?::uuid AND store_id=?::uuid"
      [PersistText mpuSlug,PersistText mpuName,PersistText mpuDescription,PersistText mpuCategory,PersistText mpuVisibility,PersistText mpuAvailabilityMode,optionalText mpuPreorderReleaseAt,optionalText mpuPublishAt,optionalText mpuUnpublishAt,optionalInt mpuBuyerLimit,optionalUuid mpuPolicyId,PersistText (uuidText productId),PersistText (uuidText storeId)]
    insertOrUpdateVariants storeId productId mpuVariants
  loadProduct (uuidText productId)

updateProductStatus :: AuthedUser -> UUID -> UUID -> MerchStatusRequest -> AppM Value
updateProductStatus user storeId productId MerchStatusRequest{..} = do
  requireStorePermission user storeId "catalog"
  current <- runDB (rawSql "SELECT status FROM merch_product WHERE id=?::uuid AND store_id=?::uuid"
    [PersistText (uuidText productId),PersistText (uuidText storeId)] :: SqlPersistT IO [Single Text])
  old <- case current of [Single value] -> pure value; _ -> throwError err404
  let new = T.toLower (T.strip mstStatus)
      sellerAllowed = (old,new) `elem` [("draft","pending_review"),("rejected","draft"),("published","paused"),("sold_out","paused"),("draft","archived"),("paused","archived")]
  unless (sellerAllowed && validProductTransition old new) $ throwError (conflict "Seller product status transition is not allowed")
  when (new == "pending_review") $ do
    readiness <- runDB (rawSql
      "SELECT EXISTS(SELECT 1 FROM merch_product_variant variant WHERE variant.product_id=?::uuid AND variant.active) AND EXISTS(SELECT 1 FROM merch_product_image image WHERE image.product_id=?::uuid AND image.deleted_at IS NULL AND image.scan_status='clean') AND EXISTS(SELECT 1 FROM merch_store_policy policy JOIN merch_product product ON product.store_id=policy.store_id WHERE product.id=?::uuid AND policy.status='active')"
      (replicate 3 (PersistText (uuidText productId))) :: SqlPersistT IO [Single Bool])
    unless (readiness == [Single True]) $ throwError (conflict "Product needs an active variant, a validated image, and an active store policy before review")
  runDB $ rawExecute
    "UPDATE merch_product SET status=?,submitted_at=CASE WHEN ?='pending_review' THEN now() ELSE submitted_at END,archived_at=CASE WHEN ?='archived' THEN now() ELSE archived_at END,updated_at=now(),version=version+1 WHERE id=?::uuid AND store_id=?::uuid"
    [PersistText new,PersistText new,PersistText new,PersistText (uuidText productId),PersistText (uuidText storeId)]
  runDB $ appendMerchAudit user "seller" (Just storeId) "product.status_changed" "product" (uuidText productId)
    (object ["from" .= old, "to" .= new])
  loadProduct (uuidText productId)

uploadProductImage :: AuthedUser -> UUID -> UUID -> MerchImageUploadForm -> AppM Value
uploadProductImage user storeId productId MerchImageUploadForm{..} = do
  requireStorePermission user storeId "catalog"
  belongs <- runDB (rawSql "SELECT TRUE FROM merch_product WHERE id=?::uuid AND store_id=?::uuid AND status NOT IN ('pending_review','archived')"
    [PersistText (uuidText productId),PersistText (uuidText storeId)] :: SqlPersistT IO [Single Bool])
  unless (belongs == [Single True]) $ throwError err404
  size <- liftIO (getFileSize (fdPayload miuFile))
  when (size < 1 || size > 10*1024*1024) $ throwError (badRequest "Image must be between 1 byte and 10 MB")
  let mime = T.toLower (T.strip (fst (T.breakOn ";" (fdFileCType miuFile))))
      extension = T.toLower . T.pack . takeExtension . T.unpack $ fdFileName miuFile
      mimeMatches = (mime == "image/jpeg" && extension `elem` [".jpg",".jpeg"])
        || (mime == "image/png" && extension == ".png")
  unless mimeMatches $ throwError (badRequest "Image MIME type and extension must match JPEG or PNG")
  bytes <- liftIO (BS.readFile (fdPayload miuFile))
  decoded <- either (const (throwError (badRequest "Image bytes are invalid or unsupported"))) pure (decodeImage bytes)
  let rgb = convertRGB8 decoded
      width = imageWidth rgb
      height = imageHeight rgb
  when (width < 1 || height < 1 || width > 12000 || height > 12000 || width*height > 40000000) $
    throwError (badRequest "Image dimensions exceed the safe processing limit")
  imageId <- liftIO nextRandom
  root <- asks (assetsRootDir . envConfig)
  let directory = root </> "merch" </> T.unpack (uuidText storeId) </> T.unpack (uuidText productId)
      baseName = T.unpack (uuidText imageId)
      originalFile = baseName <> "-original.jpg"
      originalPath = directory </> originalFile
      objectKey fileName = "merch/"<>uuidText storeId<>"/"<>uuidText productId<>"/"<>T.pack fileName
      responsiveWidths = filter (< width) [480,960,1600]
  liftIO $ createDirectoryIfMissing True directory
  writeResult <- liftIO (tryAny (do
    saveJpgImage 88 originalPath (ImageRGB8 rgb)
    forM_ responsiveWidths $ \targetWidth -> do
      let targetHeight = max 1 (height * targetWidth `div` width)
      saveJpgImage 84 (directory </> baseName<>"-"<>show targetWidth<>".jpg")
        (ImageRGB8 (scaleImageNearest targetWidth targetHeight rgb))
    ))
  either (const (throwError err500 { errBody = "Image processing failed" })) pure writeResult
  let variants = object
        [ AesonKey.fromText (T.pack (show targetWidth)) .= object
            [ "objectKey" .= objectKey (baseName<>"-"<>show targetWidth<>".jpg")
            , "width" .= targetWidth
            , "height" .= max 1 (height * targetWidth `div` width)
            ]
        | targetWidth <- responsiveWidths
        ]
      checksum = T.pack (show (hash bytes :: Digest SHA256))
  pool <- asks envPool
  inserted <- liftIO $ tryAny $ runSqlPool (rawExecute
    "INSERT INTO merch_product_image(id,product_id,object_key,original_filename,mime_type,byte_size,width_px,height_px,checksum_sha256,variants,alt_text,sort_order,scan_status,moderation_status,created_by) VALUES(?::uuid,?::uuid,?,?,?,?,?,?,?,?::jsonb,?,?,'clean','pending',?)"
    [PersistText (uuidText imageId),PersistText (uuidText productId),PersistText (objectKey originalFile),PersistText (fdFileName miuFile),PersistText "image/jpeg",PersistInt64 (fromIntegral size),PersistInt64 (fromIntegral width),PersistInt64 (fromIntegral height),PersistText checksum,PersistText (jsonText variants),PersistText miuAltText,PersistInt64 (fromIntegral miuSortOrder),PersistInt64 (currentPartyId user)]) pool
  case inserted of
    Left _ -> do
      liftIO $ forM_ (originalPath:[directory </> baseName<>"-"<>show w<>".jpg" | w <- responsiveWidths]) $ \path -> do
        _ <- try (removeFile path) :: IO (Either SomeException ())
        pure ()
      throwError (conflict "Image order is already used or image metadata could not be stored")
    Right () -> jsonOne err404
      "SELECT jsonb_build_object('id',id,'url','/assets/serve/'||object_key,'variants',variants,'altText',alt_text,'sortOrder',sort_order,'scanStatus',scan_status,'moderationStatus',moderation_status,'width',width_px,'height',height_px) FROM merch_product_image WHERE id=?::uuid"
      [PersistText (uuidText imageId)]

updateVariantStock :: AuthedUser -> UUID -> UUID -> MerchStockRequest -> AppM Value
updateVariantStock user storeId variantId MerchStockRequest{..} = do
  requireStorePermission user storeId "stock"
  when (msqStockOnHand < 0 || msqReorderThreshold < 0 || msqVersion < 1) $
    throwError (badRequest "Stock and reorderThreshold must be non-negative; version must be positive")
  updated <- runDB $ do
    rows <- (rawSql
      "UPDATE merch_product_variant variant SET stock_on_hand=?,reorder_threshold=?,active=?,updated_at=now(),version=version+1\
      \ WHERE variant.store_id=?::uuid AND variant.id=?::uuid AND variant.version=?\
      \ AND (variant.stock_mode='made_to_order' OR ?>=variant.stock_reserved+variant.stock_sold)\
      \ AND EXISTS(SELECT 1 FROM merch_product product WHERE product.id=variant.product_id AND product.status<>'archived')\
      \ RETURNING variant.product_id::text,jsonb_build_object('id',variant.id,'sku',variant.sku,'name',variant.name,'stockMode',variant.stock_mode,'stockOnHand',variant.stock_on_hand,'stockReserved',variant.stock_reserved,'stockSold',variant.stock_sold,'reorderThreshold',variant.reorder_threshold,'active',variant.active,'version',variant.version)"
      [ PersistInt64 (fromIntegral msqStockOnHand), PersistInt64 (fromIntegral msqReorderThreshold)
      , PersistBool msqActive, PersistText (uuidText storeId), PersistText (uuidText variantId)
      , PersistInt64 msqVersion, PersistInt64 (fromIntegral msqStockOnHand)
      ] :: SqlPersistT IO [(Single Text,Single CMS.AesonValue)])
    case rows of
      [(Single productId,Single value)] -> do
        rawExecute
          "UPDATE merch_product product SET status='sold_out',updated_at=now(),version=version+1 WHERE product.id=?::uuid AND product.status='published' AND NOT EXISTS(SELECT 1 FROM merch_product_variant candidate WHERE candidate.product_id=product.id AND candidate.active AND (candidate.stock_mode='made_to_order' OR candidate.stock_on_hand-candidate.stock_reserved-candidate.stock_sold>0))"
          [PersistText productId]
        rawExecute
          "UPDATE merch_product product SET status='published',updated_at=now(),version=version+1 WHERE product.id=?::uuid AND product.status='sold_out' AND EXISTS(SELECT 1 FROM merch_product_variant candidate WHERE candidate.product_id=product.id AND candidate.active AND (candidate.stock_mode='made_to_order' OR candidate.stock_on_hand-candidate.stock_reserved-candidate.stock_sold>0))"
          [PersistText productId]
        appendMerchAudit user "seller" (Just storeId) "variant.stock_updated" "variant" (uuidText variantId)
          (object ["stockOnHand" .= msqStockOnHand, "reorderThreshold" .= msqReorderThreshold, "active" .= msqActive])
        pure (Just (CMS.unAesonValue value))
      _ -> pure Nothing
  maybe (throwError (conflict "Variant changed, is archived, or stock is below sold and reserved units")) pure updated

listSellerOrders :: AuthedUser -> UUID -> Maybe Text -> AppM [Value]
listSellerOrders user storeId rawStatus = do
  requireStorePermission user storeId "orders"
  financeAllowed <- hasStorePermission user storeId "finance"
  jsonRows
    "SELECT jsonb_strip_nulls(jsonb_build_object('id',order_record.id,'orderNumber',order_record.order_number,'customerName',order_record.customer_name,'customerEmail',order_record.customer_email,'customerPhone',order_record.customer_phone,'recipient',order_record.recipient_snapshot,'shippingMethod',order_record.shipping_method,'currency',order_record.currency,'productSubtotalMinor',order_record.product_subtotal_minor,'taxMinor',order_record.tax_minor,'shippingMinor',order_record.shipping_minor,'totalMinor',order_record.total_minor,'tdfCommissionMinor',CASE WHEN ?::boolean THEN order_record.tdf_commission_minor ELSE NULL END,'sellerNetMinor',CASE WHEN ?::boolean THEN order_record.seller_net_minor ELSE NULL END,'commercialStatus',order_record.commercial_status,'paymentStatus',order_record.payment_status,'fulfillmentStatus',order_record.fulfillment_status,'refundStatus',order_record.refund_status,'disputeStatus',order_record.dispute_status,'settlementStatus',order_record.settlement_status,'createdAt',order_record.created_at,'lines',(SELECT jsonb_agg(jsonb_build_object('quantity',line.quantity,'product',line.product_snapshot,'variant',line.variant_snapshot) ORDER BY line.line_number) FROM merch_order_line line WHERE line.order_id=order_record.id))) FROM merch_order order_record WHERE order_record.store_id=?::uuid AND (?::text IS NULL OR order_record.fulfillment_status=?::text) ORDER BY order_record.created_at DESC,order_record.id"
    [PersistBool financeAllowed,PersistBool financeAllowed,PersistText (uuidText storeId),optionalText rawStatus,optionalText rawStatus]

updateFulfillment :: AuthedUser -> UUID -> UUID -> MerchFulfillmentRequest -> AppM Value
updateFulfillment user storeId orderId MerchFulfillmentRequest{..} = do
  requireStorePermission user storeId "fulfillment"
  current <- runDB (rawSql
    "SELECT fulfillment_status,payment_status,shipping_method FROM merch_order WHERE id=?::uuid AND store_id=?::uuid FOR UPDATE"
    [PersistText (uuidText orderId),PersistText (uuidText storeId)]
    :: SqlPersistT IO [(Single Text,Single Text,Single Text)])
  (old,payment,shippingMethod) <- case current of [row] -> pure row; _ -> throwError err404
  let new = T.toLower (T.strip mfrStatus)
  unless (new `elem` ["preparing","ready_for_pickup","shipped","delivered","problem","return_requested","returned"]) $
    throwError (badRequest "Unsupported seller fulfillment status; cancellations and refunds require an issue and independent review")
  unless (validFulfillmentTransition (unSingle old) new) $ throwError (conflict "Fulfillment status transition is not allowed")
  when (new `elem` ["preparing","ready_for_pickup","shipped","delivered"] && unSingle payment /= "paid") $
    throwError (conflict "Paid status must be server-verified before fulfillment advances")
  when (new == "ready_for_pickup" && unSingle shippingMethod /= "coordinated_pickup") $
    throwError (conflict "Only pickup orders can be marked ready for pickup")
  when (new == "shipped" && (unSingle shippingMethod /= "national_shipping" || mfrCarrier == Nothing || mfrTrackingNumber == Nothing)) $
    throwError (badRequest "Carrier and tracking number are required before marking a shipped order")
  forM_ mfrCarrier $ \value -> do
    _ <- requiredSafeText "carrier" 120 value
    pure ()
  forM_ mfrTrackingNumber $ \value -> do
    _ <- requiredSafeText "trackingNumber" 200 value
    pure ()
  forM_ mfrTrackingUrl $ \value ->
    unless ("https://" `T.isPrefixOf` T.toLower value && T.length value <= 500 && not (T.any isControl value)) $
      throwError (badRequest "trackingUrl must be a safe HTTPS URL")
  runDB $ do
    rawExecute "UPDATE merch_order SET fulfillment_status=?,updated_at=now(),completed_at=CASE WHEN ?='delivered' THEN now() ELSE completed_at END WHERE id=?::uuid AND store_id=?::uuid"
      [PersistText new,PersistText new,PersistText (uuidText orderId),PersistText (uuidText storeId)]
    rawExecute "INSERT INTO merch_fulfillment_event(order_id,event_type,from_status,to_status,actor_party_id,public_note,private_note) VALUES(?::uuid,?,?,?, ?,?,?)"
      [PersistText (uuidText orderId),PersistText (fulfillmentEvent new),PersistText (unSingle old),PersistText new,PersistInt64 (currentPartyId user),optionalText mfrPublicNote,optionalText mfrPrivateNote]
    when (new == "shipped") $ rawExecute
      "INSERT INTO merch_shipment(order_id,carrier,tracking_number,tracking_url,status,shipped_at,created_by) VALUES(?::uuid,?,?,?,'shipped',now(),?)"
      [PersistText (uuidText orderId),optionalText mfrCarrier,optionalText mfrTrackingNumber,optionalText mfrTrackingUrl,PersistInt64 (currentPartyId user)]
    appendMerchAudit user "seller" (Just storeId) "order.fulfillment_changed" "order" (uuidText orderId)
      (object ["from" .= unSingle old, "to" .= new])
  jsonOne err404 "SELECT jsonb_build_object('id',id,'orderNumber',order_number,'paymentStatus',payment_status,'fulfillmentStatus',fulfillment_status,'updatedAt',updated_at) FROM merch_order WHERE id=?::uuid" [PersistText (uuidText orderId)]
  where
    unSingle (Single value) = value
    fulfillmentEvent status = case status of
      "preparing" -> "preparation_started"
      "ready_for_pickup" -> "pickup_ready"
      "shipped" -> "shipped"
      "delivered" -> "delivered"
      "problem" -> "problem_reported"
      "cancelled" -> "cancelled"
      "return_requested" -> "return_requested"
      "returned" -> "returned"
      _ -> "problem_reported"

listAdminStores :: AuthedUser -> Maybe Text -> AppM [Value]
listAdminStores user rawStatus = do
  requireAdmin user
  jsonRows
    "SELECT jsonb_build_object('id',store.id,'profileId',store.directory_profile_id,'profileName',profile.public_name,'slug',store.slug,'displayName',store.display_name,'applicationStatus',store.application_status,'operationalStatus',store.operational_status,'applicationNote',store.application_note,'requestedAt',store.requested_at,'reviewedAt',store.reviewed_at,'reviewerNotes',store.reviewer_notes) FROM merch_store store JOIN directory_profile profile ON profile.id=store.directory_profile_id WHERE (?::text IS NULL OR store.application_status=?::text) ORDER BY store.requested_at,store.id"
    [optionalText rawStatus,optionalText rawStatus]

reviewStore :: AuthedUser -> UUID -> MerchStoreReviewRequest -> AppM Value
reviewStore user storeId MerchStoreReviewRequest{..} = do
  requireAdmin user
  let decision = T.toLower (T.strip msrDecision)
  unless (decision `elem` ["approve","reject","suspend","reactivate"]) $ throwError (badRequest "Unsupported store decision")
  current <- runDB (rawSql
    "SELECT application_status,operational_status FROM merch_store WHERE id=?::uuid FOR UPDATE"
    [PersistText (uuidText storeId)] :: SqlPersistT IO [(Single Text,Single Text)])
  (applicationStatus,operationalStatus) <- case current of
    [row] -> pure row
    _ -> throwError err404
  let transitionAllowed = case decision of
        "approve" -> applicationStatus == Single "requested"
        "reject" -> applicationStatus == Single "requested"
        "suspend" -> applicationStatus == Single "approved" && operationalStatus == Single "active"
        "reactivate" -> applicationStatus == Single "approved" && operationalStatus == Single "suspended"
        _ -> False
  unless transitionAllowed $ throwError (conflict "Store review transition is not allowed")
  notes <- requiredSafeText "reviewerNotes" 2000 msrReviewerNotes
  when (decision == "approve" && maybe False (\bps -> bps < 0 || bps > 10000) msrCommissionBps) $
    throwError (badRequest "commissionBps must be between 0 and 10000")
  runDB $ do
    rawExecute
      "UPDATE merch_store SET application_status=CASE WHEN ?='approve' THEN 'approved' WHEN ?='reject' THEN 'rejected' ELSE application_status END,operational_status=CASE WHEN ? IN ('approve','reactivate') THEN 'active' WHEN ?='suspend' THEN 'suspended' WHEN ?='reject' THEN 'inactive' ELSE operational_status END,reviewer_notes=?,reviewed_by=?,reviewed_at=CASE WHEN ? IN ('approve','reject') THEN now() ELSE reviewed_at END,activated_at=CASE WHEN ? IN ('approve','reactivate') THEN coalesce(activated_at,now()) ELSE activated_at END,suspended_at=CASE WHEN ?='suspend' THEN now() ELSE NULL END,suspension_reason=CASE WHEN ?='suspend' THEN ? ELSE NULL END,updated_at=now(),version=version+1 WHERE id=?::uuid"
      [PersistText decision,PersistText decision,PersistText decision,PersistText decision,PersistText decision,PersistText notes,PersistInt64 (currentPartyId user),PersistText decision,PersistText decision,PersistText decision,PersistText decision,PersistText notes,PersistText (uuidText storeId)]
    forM_ msrCommissionBps $ \bps -> do
      rawExecute "UPDATE merch_commission_policy SET effective_until=now() WHERE store_id=?::uuid AND effective_until IS NULL" [PersistText (uuidText storeId)]
      rawExecute "INSERT INTO merch_commission_policy(store_id,commission_bps,reason,approved_by) VALUES(?::uuid,?,?,?)"
        [PersistText (uuidText storeId),PersistInt64 (fromIntegral bps),PersistText (fromMaybe notes msrCommissionReason),PersistInt64 (currentPartyId user)]
    appendMerchAudit user "staff" (Just storeId) "store.reviewed" "store" (uuidText storeId)
      (object ["decision" .= decision, "commissionBps" .= msrCommissionBps])
  loadManagedStore (uuidText storeId)

listAdminProducts :: AuthedUser -> Maybe Text -> AppM [Value]
listAdminProducts user rawStatus = do
  requireAdmin user
  jsonRows
    "SELECT jsonb_build_object('id',product.id,'storeId',product.store_id,'storeName',store.display_name,'slug',product.slug,'name',product.name,'category',product.category,'status',product.status,'submittedAt',product.submitted_at,'imagesReady',EXISTS(SELECT 1 FROM merch_product_image image WHERE image.product_id=product.id AND image.scan_status='clean' AND image.deleted_at IS NULL),'variantCount',(SELECT count(*) FROM merch_product_variant variant WHERE variant.product_id=product.id AND variant.active)) FROM merch_product product JOIN merch_store store ON store.id=product.store_id WHERE (?::text IS NULL OR product.status=?::text) ORDER BY product.submitted_at NULLS LAST,product.id"
    [optionalText rawStatus,optionalText rawStatus]

reviewProduct :: AuthedUser -> UUID -> MerchStatusRequest -> AppM Value
reviewProduct user productId MerchStatusRequest{..} = do
  requireAdmin user
  current <- runDB (rawSql "SELECT status FROM merch_product WHERE id=?::uuid"
    [PersistText (uuidText productId)] :: SqlPersistT IO [Single Text])
  old <- case current of [Single value] -> pure value; _ -> throwError err404
  let decision = T.toLower (T.strip mstStatus)
  unless (old == "pending_review" && decision `elem` ["published","rejected"] && validProductTransition old decision) $
    throwError (conflict "Product review transition is not allowed")
  when (decision == "rejected" && maybe True ((<5) . T.length . T.strip) mstReason) $
    throwError (badRequest "A rejection reason of at least 5 characters is required")
  runDB $ do
    rawExecute
      "UPDATE merch_product SET status=?,reviewed_by=?,reviewed_at=now(),published_at=CASE WHEN ?='published' THEN coalesce(published_at,now()) ELSE published_at END,rejection_reason=CASE WHEN ?='rejected' THEN ? ELSE NULL END,updated_at=now(),version=version+1 WHERE id=?::uuid"
      [PersistText decision,PersistInt64 (currentPartyId user),PersistText decision,PersistText decision,optionalText mstReason,PersistText (uuidText productId)]
    when (decision == "published") $ rawExecute
      "UPDATE merch_product_image SET moderation_status='allowed' WHERE product_id=?::uuid AND scan_status='clean' AND moderation_status='pending'"
      [PersistText (uuidText productId)]
    appendMerchAudit user "staff" Nothing "product.reviewed" "product" (uuidText productId)
      (object ["from" .= old, "to" .= decision, "reason" .= mstReason])
  loadProduct (uuidText productId)

createSettlement :: AuthedUser -> MerchSettlementRequest -> AppM Value
createSettlement user MerchSettlementRequest{..} = do
  requireAdmin user
  when (null mseOrderIds || length mseOrderIds > 1000) $ throwError (badRequest "Settlement requires 1 to 1000 orders")
  settlementId <- liftIO nextRandom
  created <- runDB (rawSql
    "WITH selected AS (SELECT order_record.* FROM merch_order order_record WHERE order_record.id=ANY(?::uuid[]) AND order_record.store_id=?::uuid AND order_record.payment_status IN ('paid','partially_refunded') AND order_record.fulfillment_status IN ('delivered','returned') AND order_record.settlement_status IN ('not_ready','ready') FOR UPDATE),\
    \ totals AS (SELECT count(*) count,sum(product_subtotal_minor) gross,sum(discount_minor) discounts,sum(tax_minor) taxes,sum(shipping_minor) shipping,sum(processor_fee_minor) processor_fees,sum(tdf_commission_minor) commission,sum(refunded_minor) refunds,sum(adjusted_minor) adjustments,sum(seller_net_minor-refunded_minor+adjusted_minor) seller_net FROM selected),\
    \ inserted AS (INSERT INTO merch_settlement(id,store_id,period_start,period_end,currency,gross_product_minor,discounts_minor,taxes_minor,shipping_minor,processor_fees_minor,tdf_commission_minor,refunds_minor,adjustments_minor,seller_net_minor,status,review_notes,prepared_by) SELECT ?::uuid,?::uuid,?::timestamptz,?::timestamptz,'USD',gross,discounts,taxes,shipping,processor_fees,commission,refunds,adjustments,seller_net,'under_review',?,? FROM totals WHERE count=? RETURNING id),\
    \ linked AS (INSERT INTO merch_settlement_order(settlement_id,order_id,seller_net_minor,refund_minor,adjustment_minor) SELECT inserted.id,selected.id,selected.seller_net_minor,selected.refunded_minor,selected.adjusted_minor FROM inserted CROSS JOIN selected RETURNING order_id)\
    \ SELECT jsonb_build_object('id',inserted.id,'orderCount',(SELECT count(*) FROM linked)) FROM inserted"
    [ PersistArray (map toPersistValue mseOrderIds),PersistText (uuidText mseStoreId),PersistText (uuidText settlementId)
    , PersistText (uuidText mseStoreId),PersistText msePeriodStart,PersistText msePeriodEnd,optionalText mseReviewNotes
    , PersistInt64 (currentPartyId user),PersistInt64 (fromIntegral (length mseOrderIds))
    ] :: SqlPersistT IO [Single CMS.AesonValue])
  case created of
    [Single _] -> do
      runDB $ appendMerchAudit user "staff" (Just mseStoreId) "settlement.created" "settlement" (uuidText settlementId)
        (object ["orderCount" .= length mseOrderIds, "periodStart" .= msePeriodStart, "periodEnd" .= msePeriodEnd])
      loadSettlement settlementId
    _ -> throwError (conflict "Settlement orders are ineligible, already linked, or belong to another seller")

updateSettlementStatus :: AuthedUser -> UUID -> MerchStatusRequest -> AppM Value
updateSettlementStatus user settlementId MerchStatusRequest{..} = do
  requireAdmin user
  let status = T.toLower (T.strip mstStatus)
  unless (status `elem` ["approved","held"]) $ throwError (badRequest "Settlement can only be approved or held here; paid requires a separate evidence workflow")
  updated <- runDB (rawSql
    "UPDATE merch_settlement SET status=?,approved_by=CASE WHEN ?='approved' THEN ? ELSE approved_by END,approved_at=CASE WHEN ?='approved' THEN now() ELSE approved_at END,review_notes=coalesce(?,review_notes),updated_at=now() WHERE id=?::uuid AND status='under_review' AND prepared_by<>? RETURNING store_id::text"
    [PersistText status,PersistText status,PersistInt64 (currentPartyId user),PersistText status,optionalText mstReason,PersistText (uuidText settlementId),PersistInt64 (currentPartyId user)]
    :: SqlPersistT IO [Single Text])
  storeId <- case updated of
    [Single value] -> maybe (throwError err500) pure (UUID.fromText value)
    _ -> throwError (conflict "Settlement must be under review and approved by someone other than its preparer")
  runDB $ appendMerchAudit user "staff" (Just storeId) "settlement.status_changed" "settlement" (uuidText settlementId)
    (object ["status" .= status, "reason" .= mstReason])
  loadSettlement settlementId

loadSettlement :: UUID -> AppM Value
loadSettlement settlementId = jsonOne err404
  "SELECT jsonb_build_object('id',settlement.id,'storeId',settlement.store_id,'periodStart',settlement.period_start,'periodEnd',settlement.period_end,'currency',settlement.currency,'grossProductMinor',settlement.gross_product_minor,'discountsMinor',settlement.discounts_minor,'taxesMinor',settlement.taxes_minor,'shippingMinor',settlement.shipping_minor,'processorFeesMinor',settlement.processor_fees_minor,'tdfCommissionMinor',settlement.tdf_commission_minor,'refundsMinor',settlement.refunds_minor,'adjustmentsMinor',settlement.adjustments_minor,'sellerNetMinor',settlement.seller_net_minor,'status',settlement.status,'preparedBy',settlement.prepared_by,'approvedBy',settlement.approved_by,'paidBy',settlement.paid_by,'approvedAt',settlement.approved_at,'paidAt',settlement.paid_at,'evidenceObjectKey',settlement.evidence_object_key,'orderCount',(SELECT count(*) FROM merch_settlement_order linked WHERE linked.settlement_id=settlement.id)) FROM merch_settlement settlement WHERE settlement.id=?::uuid"
  [PersistText (uuidText settlementId)]
