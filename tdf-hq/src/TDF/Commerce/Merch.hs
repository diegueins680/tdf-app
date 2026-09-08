{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.Merch
  ( MerchMoney(..)
  , calculateMerchMoney
  , validProductTransition
  , validFulfillmentTransition
  , validateMerchSlug
  , validateSku
  , validateQuantity
  , validateCheckoutText
  , allowedProductCategories
  ) where

import           Data.Char (isAsciiLower, isDigit, isControl)
import           Data.Int (Int64)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T

data MerchMoney = MerchMoney
  { merchProductSubtotalMinor :: Int64
  , merchDiscountMinor        :: Int64
  , merchTaxMinor             :: Int64
  , merchShippingMinor        :: Int64
  , merchProcessorFeeMinor    :: Int64
  , merchCommissionBps        :: Int
  , merchCommissionMinor      :: Int64
  , merchSellerNetMinor       :: Int64
  , merchTotalMinor           :: Int64
  } deriving (Eq, Show)

calculateMerchMoney
  :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int -> Either Text MerchMoney
calculateMerchMoney subtotal discount tax shipping processorFee commissionBps
  | subtotal <= 0 = Left "Product subtotal must be positive"
  | discount < 0 || discount > subtotal = Left "Discount must be between zero and product subtotal"
  | tax < 0 = Left "Tax cannot be negative"
  | shipping < 0 = Left "Shipping cannot be negative"
  | processorFee < 0 = Left "Processor fee cannot be negative"
  | commissionBps < 0 || commissionBps > 10000 = Left "Commission must be between 0 and 10000 basis points"
  | sellerNet < 0 = Left "Seller net cannot be negative"
  | otherwise = Right MerchMoney
      { merchProductSubtotalMinor = subtotal
      , merchDiscountMinor = discount
      , merchTaxMinor = tax
      , merchShippingMinor = shipping
      , merchProcessorFeeMinor = processorFee
      , merchCommissionBps = commissionBps
      , merchCommissionMinor = commission
      , merchSellerNetMinor = sellerNet
      , merchTotalMinor = total
      }
  where
    commissionBase = subtotal - discount
    commission = commissionBase * fromIntegral commissionBps `div` 10000
    total = commissionBase + tax + shipping
    sellerNet = commissionBase + tax + shipping - commission - processorFee

validProductTransition :: Text -> Text -> Bool
validProductTransition fromStatus toStatus
  | fromStatus == toStatus = True
  | otherwise = toStatus `Set.member` fromMaybeEmpty fromStatus transitions
  where
    transitions :: [(Text, Set Text)]
    transitions =
      [ ("draft", Set.fromList ["pending_review","archived"])
      , ("pending_review", Set.fromList ["draft","published","rejected","archived"])
      , ("published", Set.fromList ["sold_out","paused","archived"])
      , ("sold_out", Set.fromList ["published","paused","archived"])
      , ("paused", Set.fromList ["published","archived"])
      , ("rejected", Set.fromList ["draft","archived"])
      , ("archived", Set.empty)
      ]
    fromMaybeEmpty key = maybe Set.empty id . lookup key

validFulfillmentTransition :: Text -> Text -> Bool
validFulfillmentTransition fromStatus toStatus
  | fromStatus == toStatus = True
  | otherwise = toStatus `Set.member` fromMaybeEmpty fromStatus transitions
  where
    transitions :: [(Text, Set Text)]
    transitions =
      [ ("pending", Set.fromList ["preparing","cancelled","problem"])
      , ("preparing", Set.fromList ["ready_for_pickup","shipped","cancelled","problem"])
      , ("ready_for_pickup", Set.fromList ["delivered","cancelled","problem"])
      , ("shipped", Set.fromList ["delivered","problem","return_requested"])
      , ("delivered", Set.fromList ["return_requested","problem"])
      , ("return_requested", Set.fromList ["returned","problem"])
      , ("problem", Set.fromList ["preparing","ready_for_pickup","shipped","delivered","cancelled","return_requested","returned"])
      , ("returned", Set.empty)
      , ("cancelled", Set.empty)
      ]
    fromMaybeEmpty key = maybe Set.empty id . lookup key

validateMerchSlug :: Text -> Either Text Text
validateMerchSlug raw
  | T.length slug < 2 || T.length slug > 120 = Left "Slug must contain 2 to 120 characters"
  | not (isAtom (T.head slug)) = Left "Slug must start with a lowercase letter or number"
  | T.any (not . isSlugCharacter) slug = Left "Slug may contain only lowercase letters, numbers, and single hyphens"
  | "--" `T.isInfixOf` slug || T.last slug == '-' = Left "Slug may not contain repeated or trailing hyphens"
  | otherwise = Right slug
  where
    slug = T.strip raw
    isAtom ch = isAsciiLower ch || isDigit ch
    isSlugCharacter ch = isAtom ch || ch == '-'

validateSku :: Text -> Either Text Text
validateSku raw
  | T.null sku || T.length sku > 120 = Left "SKU must contain 1 to 120 characters"
  | T.any isControl sku = Left "SKU must not contain control characters"
  | T.any (not . isSkuCharacter) sku = Left "SKU may contain letters, numbers, spaces, dots, underscores, hyphens, and slashes"
  | otherwise = Right sku
  where
    sku = T.strip raw
    isSkuCharacter ch =
      (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || isDigit ch
        || ch `elem` (" ._-/" :: String)

validateQuantity :: Int -> Either Text Int
validateQuantity quantity
  | quantity < 1 || quantity > 100 = Left "Quantity must be between 1 and 100"
  | otherwise = Right quantity

validateCheckoutText :: Text -> Int -> Text -> Either Text Text
validateCheckoutText fieldName maxLength raw
  | T.null value = Left (fieldName <> " is required")
  | T.length value > maxLength = Left (fieldName <> " is too long")
  | T.any isControl value = Left (fieldName <> " must not contain control characters")
  | otherwise = Right value
  where value = T.strip raw

allowedProductCategories :: Set Text
allowedProductCategories = Set.fromList
  [ "apparel", "vinyl", "cd", "cassette", "poster", "accessory"
  , "limited_edition", "bundle", "other"
  ]
