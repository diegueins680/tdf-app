{-# LANGUAGE OverloadedStrings #-}
module TDF.Studio.Gallery (validateStudioGalleryPayload) where

import Control.Monad (unless)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKeyMap
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isControl, isDigit, isSpace)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (fromGregorianValid)
import qualified Data.UUID as UUID
import Text.Read (readMaybe)

-- | Mirrors @parseStudioGallery@ in the web client.
validateStudioGalleryPayload :: Bool -> Value -> Either Text ()
validateStudioGalleryPayload allowIncomplete value =
  case value of
    Object root
      | AKeyMap.lookup "schemaVersion" root == Just (Number 1)
      , Just (Array photoValues) <- AKeyMap.lookup "photos" root ->
          let photos = toList photoValues
          in if length photos > 200
               then Left "at most 200 photos are allowed"
               else checkPhotos Set.empty (zip [1 :: Int ..] photos)
    _ -> Left "schemaVersion 1 and a photos list are required"
  where
    checkPhotos :: Set.Set Text -> [(Int, Value)] -> Either Text ()
    checkPhotos _ [] = Right ()
    checkPhotos seen ((index, photoValue) : rest) =
      case checkPhoto seen photoValue of
        Left problem -> Left ("photo " <> T.pack (show index) <> ": " <> problem)
        Right photoId -> checkPhotos (Set.insert photoId seen) rest

    checkPhoto :: Set.Set Text -> Value -> Either Text Text
    checkPhoto seen (Object photo) = do
      photoId <- requiredText photo "id"
      title <- requiredText photo "title"
      alt <- requiredText photo "alt"
      imageUrl <- requiredText photo "imageUrl"
      unless (validPhotoId photoId && not (Set.member photoId seen)) $
        Left "id is invalid or repeated"
      unless (T.length title <= 200 && T.length alt <= 1000) $
        Left "title or alt is too long"
      case AKeyMap.lookup "visible" photo of
        Just (Bool _) -> Right ()
        _ -> Left "visible must be true or false"
      thumbnailUrl <- optionalText photo "thumbnailUrl"
      _ <- optionalText photo "credit"
      capturedAt <- optionalText photo "capturedAt"
      sourceUrl <- optionalText photo "sourceUrl"
      unless allowIncomplete $ do
        unless (studioGalleryImageUrlAllowed imageUrl && blankOr studioGalleryImageUrlAllowed thumbnailUrl) $
          Left "imageUrl and thumbnailUrl must be public HTTPS image URLs"
        unless (blankOr (\url -> "https://" `T.isPrefixOf` url && studioGalleryImageUrlAllowed url) sourceUrl) $
          Left "sourceUrl must use HTTPS"
        unless (blankOr validCapturedAt capturedAt) $
          Left "capturedAt must be an existing YYYY-MM or YYYY-MM-DD date"
      Right photoId
    checkPhoto _ _ = Left "must be an object"

    requiredText :: AKeyMap.KeyMap Value -> Text -> Either Text Text
    requiredText photo field =
      case AKeyMap.lookup (AKey.fromText field) photo of
        Just (String text)
          | allowIncomplete || not (T.null (T.strip text)) -> Right text
        _ -> Left (field <> " is required")

    optionalText :: AKeyMap.KeyMap Value -> Text -> Either Text (Maybe Text)
    optionalText photo field =
      case AKeyMap.lookup (AKey.fromText field) photo of
        Nothing -> Right Nothing
        Just (String text)
          | T.length text <= 2000 -> Right (Just text)
        _ -> Left (field <> " must be text of at most 2000 characters")

    blankOr :: (Text -> Bool) -> Maybe Text -> Bool
    blankOr accepts = maybe True (\text -> T.null text || accepts text)

    validPhotoId :: Text -> Bool
    validPhotoId photoId =
      not (T.null photoId)
        && T.length photoId <= 100
        && T.all (\c -> isAscii c && (isAlphaNum c || c == '-')) photoId

    validCapturedAt :: Text -> Bool
    validCapturedAt raw =
      case T.splitOn "-" raw of
        [year, month] -> existingDate year month "01"
        [year, month, day] -> existingDate year month day
        _ -> False

    existingDate :: Text -> Text -> Text -> Bool
    existingDate year month day =
      T.length year == 4 && T.length month == 2 && T.length day == 2
        && T.all isDigit (year <> month <> day)
        && case (readMaybe (T.unpack year), readMaybe (T.unpack month), readMaybe (T.unpack day)) of
             (Just y, Just m, Just d) -> isJust (fromGregorianValid y m d)
             _ -> False

-- | A bundled studio asset or a credential-free HTTPS URL.
studioGalleryImageUrlAllowed :: Text -> Bool
studioGalleryImageUrlAllowed url = bundledAsset || submissionImage || secureRemote
  where
    submissionImage = case T.splitOn "/" url of
      ["","studio-submissions","images",identifier,size] ->
        isJust (UUID.fromText identifier) && size `elem` ["small","large"]
      _ -> False
    bundledAsset =
      case T.stripPrefix "/assets/tdf-studio/" url of
        Just name ->
          any (\suffix -> maybe False validStem (T.stripSuffix suffix name)) ["-640.webp", "-1280.webp"]
        Nothing -> False
    validStem stem =
      not (T.null stem) && T.all (\c -> isAsciiLower c || isDigit c || c == '-') stem
    secureRemote =
      T.toLower (T.take 8 url) == "https://"
        && not (T.null authority)
        && not (T.any (== '@') authority)
        && not (T.any (\c -> isSpace c || isControl c) url)
    authority = T.takeWhile (`notElem` ['/', '?', '#']) (T.drop 8 url)
