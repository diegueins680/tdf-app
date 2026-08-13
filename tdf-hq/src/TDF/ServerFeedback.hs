{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.ServerFeedback
  ( feedbackServer
  , normalizeOptionalFeedbackText
  , validateFeedbackDescription
  , validateFeedbackTitle
  , validateFeedbackConsent
  , validateOptionalFeedbackContactEmail
  , validateFeedbackAttachmentSize
  , validateFeedbackAttachmentContentType
  , validateFeedbackAttachmentFileName
  , validateFeedbackAttachmentMetadata
  , sanitizeFeedbackAttachmentFileName
  ) where

import           Control.Exception         (SomeException, displayException, try)
import           Control.Monad              (forM_)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Char                  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
                                            , generalCategory
                                            , isAlphaNum
                                            , isAscii
                                            , isAsciiLower
                                            , isControl
                                            , isDigit
                                            )
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (getCurrentTime)
import           Database.Persist           (get, insert)
import           Database.Persist.Sql       (runSqlPool)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp)
import           System.Directory           (createDirectoryIfMissing, getFileSize)
import           System.FilePath            ((</>), takeExtension, takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy       as BL
import           Data.UUID.V4               (nextRandom)
import           Data.UUID                  (toText)
import           Web.PathPieces             (fromPathPiece)

import           TDF.API.Feedback
import           TDF.Auth                   ( AuthedUser(..)
                                            , extractTokenFromHeaders
                                            , loadAuthedUser
                                            )
import           TDF.DB                     (Env(..))
import           TDF.ModelsExtra            (Feedback(..))
import qualified TDF.Catalog.Models          as Catalog
import qualified TDF.Email.Service          as EmailSvc

feedbackServer
  :: forall m.
     ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => ServerT FeedbackAPI m
feedbackServer authorizationHeader cookieHeader = submitFeedback
  where
    submitFeedback :: FeedbackPayload -> m NoContent
    submitFeedback FeedbackPayload{..} = do
      title <- either throwError pure (validateFeedbackTitle fpTitle)
      body <- either throwError pure (validateFeedbackDescription fpDescription)
      (categoryId, categoryLabel) <- resolvePublishedFeedbackCategory fpCategoryId
      (severityId, severityLabel) <- resolvePublishedFeedbackSeverity fpSeverityId
      either throwError pure (validateFeedbackConsent fpConsent)
      contactEmail <- either throwError pure (validateOptionalFeedbackContactEmail fpContactEmail)

      now <- liftIO getCurrentTime
      attachmentPath <- traverse validateAndStoreAttachment fpAttachment

      Env{..} <- ask
      let emailSvc = EmailSvc.mkEmailService envConfig
      creator <- case extractTokenFromHeaders envConfig authorizationHeader cookieHeader of
        Left _ ->
          pure Nothing
        Right token ->
          liftIO $ runSqlPool (loadAuthedUser token) envPool

      _ <- liftIO $ runSqlPool
        (insert Feedback
          { feedbackTitle        = title
          , feedbackDescription  = body
          , feedbackCategory     = Nothing
          , feedbackSeverity     = Nothing
          , feedbackCategoryId   = Just categoryId
          , feedbackSeverityId   = Just severityId
          , feedbackContactEmail = contactEmail
          , feedbackAttachment   = fmap T.pack attachmentPath
          , feedbackConsent      = fpConsent
          , feedbackCreatedBy    = auPartyId <$> creator
          , feedbackCreatedAt    = now
          })
        envPool

      liftIO $ notify emailSvc title body (Just categoryLabel) (Just severityLabel) contactEmail attachmentPath

      pure NoContent

    resolvePublishedFeedbackCategory :: Text -> m (Catalog.FeedbackCategoryId, Text)
    resolvePublishedFeedbackCategory rawId = do
      Env{envPool = pool} <- ask
      categoryKey <- maybe
        (throwError err400 { errBody = "categoryId must be a valid catalog UUID" })
        pure
        (fromPathPiece (T.strip rawId))
      result <- liftIO $ runSqlPool (do
        item <- get categoryKey
        case item of
          Nothing -> pure Nothing
          Just category -> do
            state <- get (Catalog.feedbackCategoryWorkflowStateId category)
            catalog <- get (Catalog.feedbackCategoryCatalogId category)
            pure $
              if Catalog.feedbackCategoryActive category
                && Catalog.feedbackCategoryDeprecatedAt category == Nothing
                && maybe False ((== "published") . Catalog.workflowStateCode) state
                && maybe False (\definition -> Catalog.catalogDefinitionActive definition && Catalog.catalogDefinitionCode definition == "feedback-categories") catalog
                then Just (Catalog.feedbackCategoryNameEs category)
                else Nothing) pool
      label <- maybe
        (throwError err400 { errBody = "categoryId must reference an active published feedback category" })
        pure
        result
      pure (categoryKey, label)

    resolvePublishedFeedbackSeverity :: Text -> m (Catalog.FeedbackSeverityId, Text)
    resolvePublishedFeedbackSeverity rawId = do
      Env{envPool = pool} <- ask
      severityKey <- maybe
        (throwError err400 { errBody = "severityId must be a valid catalog UUID" })
        pure
        (fromPathPiece (T.strip rawId))
      result <- liftIO $ runSqlPool (do
        item <- get severityKey
        case item of
          Nothing -> pure Nothing
          Just severity -> do
            state <- get (Catalog.feedbackSeverityWorkflowStateId severity)
            catalog <- get (Catalog.feedbackSeverityCatalogId severity)
            pure $
              if Catalog.feedbackSeverityActive severity
                && Catalog.feedbackSeverityDeprecatedAt severity == Nothing
                && maybe False ((== "published") . Catalog.workflowStateCode) state
                && maybe False (\definition -> Catalog.catalogDefinitionActive definition && Catalog.catalogDefinitionCode definition == "feedback-severities") catalog
                then Just (Catalog.feedbackSeverityNameEs severity)
                else Nothing) pool
      label <- maybe
        (throwError err400 { errBody = "severityId must reference an active published feedback severity" })
        pure
        result
      pure (severityKey, label)

    validateAndStoreAttachment :: FileData Tmp -> m FilePath
    validateAndStoreAttachment file@FileData{..} = do
      (safeName, _) <-
        either throwError pure (validateFeedbackAttachmentMetadata fdFileName fdFileCType)
      size <- liftIO (getFileSize fdPayload)
      either throwError pure (validateFeedbackAttachmentSize size)
      liftIO (storeAttachment safeName file)

    storeAttachment :: Text -> FileData Tmp -> IO FilePath
    storeAttachment safeName FileData{fdPayload = payload} = do
      token <- toText <$> nextRandom
      let destDir = "uploads/feedback"
      createDirectoryIfMissing True destDir
      let destPath = destDir </> T.unpack token <> "-" <> T.unpack safeName
      BL.readFile payload >>= BL.writeFile destPath
      pure destPath

normalizeOptionalFeedbackText :: Maybe Text -> Maybe Text
normalizeOptionalFeedbackText mVal =
  case fmap T.strip mVal of
    Just txt | T.null txt -> Nothing
    other                 -> other

validateOptionalFeedbackContactEmail :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalFeedbackContactEmail Nothing = Right Nothing
validateOptionalFeedbackContactEmail (Just rawEmail) =
  case normalizeOptionalFeedbackText (Just rawEmail) of
    Nothing -> Right Nothing
    Just emailVal ->
      let normalized = T.toLower emailVal
      in if T.length normalized > maxFeedbackContactEmailChars
           then Left err400 { errBody = "contactEmail must be 254 characters or fewer" }
           else
             if isValidFeedbackEmail normalized
               then Right (Just normalized)
               else Left err400 { errBody = "contactEmail must be a valid email address" }

maxFeedbackContactEmailChars :: Int
maxFeedbackContactEmailChars = 254

maxFeedbackTitleChars :: Int
maxFeedbackTitleChars = 160

maxFeedbackDescriptionChars :: Int
maxFeedbackDescriptionChars = 5000

validateFeedbackTitle :: Text -> Either ServerError Text
validateFeedbackTitle rawTitle
  | T.null title =
      Left err400 { errBody = "title is required" }
  | T.length title > maxFeedbackTitleChars =
      Left err400 { errBody = "title must be 160 characters or fewer" }
  | T.any isUnsafeFeedbackSingleLineChar title =
      Left err400
        { errBody =
            "title must not contain control characters or hidden formatting characters"
        }
  | not (T.any isAlphaNum title) =
      Left err400 { errBody = "title must include letters or numbers" }
  | otherwise =
      Right title
  where
    title = T.strip rawTitle

validateFeedbackDescription :: Text -> Either ServerError Text
validateFeedbackDescription rawDescription
  | T.null description =
      Left err400 { errBody = "description is required" }
  | T.length description > maxFeedbackDescriptionChars =
      Left err400 { errBody = "description must be 5000 characters or fewer" }
  | T.any isDisallowedDescriptionControl description =
      Left err400
        { errBody =
            "description must not contain control characters or hidden formatting characters"
        }
  | otherwise =
      Right description
  where
    description = T.strip rawDescription
    isDisallowedDescriptionControl ch =
      (isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t')
        || isHiddenFormattingChar ch

isUnsafeFeedbackSingleLineChar :: Char -> Bool
isUnsafeFeedbackSingleLineChar ch =
  isControl ch || isHiddenFormattingChar ch

isHiddenFormattingChar :: Char -> Bool
isHiddenFormattingChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateFeedbackConsent :: Bool -> Either ServerError ()
validateFeedbackConsent True = Right ()
validateFeedbackConsent False =
  Left err400 { errBody = "consent must be accepted before submitting feedback" }

maxFeedbackAttachmentBytes :: Integer
maxFeedbackAttachmentBytes = 10 * 1024 * 1024

validateFeedbackAttachmentSize :: Integer -> Either ServerError ()
validateFeedbackAttachmentSize size
  | size < 0 =
      Left err400 { errBody = "attachment size is invalid" }
  | size == 0 =
      Left err400 { errBody = "attachment must not be empty" }
  | size > maxFeedbackAttachmentBytes =
      Left err400 { errBody = "attachment must be 10 MB or smaller" }
  | otherwise =
      Right ()

validateFeedbackAttachmentContentType :: Text -> Either ServerError Text
validateFeedbackAttachmentContentType rawContentType
  | T.null cleaned =
      Left err400 { errBody = "attachment content type is required" }
  | T.length cleaned > maxFeedbackAttachmentContentTypeChars =
      Left err400 { errBody = "attachment content type must be 100 characters or fewer" }
  | T.any isUnsafeAttachmentContentTypeChar cleaned =
      Left err400
        { errBody =
            "attachment content type must not contain control characters or hidden formatting characters"
        }
  | hasMalformedAttachmentContentTypeParameter cleaned =
      Left err400
        { errBody =
            "attachment content type parameters must be key=value tokens"
        }
  | hasAttachmentContentTypeNameParameter cleaned =
      Left err400
        { errBody =
            "attachment content type must not include filename parameters"
        }
  | mediaType `elem` allowedFeedbackAttachmentContentTypes =
      Right mediaType
  | otherwise =
      Left err400
        { errBody =
            "attachment content type must be a PDF, image, plain text, or CSV file"
        }
  where
    cleaned = T.strip rawContentType
    mediaType = T.toLower (T.strip (fst (T.breakOn ";" cleaned)))

maxFeedbackAttachmentContentTypeChars :: Int
maxFeedbackAttachmentContentTypeChars = 100

hasMalformedAttachmentContentTypeParameter :: Text -> Bool
hasMalformedAttachmentContentTypeParameter contentType =
  any (not . isValidContentTypeParameter) (drop 1 (T.splitOn ";" contentType))

isValidContentTypeParameter :: Text -> Bool
isValidContentTypeParameter rawParameter =
  let (rawKey, rawValueWithEquals) = T.breakOn "=" rawParameter
      key = T.toLower (T.strip rawKey)
      value = T.strip (T.drop 1 rawValueWithEquals)
  in not (T.null key)
       && "=" `T.isPrefixOf` rawValueWithEquals
       && T.all isContentTypeParameterKeyChar key
       && not (T.null value)

isContentTypeParameterKeyChar :: Char -> Bool
isContentTypeParameterKeyChar ch =
  isAsciiLower ch || isDigit ch || ch == '-' || ch == '*'

hasAttachmentContentTypeNameParameter :: Text -> Bool
hasAttachmentContentTypeNameParameter contentType =
  any isNameParameter (drop 1 (T.splitOn ";" contentType))
  where
    isNameParameter rawParameter =
      let key = T.toLower (T.strip (fst (T.breakOn "=" rawParameter)))
      in key `elem` ["name", "name*", "filename", "filename*"]
           || "name*" `T.isPrefixOf` key
           || "filename*" `T.isPrefixOf` key

allowedFeedbackAttachmentContentTypes :: [Text]
allowedFeedbackAttachmentContentTypes =
  [ "application/csv"
  , "application/pdf"
  , "image/gif"
  , "image/jpeg"
  , "image/png"
  , "image/webp"
  , "text/csv"
  , "text/plain"
  ]

isUnsafeAttachmentContentTypeChar :: Char -> Bool
isUnsafeAttachmentContentTypeChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateFeedbackAttachmentMetadata :: Text -> Text -> Either ServerError (Text, Text)
validateFeedbackAttachmentMetadata rawName rawContentType = do
  safeName <- validateFeedbackAttachmentFileName rawName
  mediaType <- validateFeedbackAttachmentContentType rawContentType
  validateFeedbackAttachmentExtension mediaType safeName
  Right (safeName, mediaType)

validateFeedbackAttachmentExtension :: Text -> Text -> Either ServerError ()
validateFeedbackAttachmentExtension mediaType safeName =
  let extension = T.toLower (T.pack (takeExtension (T.unpack safeName)))
  in if extension `elem` allowedFeedbackAttachmentExtensions mediaType
       then Right ()
       else
         Left err400
           { errBody = "attachment file name extension must match its content type" }

allowedFeedbackAttachmentExtensions :: Text -> [Text]
allowedFeedbackAttachmentExtensions mediaType =
  case mediaType of
    "application/csv" -> [".csv"]
    "application/pdf" -> [".pdf"]
    "image/gif" -> [".gif"]
    "image/jpeg" -> [".jpg", ".jpeg"]
    "image/png" -> [".png"]
    "image/webp" -> [".webp"]
    "text/csv" -> [".csv"]
    "text/plain" -> [".txt", ".text", ".log", ".md"]
    _ -> []

validateFeedbackAttachmentFileName :: Text -> Either ServerError Text
validateFeedbackAttachmentFileName rawName
  | T.null trimmed =
      Left err400 { errBody = "attachment file name is required" }
  | T.any isUnsafeAttachmentFileNameChar trimmed =
      Left err400
        { errBody =
            "attachment file name must not contain control characters or hidden formatting characters"
        }
  | T.any isPathSeparator trimmed =
      Left err400 { errBody = "attachment file name must not contain path separators" }
  | T.length trimmed > maxFeedbackAttachmentFileNameChars =
      Left err400
        { errBody =
            BL.fromStrict
              ( TE.encodeUtf8
                  ( "attachment file name must be "
                      <> T.pack (show maxFeedbackAttachmentFileNameChars)
                      <> " characters or fewer"
                  )
              )
        }
  | sanitized == "attachment" && trimmed /= "attachment" =
      Left err400 { errBody = "attachment file name must include a usable name" }
  | hasDisallowedFeedbackAttachmentExtension sanitized =
      Left err400 { errBody = "attachment file name extension is not allowed" }
  | otherwise =
      Right sanitized
  where
    trimmed = T.strip rawName
    sanitized = sanitizeFeedbackAttachmentFileName trimmed

isUnsafeAttachmentFileNameChar :: Char -> Bool
isUnsafeAttachmentFileNameChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

isPathSeparator :: Char -> Bool
isPathSeparator ch = ch == '/' || ch == '\\'

hasDisallowedFeedbackAttachmentExtension :: Text -> Bool
hasDisallowedFeedbackAttachmentExtension name =
  any (`elem` extensionChain) disallowedFeedbackAttachmentExtensions
  where
    loweredName = T.toLower name
    extensionChain = map ("." <>) (drop 1 (T.splitOn "." loweredName))

disallowedFeedbackAttachmentExtensions :: [Text]
disallowedFeedbackAttachmentExtensions =
  [ ".bat"
  , ".cmd"
  , ".com"
  , ".exe"
  , ".htm"
  , ".html"
  , ".jar"
  , ".js"
  , ".mjs"
  , ".php"
  , ".ps1"
  , ".scr"
  , ".sh"
  , ".svg"
  , ".svgz"
  , ".xhtml"
  ]

isValidFeedbackEmail :: Text -> Bool
isValidFeedbackEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      isValidFeedbackEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && T.isInfixOf "." domain
        && all isValidDomainLabel (T.splitOn "." domain)
        && isValidFeedbackFinalDomainLabel domain
    _ -> False

isValidFeedbackEmailLocalPart :: Text -> Bool
isValidFeedbackEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= maxFeedbackEmailLocalPartChars
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidFeedbackEmailLocalChar localPart

isValidFeedbackEmailLocalChar :: Char -> Bool
isValidFeedbackEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidDomainLabel :: Text -> Bool
isValidDomainLabel label =
  not (T.null label)
    && T.length label <= maxFeedbackEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidDomainChar label

isValidFeedbackFinalDomainLabel :: Text -> Bool
isValidFeedbackFinalDomainLabel domain =
  case reverse (T.splitOn "." domain) of
    finalLabel : _ ->
      T.length finalLabel >= 2 && T.any isAsciiLower finalLabel
    [] -> False

isValidDomainChar :: Char -> Bool
isValidDomainChar c = isAsciiLower c || isDigit c || c == '-'

maxFeedbackEmailLocalPartChars :: Int
maxFeedbackEmailLocalPartChars = 64

maxFeedbackEmailDomainLabelChars :: Int
maxFeedbackEmailDomainLabelChars = 63

sanitizeFeedbackAttachmentFileName :: Text -> Text
sanitizeFeedbackAttachmentFileName rawName =
  let trimmed = T.strip rawName
      baseName = T.pack (takeFileName (T.unpack trimmed))
      cleaned = T.map normalizeAttachmentChar baseName
      stripped = T.dropWhile (== '-') (T.dropWhileEnd (== '-') cleaned)
  in
    if T.null stripped
        || stripped == "."
        || stripped == ".."
        || not (T.any isAlphaNum stripped)
      then "attachment"
      else truncateAttachmentFileName stripped
  where
    normalizeAttachmentChar ch
      | isAscii ch && isAlphaNum ch = ch
      | ch == '.' || ch == '-' || ch == '_' = ch
      | ch == ' ' = '-'
      | otherwise = '-'

maxFeedbackAttachmentFileNameChars :: Int
maxFeedbackAttachmentFileNameChars = 120

truncateAttachmentFileName :: Text -> Text
truncateAttachmentFileName name
  | T.length name <= maxFeedbackAttachmentFileNameChars = name
  | T.length extension > 20 || T.null stem =
      T.take maxFeedbackAttachmentFileNameChars name
  | T.length extension >= maxFeedbackAttachmentFileNameChars =
      T.take maxFeedbackAttachmentFileNameChars name
  | otherwise =
      T.take stemLimit stem <> extension
  where
    (stemWithDot, ext) = T.breakOnEnd "." name
    (stem, extension) =
      if T.null stemWithDot || T.null ext
        then (name, "")
        else (T.dropEnd 1 stemWithDot, "." <> ext)
    stemLimit = maxFeedbackAttachmentFileNameChars - T.length extension

notify :: EmailSvc.EmailService -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe FilePath -> IO ()
notify emailSvc title body mCat mSev mContact attachmentPath = do
  let subject = "[TDF Feedback] " <> title
      catLine = maybe "" (\c -> "Categoría: " <> c) mCat
      sevLine = maybe "" (\s -> "Severidad: " <> s) mSev
      contactLine = maybe "Contacto: (no especificado)" (\c -> "Contacto: " <> c) (normalizeOptionalFeedbackText mContact)
      attachmentLine = maybe "Adjunto: (ninguno)" (\p -> "Adjunto: " <> T.pack p) attachmentPath
      bodyLines =
        filter (not . T.null)
          [ catLine
          , sevLine
          , contactLine
          , attachmentLine
          , ""
          , "Descripción:"
          , body
          ]
      recipients =
        [ ("Diego Saa", "diego@tdfrecords.net")
        , ("Equipo TDF", "info@tdfrecords.net")
        , ("TDF Estudio", "tdfestudiodegrabacion@gmail.com")
        ]
  forM_ recipients $ \(name, email) -> do
    sendResult <- try $
      EmailSvc.sendTestEmail emailSvc name email subject bodyLines Nothing
    case sendResult of
      Left (err :: SomeException) ->
        hPutStrLn stderr ("[Feedback] Failed to email " <> T.unpack email <> ": " <> displayException err)
      Right () -> pure ()
  pure ()
