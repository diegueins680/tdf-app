{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.ServerFeedback
  ( feedbackServer
  , normalizeOptionalFeedbackText
  , validateFeedbackTitle
  , validateOptionalFeedbackContactEmail
  , validateFeedbackAttachmentSize
  , sanitizeFeedbackAttachmentFileName
  ) where

import           Control.Exception         (SomeException, displayException, try)
import           Control.Monad              (forM_, when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Char                  (isAlphaNum, isAscii, isAsciiLower, isControl, isDigit)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.Time                  (getCurrentTime)
import           Database.Persist           (insert)
import           Database.Persist.Sql       (runSqlPool)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp)
import           System.Directory           (createDirectoryIfMissing, getFileSize)
import           System.FilePath            ((</>), takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy       as BL
import           Data.UUID.V4               (nextRandom)
import           Data.UUID                  (toText)

import           TDF.API.Feedback
import           TDF.Auth                   (AuthedUser(..))
import           TDF.DB                     (Env(..))
import           TDF.ModelsExtra            (Feedback(..))
import qualified TDF.Email.Service          as EmailSvc

feedbackServer
  :: forall m.
     ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT FeedbackAPI m
feedbackServer user = submitFeedback
  where
    submitFeedback :: FeedbackPayload -> m NoContent
    submitFeedback FeedbackPayload{..} = do
      title <- either throwError pure (validateFeedbackTitle fpTitle)
      let body  = T.strip fpDescription
          category = normalizeOptionalFeedbackText fpCategory
          severity = normalizeOptionalFeedbackText fpSeverity
      when (T.null body) $
        throwError err400 { errBody = "description is required" }
      contactEmail <- either throwError pure (validateOptionalFeedbackContactEmail fpContactEmail)

      now <- liftIO getCurrentTime
      attachmentPath <- traverse validateAndStoreAttachment fpAttachment

      Env{..} <- ask
      let emailSvc = EmailSvc.mkEmailService envConfig

      _ <- liftIO $ runSqlPool
        (insert Feedback
          { feedbackTitle        = title
          , feedbackDescription  = body
          , feedbackCategory     = category
          , feedbackSeverity     = severity
          , feedbackContactEmail = contactEmail
          , feedbackAttachment   = fmap T.pack attachmentPath
          , feedbackConsent      = fpConsent
          , feedbackCreatedBy    = Just (auPartyId user)
          , feedbackCreatedAt    = now
          })
        envPool

      liftIO $ notify emailSvc title body category severity contactEmail attachmentPath

      pure NoContent

    validateAndStoreAttachment :: FileData Tmp -> m FilePath
    validateAndStoreAttachment file@FileData{..} = do
      size <- liftIO (getFileSize fdPayload)
      either throwError pure (validateFeedbackAttachmentSize size)
      liftIO (storeAttachment file)

    storeAttachment :: FileData Tmp -> IO FilePath
    storeAttachment FileData{..} = do
      let safeName = sanitizeFeedbackAttachmentFileName (T.pack (takeFileName (T.unpack fdFileName)))
      token <- toText <$> nextRandom
      let destDir = "uploads/feedback"
      createDirectoryIfMissing True destDir
      let destPath = destDir </> T.unpack token <> "-" <> T.unpack safeName
      BL.readFile fdPayload >>= BL.writeFile destPath
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
      in if isValidFeedbackEmail normalized
           then Right (Just normalized)
           else Left err400 { errBody = "contactEmail must be a valid email address" }

maxFeedbackTitleChars :: Int
maxFeedbackTitleChars = 160

validateFeedbackTitle :: Text -> Either ServerError Text
validateFeedbackTitle rawTitle
  | T.null title =
      Left err400 { errBody = "title is required" }
  | T.length title > maxFeedbackTitleChars =
      Left err400 { errBody = "title must be 160 characters or fewer" }
  | T.any isControl title =
      Left err400 { errBody = "title must not contain control characters" }
  | otherwise =
      Right title
  where
    title = T.strip rawTitle

maxFeedbackAttachmentBytes :: Integer
maxFeedbackAttachmentBytes = 10 * 1024 * 1024

validateFeedbackAttachmentSize :: Integer -> Either ServerError ()
validateFeedbackAttachmentSize size
  | size < 0 =
      Left err400 { errBody = "attachment size is invalid" }
  | size > maxFeedbackAttachmentBytes =
      Left err400 { errBody = "attachment must be 10 MB or smaller" }
  | otherwise =
      Right ()

isValidFeedbackEmail :: Text -> Bool
isValidFeedbackEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      isValidFeedbackEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && T.isInfixOf "." domain
        && all isValidDomainLabel (T.splitOn "." domain)
    _ -> False

isValidFeedbackEmailLocalPart :: Text -> Bool
isValidFeedbackEmailLocalPart localPart =
  not (T.null localPart)
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
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidDomainChar label

isValidDomainChar :: Char -> Bool
isValidDomainChar c = isAsciiLower c || isDigit c || c == '-'

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
      else stripped
  where
    normalizeAttachmentChar ch
      | isAscii ch && isAlphaNum ch = ch
      | ch == '.' || ch == '-' || ch == '_' = ch
      | ch == ' ' = '-'
      | otherwise = '-'

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
