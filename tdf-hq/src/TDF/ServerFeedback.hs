{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.ServerFeedback
  ( feedbackServer
  , normalizeOptionalFeedbackText
  , validateOptionalFeedbackContactEmail
  ) where

import           Control.Exception         (SomeException, displayException, try)
import           Control.Monad              (forM_, when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.Time                  (getCurrentTime)
import           Database.Persist           (insert)
import           Database.Persist.Sql       (runSqlPool)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp)
import           System.Directory           (createDirectoryIfMissing)
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
      let title = T.strip fpTitle
          body  = T.strip fpDescription
          category = normalizeOptionalFeedbackText fpCategory
          severity = normalizeOptionalFeedbackText fpSeverity
      when (T.null title) $
        throwError err400 { errBody = "title is required" }
      when (T.null body) $
        throwError err400 { errBody = "description is required" }
      contactEmail <- either throwError pure (validateOptionalFeedbackContactEmail fpContactEmail)

      now <- liftIO getCurrentTime
      attachmentPath <- liftIO $ traverse storeAttachment fpAttachment

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

    storeAttachment :: FileData Tmp -> IO FilePath
    storeAttachment FileData{..} = do
      let safeName = sanitize (T.pack (takeFileName (T.unpack fdFileName)))
      token <- toText <$> nextRandom
      let destDir = "uploads/feedback"
      createDirectoryIfMissing True destDir
      let destPath = destDir </> T.unpack token <> "-" <> T.unpack safeName
      BL.readFile fdPayload >>= BL.writeFile destPath
      pure destPath

    sanitize :: Text -> Text
    sanitize = T.filter (\c -> c /= '/' && c /= '\\')

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

isValidFeedbackEmail :: Text -> Bool
isValidFeedbackEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      not (T.null localPart)
        && not (T.null domain)
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && not (T.isPrefixOf "." domain)
        && not (T.isSuffixOf "." domain)
        && T.isInfixOf "." domain
    _ -> False

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
