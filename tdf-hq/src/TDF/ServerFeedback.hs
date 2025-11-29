{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerFeedback
  ( feedbackServer
  ) where

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.Time                  (getCurrentTime)
import           Database.Persist           (insert)
import           Database.Persist.Sql       (runSqlPool)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>), takeFileName)
import qualified Data.ByteString.Lazy       as BL
import           Data.UUID.V4               (nextRandom)
import           Data.UUID                  (toText)

import           TDF.API.Feedback
import           TDF.Auth                   (AuthedUser(..))
import           TDF.DB                     (Env(..))
import           TDF.ModelsExtra            (Feedback(..))
import qualified TDF.ModelsExtra            as ME
import qualified TDF.Services               as Services
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
      when (T.null title) $
        throwError err400 { errBody = "title is required" }
      when (T.null body) $
        throwError err400 { errBody = "description is required" }

      now <- liftIO getCurrentTime
      attachmentPath <- liftIO $ traverse storeAttachment fpAttachment

      Env{..} <- ask
      let services = Services.buildServices envConfig
          emailSvc = Services.emailService services

      _ <- liftIO $ runSqlPool
        (insert Feedback
          { feedbackTitle        = title
          , feedbackDescription  = body
          , feedbackCategory     = T.strip <$> fpCategory
          , feedbackSeverity     = T.strip <$> fpSeverity
          , feedbackContactEmail = sanitizeContact fpContactEmail
          , feedbackAttachment   = fmap T.pack attachmentPath
          , feedbackConsent      = fpConsent
          , feedbackCreatedBy    = Just (auPartyId user)
          , feedbackCreatedAt    = now
          })
        envPool

      liftIO $ notify emailSvc title body fpCategory fpSeverity fpContactEmail attachmentPath

      pure NoContent

    sanitizeContact :: Maybe Text -> Maybe Text
    sanitizeContact mVal =
      case fmap T.strip mVal of
        Just txt | T.null txt -> Nothing
        other                 -> other

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

notify :: EmailSvc.EmailService -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe FilePath -> IO ()
notify emailSvc title body mCat mSev mContact attachmentPath = do
  let subject = "[TDF Feedback] " <> title
      catLine = maybe "" (\c -> "Categoría: " <> c) mCat
      sevLine = maybe "" (\s -> "Severidad: " <> s) mSev
      contactLine = maybe "Contacto: (no especificado)" (\c -> "Contacto: " <> c) (sanitizeContact mContact)
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
  mapM_
    (\(name, email) -> EmailSvc.sendTestEmail emailSvc name email subject bodyLines Nothing)
    recipients
  pure ()
  where
    sanitizeContact :: Maybe Text -> Maybe Text
    sanitizeContact = sanitizeMaybe

    sanitizeMaybe :: Maybe Text -> Maybe Text
    sanitizeMaybe mVal =
      case fmap T.strip mVal of
        Just txt | T.null txt -> Nothing
        other                 -> other
