{-# LANGUAGE OverloadedStrings #-}
module TDF.Email
  ( generateTempPassword
  , sendWelcomeEmail
  , sendPasswordResetEmail
  , sendCourseRegistrationEmail
  ) where

import           Control.Exception        (SomeException, try)
import           Data.Char                (isAlphaNum)
import qualified Data.ByteString.Base64.URL as B64
import           Data.Maybe              (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Encoding       as TE
import qualified Data.ByteString.Char8    as BS
import           Network.Mail.Mime        (Address(..), simpleMail')
import qualified Network.Mail.Mime        as Mime
import qualified Network.Mail.SMTP        as SMTP
import           System.Entropy           (getEntropy)
import           System.IO                (hPutStrLn, stderr)

import           TDF.Config               (EmailConfig(..))

generateTempPassword :: IO Text
generateTempPassword = do
  bytes <- getEntropy 24
  let raw = T.filter isAlphaNum . T.map replaceDash . decode $ bytes
      token = T.take 18 raw
  pure (ensureComplexity token)
  where
    decode = TE.decodeUtf8 . B64.encode
    replaceDash '-' = 'X'
    replaceDash '_' = 'Y'
    replaceDash c   = c
    ensureComplexity txt =
      let base = if T.length txt >= 12 then txt else txt <> "TDFRecords"
      in base <> "!1"

sendWelcomeEmail
  :: Maybe EmailConfig
  -> Text   -- ^ recipient name
  -> Text   -- ^ recipient email
  -> Text   -- ^ username
  -> Text   -- ^ password
  -> Maybe Text -- ^ optional app URL
  -> IO ()
sendWelcomeEmail Nothing name email username password _ = do
  putStrLn $ "SMTP configuration missing; skipped welcome email for "
    <> T.unpack name <> " <" <> T.unpack email <> ">."
  putStrLn $ "Temporary password for " <> T.unpack username <> ": " <> T.unpack password
sendWelcomeEmail (Just cfg) name email username password mAppUrl = do
  let subject = "Tu acceso a TDF Records HQ"
      greeting = if T.null name then "Hola," else "Hola " <> name <> ","
      urlLine = maybe "" (\url -> "Inicia sesión en: " <> url <> "\n") mAppUrl
      body =
        T.unlines
          [ greeting
          , ""
          , "Se creó tu acceso a TDF Records HQ."
          , "Usuario: " <> username
          , "Contraseña temporal: " <> password
          , ""
          , "Te recomendamos iniciar sesión y cambiar la contraseña inmediatamente."
          , urlLine
          , "Equipo TDF Records"
          ]
      fromAddr = Address (Just (emailFromName cfg)) (emailFromAddress cfg)
      toAddr = Address (Just name) email
      mail = simpleMail' toAddr fromAddr subject (TL.fromStrict body)
  sendMailWithLogging cfg toAddr subject mail

sendPasswordResetEmail
  :: Maybe EmailConfig
  -> Text   -- ^ recipient name
  -> Text   -- ^ recipient email
  -> Text   -- ^ reset token
  -> Maybe Text -- ^ optional app URL
  -> IO ()
sendPasswordResetEmail Nothing name email token mAppUrl = do
  putStrLn $ "SMTP configuration missing; skipped password reset email for "
    <> T.unpack name <> " <" <> T.unpack email <> ">."
  putStrLn $ "Reset token: " <> T.unpack token
  maybe (pure ()) (\url -> putStrLn $ "Use base URL: " <> T.unpack url) mAppUrl
sendPasswordResetEmail (Just cfg) name email token mAppUrl = do
  let subject = "Restablecer tu contraseña de TDF Records"
      greeting = if T.null name then "Hola," else "Hola " <> name <> ","
      baseUrl = fromMaybe "https://tdf-app.pages.dev" mAppUrl
      sanitizedBase =
        let trimmed = T.dropWhileEnd (== '/') baseUrl
        in if T.null trimmed then baseUrl else trimmed
      resetLink = sanitizedBase <> "/reset?token=" <> token
      body =
        T.unlines
          [ greeting
          , ""
          , "Recibimos una solicitud para restablecer tu acceso a TDF Records HQ."
          , "Usa el siguiente enlace o token temporal para definir una nueva contraseña:"
          , ""
          , "Enlace: " <> resetLink
          , "Token: " <> token
          , ""
          , "Si no hiciste esta solicitud puedes ignorar este mensaje."
          ]
      fromAddr = Address (Just (emailFromName cfg)) (emailFromAddress cfg)
      toAddr = Address (Just name) email
      mail = simpleMail' toAddr fromAddr subject (TL.fromStrict body)
  sendMailWithLogging cfg toAddr subject mail

sendCourseRegistrationEmail
  :: Maybe EmailConfig
  -> Text   -- ^ recipient name
  -> Text   -- ^ recipient email
  -> Text   -- ^ course title
  -> Text   -- ^ landing URL
  -> Text   -- ^ dates summary
  -> IO ()
sendCourseRegistrationEmail Nothing _name _email _courseTitle _landingUrl _datesSummary =
  putStrLn "[Email] SMTP not configured; skipped course registration email."
sendCourseRegistrationEmail (Just cfg) name email courseTitle landingUrl datesSummary = do
  let subject = "Reserva recibida: " <> courseTitle
      greeting = if T.null name then "Hola," else "Hola " <> name <> ","
      body =
        T.unlines
          [ greeting
          , ""
          , "Gracias por inscribirte en " <> courseTitle <> "."
          , "Fechas: " <> datesSummary
          , "Landing: " <> landingUrl
          , ""
          , "Te contactaremos por este correo para completar el proceso de pago."
          , ""
          , "Equipo TDF Records"
          ]
      fromAddr = Address (Just (emailFromName cfg)) (emailFromAddress cfg)
      toAddr = Address (Just name) email
      mail = simpleMail' toAddr fromAddr subject (TL.fromStrict body)
  sendMailWithLogging cfg toAddr subject mail

-- | Send an email with a small audit trail for admins.
sendMailWithLogging :: EmailConfig -> Address -> Text -> Mime.Mail -> IO ()
sendMailWithLogging cfg toAddr subject mail = do
  let host = T.unpack (smtpHost cfg)
      port = fromIntegral (smtpPort cfg)
      user = T.unpack (smtpUsername cfg)
      pass = T.unpack (smtpPassword cfg)
      modeLabel
        | smtpUseTLS cfg && port == 465 = "SMTPS"
        | smtpUseTLS cfg                = "STARTTLS"
        | otherwise                     = "PLAIN"
      sendAction
        | modeLabel == "SMTPS"    = SMTP.sendMailWithLoginTLS' host port user pass mail
        | modeLabel == "STARTTLS" = SMTP.sendMailWithLoginSTARTTLS' host port user pass mail
        | otherwise               = SMTP.sendMailWithLogin' host port user pass mail
      toEmail = T.unpack (addressEmail toAddr)
      fromEmail = T.unpack (emailFromAddress cfg)
      subj = T.unpack subject
  let logLine = T.concat
        [ "[Email] Sending registration email to "
        , T.pack toEmail
        , " via "
        , smtpHost cfg
        , ":"
        , T.pack (show port)
        , " ("
        , T.pack modeLabel
        , ")"
        ]
  BS.putStrLn (TE.encodeUtf8 logLine)
  result <- try (sendAction :: IO ())
  case result of
    Left err -> do
      let errLine = T.concat
            [ "[Email] Failed to send to "
            , T.pack toEmail
            , ": "
            , T.pack (show (err :: SomeException))
            ]
      BS.hPutStrLn stderr (TE.encodeUtf8 errLine)
    Right () ->
      BS.putStrLn (TE.encodeUtf8 ("[Email] Sent registration email to " <> T.pack toEmail))
