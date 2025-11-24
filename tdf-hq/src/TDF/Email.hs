{-# LANGUAGE OverloadedStrings #-}
module TDF.Email
  ( generateTempPassword
  , sendWelcomeEmail
  , sendPasswordResetEmail
  , sendCourseRegistrationEmail
  , sendCoursePaymentReminderEmail
  , sendTestEmail
  ) where

import           Control.Exception        (SomeException, try)
import           Data.Char                (isAlphaNum)
import qualified Data.ByteString.Base64.URL as B64
import           Data.Maybe              (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.ByteString.Char8    as BS
import           Network.Mail.Mime        ( Address(..)
                                          , Mail(..)
                                          , emptyMail
                                          , htmlPart
                                          , plainPart
                                          )
import qualified Network.Mail.Mime        as Mime
import qualified Network.Mail.SMTP        as SMTP
import           System.Entropy           (getEntropy)
import           System.IO                (hPutStrLn, stderr)
import           Text.Printf              (printf)

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
  let subject   = "Tu acceso a TDF Records HQ"
      preheader = "Accede al panel con tu usuario y contraseña temporal."
      greeting  = if T.null name then "Hola," else "Hola " <> name <> ","
      urlLine   = mAppUrl
      bodyLines =
        [ "Se creó tu acceso a TDF Records HQ."
        , "Usuario: " <> username
        , "Contraseña temporal: " <> password
        , "Te recomendamos iniciar sesión y cambiar la contraseña inmediatamente."
        ]
      toAddr = Address (Just name) email
      mail = buildMail cfg toAddr subject preheader greeting bodyLines urlLine
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
  let subject   = "Restablecer tu contraseña de TDF Records"
      greeting  = if T.null name then "Hola," else "Hola " <> name <> ","
      baseUrl   = fromMaybe "https://tdf-app.pages.dev" mAppUrl
      sanitizedBase =
        let trimmed = T.dropWhileEnd (== '/') baseUrl
        in if T.null trimmed then baseUrl else trimmed
      resetLink = sanitizedBase <> "/reset?token=" <> token
      preheader = "Usa tu token para restablecer la contraseña de tu cuenta."
      bodyLines =
        [ "Recibimos una solicitud para restablecer tu acceso a TDF Records HQ."
        , "Usa el enlace o token para definir una nueva contraseña."
        , "Enlace: " <> resetLink
        , "Token de seguridad: " <> token
        ]
      toAddr = Address (Just name) email
      mail = buildMail cfg toAddr subject preheader greeting bodyLines (Just resetLink)
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
  let subject   = "Reserva recibida: " <> courseTitle
      preheader = "Hemos recibido tu inscripción. Te contactaremos para completar el pago."
      greeting  = if T.null name then "Hola," else "Hola " <> name <> ","
      bodyLines =
        [ "Gracias por inscribirte en " <> courseTitle <> "."
        , "Fechas: " <> datesSummary
        , "Te contactaremos por este correo para completar el proceso de pago."
        ]
      toAddr = Address (Just name) email
      mail = buildMail cfg toAddr subject preheader greeting bodyLines (Just landingUrl)
  sendMailWithLogging cfg toAddr subject mail

sendCoursePaymentReminderEmail
  :: Maybe EmailConfig
  -> Text   -- ^ recipient name
  -> Text   -- ^ recipient email
  -> Text   -- ^ course title
  -> Double -- ^ course price (USD)
  -> Int    -- ^ remaining seats
  -> Text   -- ^ landing URL
  -> IO ()
sendCoursePaymentReminderEmail Nothing _name _email _courseTitle _price _seats _landingUrl =
  putStrLn "[Email] SMTP not configured; skipped course payment reminder."
sendCoursePaymentReminderEmail (Just cfg) name email courseTitle price seats landingUrl = do
  let subject   = "Completa tu pago - " <> courseTitle
      greeting  = if T.null name then "Hola," else "Hola " <> name <> ","
      seatsLine
        | seats == 1 = "Solo queda 1 cupo disponible."
        | otherwise  = "Solo quedan " <> T.pack (show seats) <> " cupos disponibles."
      preheader = "Asegura tu cupo realizando el pago hoy mismo."
      priceLine = "Valor del curso: " <> formatUsd price
      bodyLines =
        [ "Recibimos tu inscripción a " <> courseTitle <> "."
        , "Para asegurar tu cupo realiza tu pago hoy."
        , seatsLine
        , priceLine
        , "Datos para transferencia o depósito:"
        , "Banco del Austro"
        , "Cta. Cte. #0717804813"
        , "RUC 1793215092001"
        , "Beneficiario: TDF Records S.A.S."
        , "Envíanos el comprobante respondiendo a este correo para confirmar tu cupo."
        , "Si ya realizaste el pago, por favor ignora este mensaje."
        ]
      toAddr = Address (Just name) email
      mail = buildMail cfg toAddr subject preheader greeting bodyLines (Just landingUrl)
  sendMailWithLogging cfg toAddr subject mail

formatUsd :: Double -> Text
formatUsd amount =
  let formatted = printf "%.2f" amount :: String
  in "$" <> T.pack formatted <> " USD"

-- Send a test/custom email for diagnostics.
sendTestEmail
  :: Maybe EmailConfig
  -> Text   -- ^ recipient name (optional; can be empty)
  -> Text   -- ^ recipient email
  -> Text   -- ^ subject
  -> [Text] -- ^ body paragraphs
  -> Maybe Text -- ^ optional CTA url
  -> IO ()
sendTestEmail Nothing _name _email _subject _body _cta =
  putStrLn "[Email] SMTP not configured; skipped test email."
sendTestEmail (Just cfg) name email subject bodyLines mCtaUrl = do
  let preheader = "Correo de prueba de TDF Records"
      greeting  = if T.null name then "Hola," else "Hola " <> name <> ","
      toAddr = Address (Just name) email
      mail = buildMail cfg toAddr subject preheader greeting bodyLines mCtaUrl
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

buildMail :: EmailConfig -> Address -> Text -> Text -> Text -> [Text] -> Maybe Text -> Mime.Mail
buildMail cfg toAddr subject preheader greeting bodyLines mCtaUrl =
  let fromAddr = Address (Just (emailFromName cfg)) (emailFromAddress cfg)
      mailBase = emptyMail fromAddr
      plainText = renderPlain greeting bodyLines mCtaUrl
      htmlBody = renderHtml preheader greeting bodyLines mCtaUrl
  in mailBase
      { mailTo = [toAddr]
      , mailHeaders = [("Subject", subject)]
      , mailParts = [[ plainPart (TL.fromStrict plainText)
                     , htmlPart htmlBody
                     ]]
      }

renderPlain :: Text -> [Text] -> Maybe Text -> Text
renderPlain greeting bodyLines mCtaUrl =
  T.unlines $
    [greeting, ""] <> bodyLines <> maybe [] (\url -> ["", "Ver detalles: " <> url]) mCtaUrl
    <> ["", "—", "TDF Records"]

renderHtml :: Text -> Text -> [Text] -> Maybe Text -> TL.Text
renderHtml preheader greeting bodyLines mCtaUrl =
  -- Use hosted SVG (Github raw) to avoid data URI blocking in Gmail.
  let logoUrl = "https://raw.githubusercontent.com/diegueins680/tdf-app/main/tdf-hq-ui/src/assets/tdf-logo-wordmark.svg"
      esc = escapeHtml
      bodyParas = T.concat (map (\p -> "<p style=\"margin:0 0 12px;color:#0f172a;font-size:15px;line-height:22px;\">" <> esc p <> "</p>") bodyLines)
      ctaBlock = maybe "" (\url ->
        "<table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" style=\"margin-top:20px;\"><tr><td style=\"background:#0ea5e9;padding:12px 18px;border-radius:999px;font-weight:700;\"><a href=\"" <> esc url <> "\" style=\"color:#0b0f1b;text-decoration:none;font-family:Inter,Arial,sans-serif;\">Ver detalles del curso</a></td></tr></table>"
        ) mCtaUrl
      html = T.concat
        [ "<!DOCTYPE html><html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />"
        , "<style>body{margin:0;padding:0;background:#f1f5f9;} a{color:#0ea5e9;}</style>"
        , "</head><body style=\"margin:0;padding:0;background:#f1f5f9;\">"
        , "<div style=\"display:none;max-height:0;overflow:hidden;opacity:0;\">", esc preheader, "</div>"
        , "<table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" style=\"background:#f1f5f9;padding:24px 12px;\">"
        , "<tr><td align=\"center\">"
        , "<table role=\"presentation\" width=\"640\" cellspacing=\"0\" cellpadding=\"0\" style=\"max-width:640px;width:100%;background:#ffffff;border-radius:16px;box-shadow:0 14px 40px rgba(15,23,42,0.08);overflow:hidden;border:1px solid #e2e8f0;\">"
        , "<tr><td style=\"padding:24px 32px 8px;\" align=\"center\">"
        , "<img src=\"", esc logoUrl, "\" alt=\"TDF Records\" width=\"160\" style=\"display:block;height:auto;margin:0 auto 8px;\" />"
        , "<p style=\"margin:0;color:#334155;font-size:13px;\">Escuela &amp; Estudios</p>"
        , "</td></tr>"
        , "<tr><td style=\"padding:8px 32px 24px;\">"
        , "<p style=\"margin:0 0 12px;color:#0f172a;font-size:16px;font-weight:700;\">", esc greeting, "</p>"
        , bodyParas
        , ctaBlock
        , "<div style=\"margin-top:24px;padding-top:16px;border-top:1px solid #e2e8f0;\">"
        , "<p style=\"margin:0 0 4px;color:#0f172a;font-weight:700;\">TDF Records</p>"
        , "<p style=\"margin:0;color:#475569;font-size:13px;\">Quito · Escuela &amp; Estudios</p>"
        , "</div>"
        , "</td></tr></table></td></tr></table></body></html>"
        ]
  in TL.fromStrict html

escapeHtml :: Text -> Text
escapeHtml = T.concatMap replaceChar
  where
    replaceChar '<' = "&lt;"
    replaceChar '>' = "&gt;"
    replaceChar '&' = "&amp;"
    replaceChar '"' = "&quot;"
    replaceChar '\'' = "&#39;"
    replaceChar c   = T.singleton c
