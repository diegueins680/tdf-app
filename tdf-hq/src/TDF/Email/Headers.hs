{-# LANGUAGE OverloadedStrings #-}
module TDF.Email.Headers
  ( prepareOutgoingMail
  , stampOutgoingMail
  ) where

import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Network.Mail.Mime (Mail(..), Address(..))

-- Generate the identity before handing DATA to SMTP so the signed message and
-- the application audit trail refer to the same attempt. No recipient data is
-- embedded in the identifier.
prepareOutgoingMail :: Mail -> IO (Text, Mail)
prepareOutgoingMail mail = do
  now <- getCurrentTime
  nonce <- UUID.toText <$> UUID.nextRandom
  pure (stampOutgoingMail now nonce mail)

stampOutgoingMail :: UTCTime -> Text -> Mail -> (Text, Mail)
stampOutgoingMail now nonce mail =
  let domain = T.takeWhileEnd (/= '@') (addressEmail (mailFrom mail))
      messageId = "<" <> nonce <> "@" <> domain <> ">"
      date = T.pack (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0000" now)
      remaining = filter (\(name, _) -> BS.map toLower name `notElem` ["date", "message-id"])
        (mailHeaders mail)
  in (messageId, mail { mailHeaders = [("Date", date), ("Message-ID", messageId)] <> remaining })
