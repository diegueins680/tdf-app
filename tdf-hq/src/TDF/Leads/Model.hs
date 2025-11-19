{-# LANGUAGE OverloadedStrings #-}
module TDF.Leads.Model where

import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import System.Random (randomRIO)

genToken :: IO Text
genToken = T.pack <$> mapM (const rand) [1..20]
  where
    alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
    rand = (alphabet !!) <$> randomRIO (0, length alphabet - 1)

ensureLead :: Connection -> Text -> Int -> IO (Int, Text)
ensureLead conn phone ceId = do
  rows <- query conn
            "SELECT id, token FROM lead WHERE phone_e164 = ? AND course_edition_id = ? LIMIT 1"
            (phone, ceId)
  case rows of
    [(lid :: Int, tok :: Text)] -> pure (lid, tok)
    _ -> do
      tok <- genToken
      result <- query conn
            "INSERT INTO lead (phone_e164, course_edition_id, token) VALUES (?,?,?) RETURNING id"
            (phone, ceId, tok)
      case result of
        [Only lid] -> pure (lid, tok)
        _ -> error "Failed to insert lead"

lookupCourseIdBySlug :: Connection -> Text -> IO (Maybe Int)
lookupCourseIdBySlug conn slug = do
  rows <- query conn "SELECT id FROM course_edition WHERE slug = ? LIMIT 1" (Only slug)
  pure $ case rows of
    [Only i] -> Just i
    _        -> Nothing
