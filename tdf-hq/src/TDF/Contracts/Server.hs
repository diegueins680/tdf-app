{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.Contracts.Server
  ( server
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import           Data.Aeson.Types (parseMaybe, withObject, (.:?))
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import           GHC.Generics (Generic)
import           Servant
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>))
import           TDF.Contracts.API
import qualified TDF.Handlers.InputList as InputList
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)

data StoredContract = StoredContract
  { scId        :: Text
  , scKind      :: Text
  , scPayload   :: A.Value
  , scCreatedAt :: UTCTime
  } deriving (Show, Generic)

instance A.ToJSON StoredContract where
  toJSON StoredContract{..} = A.object
    [ "id" .= scId
    , "kind" .= scKind
    , "payload" .= scPayload
    , "created_at" .= scCreatedAt
    ]

instance A.FromJSON StoredContract where
  parseJSON = withObject "StoredContract" $ \o ->
    StoredContract
      <$> o .: "id"
      <*> o .: "kind"
      <*> o .: "payload"
      <*> o .: "created_at"

contractsDir :: FilePath
contractsDir = "contracts" </> "store"

contractPath :: Text -> FilePath
contractPath cid = contractsDir </> T.unpack cid <> ".json"

server :: Server ContractsAPI
server = createH :<|> pdfH :<|> sendH
  where
    createH :: A.Value -> Handler A.Value
    createH payload = do
      cid <- liftIO (toText <$> nextRandom)
      now <- liftIO getCurrentTime
      let kindText = extractKind payload
          stored = StoredContract
            { scId = cid
            , scKind = kindText
            , scPayload = payload
            , scCreatedAt = now
            }
      liftIO (persistContract stored)
      pure (A.object
        [ "status" .= ("created" :: Text)
        , "id" .= cid
        , "kind" .= kindText
        , "payload" .= payload
        ])

    pdfH :: Text -> Handler BL.ByteString
    pdfH cid = do
      mStored <- liftIO (loadContract cid)
      case mStored of
        Nothing -> throwError err404 { errBody = "Contract not found" }
        Just stored -> do
          let latex = renderContractLatex stored
          pdfResult <- liftIO (InputList.generateInputListPdf latex)
          case pdfResult of
            Left errMsg -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
            Right pdf -> pure pdf

    sendH :: Text -> A.Value -> Handler A.Value
    sendH cid _body = do
      mStored <- liftIO (loadContract cid)
      case mStored of
        Nothing -> throwError err404 { errBody = "Contract not found" }
        Just _  -> pure (A.object ["status" .= ("sent" :: Text), "id" .= cid])

persistContract :: StoredContract -> IO ()
persistContract stored = do
  createDirectoryIfMissing True contractsDir
  BL.writeFile (contractPath (scId stored)) (A.encode stored)

loadContract :: Text -> IO (Maybe StoredContract)
loadContract cid = do
  let path = contractPath cid
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bytes <- BL.readFile path
      pure (A.decode bytes)

renderContractLatex :: StoredContract -> Text
renderContractLatex StoredContract{..} =
  T.unlines
    [ "\\documentclass{article}"
    , "\\usepackage[margin=1in]{geometry}"
    , "\\usepackage{parskip}"
    , "\\begin{document}"
    , "\\section*{Contract " <> latexEscape scId <> "}"
    , "\\textbf{Kind:} " <> latexEscape scKind <> "\\\\"
    , "\\textbf{Created:} " <> latexEscape (T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" scCreatedAt)) <> "\\\\"
    , "\\subsection*{Payload}"
    , "\\begin{verbatim}"
    , TE.decodeUtf8 (BL.toStrict (A.encode scPayload))
    , "\\end{verbatim}"
    , "\\end{document}"
    ]

extractKind :: A.Value -> Text
extractKind val =
  case parseMaybe (withObject "payload" (.:? "kind")) val of
    Just (Just k) | not (T.null (T.strip k)) -> T.strip k
    _ -> "generic"

latexEscape :: Text -> Text
latexEscape = T.concatMap escapeChar
  where
    escapeChar c = case c of
      '&'  -> "\\&"
      '%'  -> "\\%"
      '$'  -> "\\$"
      '#'  -> "\\#"
      '_'  -> "\\_"
      '{'  -> "\\{"
      '}'  -> "\\}"
      '~'  -> "\\textasciitilde{}"
      '^'  -> "\\textasciicircum{}"
      '\\' -> "\\textbackslash{}"
      _    -> T.singleton c
