{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Handlers.InputList
  ( InventoryItem
  , InputListEntry
  , AssetField(..)
  , parseAssetField
  , listInventoryDB
  , seedInventoryDB
  , seedHQDB
  , fetchSessionInputRowsByIndex
  , fetchSessionInputRowsByKey
  , renderInputListLatex
  , generateInputListPdf
  , generateInputListPdfWithAssets
  , sanitizeFileName
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (forM_, guard, when)
import           Data.Char                  (isAlphaNum)
import           Data.List                  (find)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.ByteString.Lazy       as BL
import           Data.Time                  (UTCTime)
import           Database.Persist
import           Database.Persist.Sql       (SqlPersistT)
import           System.Directory           (copyFile, createDirectoryIfMissing, doesDirectoryExist,
                                             findExecutable, listDirectory, withCurrentDirectory)
import           System.Exit                (ExitCode(..))
import           System.FilePath            ((</>), takeFileName)
import           System.Process             (readProcessWithExitCode)
import           System.IO.Temp             (withSystemTempDirectory)
import qualified Data.Set                   as Set

import qualified TDF.ModelsExtra            as ME
import           TDF.Seed                   (seedHolgerSession, seedInventoryAssets)

type InventoryItem = ME.Asset
type InputListEntry = ME.InputRow

data AssetField
  = AssetFieldMic
  | AssetFieldPreamp
  | AssetFieldInterface
  deriving (Eq, Show)

parseAssetField :: Text -> Maybe AssetField
parseAssetField raw =
  case T.toLower (T.strip raw) of
    "mic"         -> Just AssetFieldMic
    "microphone"  -> Just AssetFieldMic
    "pre"         -> Just AssetFieldPreamp
    "preamp"      -> Just AssetFieldPreamp
    "pre-amp"     -> Just AssetFieldPreamp
    "preamplifier"-> Just AssetFieldPreamp
    "interface"   -> Just AssetFieldInterface
    "converter"   -> Just AssetFieldInterface
    _             -> Nothing

listInventoryDB
  :: Maybe AssetField
  -> Maybe ME.SessionId
  -> Maybe Int
  -> SqlPersistT IO [Entity InventoryItem]
listInventoryDB mField mSession mChannel = do
  allAssets <- selectList [] [Asc ME.AssetName]
  let activeAssets = filter (isAssetActive . entityVal) allAssets
      fieldFiltered = maybe activeAssets (\field -> filter (matchesField field) activeAssets) mField
  case (mField, mSession) of
    (Just field, Just sessionId) -> do
      rows <- loadLatestInputRows sessionId
      let usedIds = Set.fromList (mapMaybe (rowAsset field) (map entityVal rows))
          keepId  = currentChannelAsset field rows mChannel
          available = filter (assetAvailable usedIds keepId) fieldFiltered
      pure available
    _ -> pure fieldFiltered
  where
    isAssetActive asset = ME.assetStatus asset == ME.Active

    matchesField field (Entity _ item) =
      let category = T.toLower (ME.assetCategory item)
      in case field of
           AssetFieldMic    -> "mic" `T.isInfixOf` category || "di" `T.isInfixOf` category
           AssetFieldPreamp -> "pre" `T.isInfixOf` category
           AssetFieldInterface -> "interface" `T.isInfixOf` category || "converter" `T.isInfixOf` category

    rowAsset field row =
      case field of
        AssetFieldMic    -> ME.inputRowMicId row
        AssetFieldPreamp -> ME.inputRowPreampId row
        AssetFieldInterface -> Nothing

    currentChannelAsset field rows mChan = do
      channelNum <- mChan
      guard (channelNum >= 1)
      row <- find ((== channelNum) . ME.inputRowChannelNumber) (map entityVal rows)
      rowAsset field row

    assetAvailable usedIds keepId (Entity key _) =
      let inUse = Set.member key usedIds
      in not inUse || Just key == keepId

seedInventoryDB :: SqlPersistT IO ()
seedInventoryDB = seedInventoryAssets

seedHQDB :: UTCTime -> SqlPersistT IO ()
seedHQDB = seedHolgerSession

fetchSessionInputRowsByIndex
  :: Int
  -> SqlPersistT IO (Maybe (Entity ME.Session, [Entity InputListEntry]))
fetchSessionInputRowsByIndex idx = do
  sessions <- selectList [] [Asc ME.SessionStartAt]
  case drop (max 0 (idx - 1)) sessions of
    (sessionEnt:_) -> do
      rows <- loadLatestInputRows (entityKey sessionEnt)
      pure (Just (sessionEnt, rows))
    [] -> pure Nothing

fetchSessionInputRowsByKey
  :: ME.SessionId
  -> SqlPersistT IO (Maybe (Entity ME.Session, [Entity InputListEntry]))
fetchSessionInputRowsByKey sessionKey = do
  mSession <- getEntity sessionKey
  case mSession of
    Nothing        -> pure Nothing
    Just sessionEnt -> do
      rows <- loadLatestInputRows sessionKey
      pure (Just (sessionEnt, rows))

loadLatestInputRows
  :: ME.SessionId
  -> SqlPersistT IO [Entity InputListEntry]
loadLatestInputRows sessionKey = do
  mList <- selectFirst [ME.InputListSessionId ==. sessionKey] []
  case mList of
    Nothing -> pure []
    Just (Entity listId _) -> do
      mVersion <- selectFirst
        [ ME.InputListVersionInputListId ==. listId ]
        [ Desc ME.InputListVersionVersion
        , Desc ME.InputListVersionCreatedAt
        , LimitTo 1
        ]
      case mVersion of
        Nothing -> pure []
        Just (Entity versionId _) ->
          selectList
            [ ME.InputRowVersionId ==. versionId ]
            [ Asc ME.InputRowChannelNumber ]

renderInputListLatex :: Text -> [Entity InputListEntry] -> Text
renderInputListLatex title rows =
  let escapedTitle = latexEscape title
      bodyLines    = map renderRow rows
  in T.unlines $
       [ "\\documentclass[a4paper,landscape,10pt]{article}"
       , "\\usepackage[margin=15mm]{geometry}"
       , "\\usepackage{array,booktabs,longtable,xcolor}"
       , "\\definecolor{rowalt}{RGB}{246,246,246}"
       , "\\rowcolors{2}{rowalt}{white}"
       , "\\begin{document}"
       , "\\section*{Input List --- " <> escapedTitle <> "}"
       , "\\small"
       , "\\begin{longtable}{@{}c l l c l l c l@{}}"
       , "\\toprule"
       , "\\# & Fuente & Mic/DI & Medusa & Preamp & Interfaz & DAW & Notas \\\\"
       , "\\midrule"
       ]
       ++ bodyLines ++
       [ "\\bottomrule"
       , "\\end{longtable}"
       , "\\end{document}"
       ]
  where
    renderRow (Entity _ row) =
      let noteMap      = notesToMap (ME.inputRowNotes row)
          medusaVal    = Map.lookup "Medusa" noteMap
          preampVal    = Map.lookup "Preamp" noteMap
          interfaceVal = Map.lookup "Interface" noteMap <|> ME.inputRowConverterChannel row
          dawVal       = Map.lookup "DAW Ch" noteMap
          extraNotes   = Map.lookup "Notes" noteMap
          cells =
            [ showText (ME.inputRowChannelNumber row)
            , maybe "-" id (ME.inputRowTrackName row)
            , maybe "-" id (ME.inputRowInstrument row)
            , maybe "-" id medusaVal
            , maybe "-" id preampVal
            , maybe "-" id interfaceVal
            , maybe "-" id dawVal
            , maybe "" id extraNotes
            ]
      in T.intercalate " & " (map latexEscape cells) <> " \\\\"

    showText :: Show a => a -> Text
    showText = T.pack . show

notesToMap :: Maybe Text -> Map.Map Text Text
notesToMap Nothing = Map.empty
notesToMap (Just txt) =
  Map.fromList $ mapMaybe parseChunk (T.splitOn "|" txt)
  where
    parseChunk chunk =
      let trimmed = T.strip chunk
          (key, rest) = T.breakOn ":" trimmed
      in case T.stripPrefix ":" rest of
           Nothing    -> Nothing
           Just value -> Just (T.strip key, T.strip value)

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

generateInputListPdf :: Text -> IO (Either Text BL.ByteString)
generateInputListPdf = generateInputListPdfWithAssets Nothing

generateInputListPdfWithAssets :: Maybe FilePath -> Text -> IO (Either Text BL.ByteString)
generateInputListPdfWithAssets mAssetsDir latex =
  withSystemTempDirectory "tdf-latex" $ \tmpDir -> do
    let texFile = tmpDir </> "inputlist.tex"
        pdfFile = tmpDir </> "inputlist.pdf"
        assetsDir = tmpDir </> "assets"
    TIO.writeFile texFile latex
    forM_ mAssetsDir (`copyAssetsDir` assetsDir)
    tectonicResult <- runTectonic tmpDir texFile
    case tectonicResult of
      Right () -> Right <$> BL.readFile pdfFile
      Left tectonicErr -> do
        mXe <- findExecutable "xelatex"
        case mXe of
          Nothing -> pure (Left tectonicErr)
          Just _ -> do
            xeResult <- runXeLaTeX tmpDir texFile
            case xeResult of
              Right () -> Right <$> BL.readFile pdfFile
              Left xeErr -> pure (Left (tectonicErr <> "\n" <> xeErr))

runTectonic :: FilePath -> FilePath -> IO (Either Text ())
runTectonic tmpDir texFile = do
  (exitCode, out, err) <- readProcessWithExitCode "tectonic"
    ["-Z", "shell-escape", "-o", tmpDir, texFile] ""
  case exitCode of
    ExitSuccess -> pure (Right ())
    ExitFailure code -> pure (Left (formatProcessError "tectonic" code out err))

runXeLaTeX :: FilePath -> FilePath -> IO (Either Text ())
runXeLaTeX tmpDir texFile =
  withCurrentDirectory tmpDir $ do
    let inputName = takeFileName texFile
    (exitCode, out, err) <- readProcessWithExitCode "xelatex"
      ["-interaction=nonstopmode", "-halt-on-error", inputName] ""
    case exitCode of
      ExitSuccess -> pure (Right ())
      ExitFailure code -> pure (Left (formatProcessError "xelatex" code out err))

formatProcessError :: Text -> Int -> String -> String -> Text
formatProcessError name code out err =
  let errText = T.strip (T.pack err)
      outText = T.strip (T.pack out)
      detail = if T.null errText then outText else errText
  in T.concat
       [ name
       , " exited with "
       , T.pack (show code)
       , ": "
       , detail
       ]

copyAssetsDir :: FilePath -> FilePath -> IO ()
copyAssetsDir src dest = do
  dirExists <- doesDirectoryExist src
  when dirExists (copyDirRecursive src dest)

copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dest = do
  createDirectoryIfMissing True dest
  entries <- listDirectory src
  forM_ entries $ \entry -> do
    let srcPath = src </> entry
        destPath = dest </> entry
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyDirRecursive srcPath destPath
      else copyFile srcPath destPath

sanitizeFileName :: Text -> Text
sanitizeFileName txt =
  let normalised = T.map normalizeChar (T.toLower txt)
      filtered   = T.filter (\c -> isAlphaNum c || c == '-') normalised
  in if T.null filtered then "session-input-list" else T.take 64 filtered
  where
    normalizeChar c
      | c == ' '  = '-'
      | c == '_'  = '-'
      | c == '/'  = '-'
      | otherwise = c
