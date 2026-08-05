{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.ERN.V432.Parse
  ( -- * Parsing
    parseErnMessage
  , ParseError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, listToMaybe)
import Control.Monad (foldM)
import Text.XML.Light
import TDF.DDEX.ERN.V432.Types

-- | Parse error with location information
data ParseError = ParseError
  { peMessage :: Text
  , peElement :: Maybe Text
  , peLine    :: Maybe Int
  } deriving (Show, Eq)

-- | Parse a complete ERN 4.3.2 message from XML
parseErnMessage :: BL.ByteString -> Either [ParseError] ErnMessage
parseErnMessage content =
  case parseXmlDocument content of
    Nothing -> Left [ParseError "Invalid XML document" Nothing Nothing]
    Just root ->
      let headerResult = parseMessageHeader root
          partiesResult = parsePartyList root
          resourcesResult = parseResourceList root
          releasesResult = parseReleaseList root
          groupsResult = parseResourceGroups root
          dealsResult = parseDealList root
      in case (headerResult, partiesResult, resourcesResult, releasesResult) of
        (Right header, Right parties, Right resources, Right releases) ->
          Right ErnMessage
            { ernMessageHeader = header
            , ernPartyList = parties
            , ernResourceList = resources
            , ernReleaseList = releases
            , ernResourceGroups = fromMaybe [] groupsResult
            , ernDealList = fromMaybe [] dealsResult
            }
        _ -> Left $ catMaybes
          [ either Just (const Nothing) headerResult
          , either Just (const Nothing) partiesResult
          , either Just (const Nothing) resourcesResult
          , either Just (const Nothing) releasesResult
          ]

-- | Parse XML document from lazy ByteString
parseXmlDocument :: BL.ByteString -> Maybe Element
parseXmlDocument = parseXMLDoc . BL8.unpack

-- | Parse MessageHeader from root element
parseMessageHeader :: Element -> Either ParseError MessageHeader
parseMessageHeader root =
  case findLocalElement (unqual "MessageHeader") root of
    Nothing -> Left $ ParseError "Missing MessageHeader" Nothing Nothing
    Just headerElem -> do
      messageId <- requireText "MessageId" headerElem
      senderId <- requireText "MessageSender/PartyId" headerElem
      recipientId <- requireText "MessageRecipient/PartyId" headerElem
      createdDateTimeText <- requireText "MessageCreatedDateTime" headerElem
      case parseDateTime createdDateTimeText of
        Nothing ->
          Left $ ParseError
            "Invalid MessageCreatedDateTime format"
            (Just "MessageCreatedDateTime")
            Nothing
        Just createdDateTime -> do
          let threadId = findElementText (unqual "MessageThreadId") headerElem
          Right MessageHeader
            { mhMessageId = messageId
            , mhMessageThreadId = threadId
            , mhSenderPartyId = PartyIdDPID senderId
            , mhRecipientPartyId = PartyIdDPID recipientId
            , mhMessageCreatedDateTime = createdDateTime
            , mhMessageAuditTrail = Nothing
            }

-- | Parse PartyList from root element
parsePartyList :: Element -> Either ParseError [Party]
parsePartyList root =
  case findLocalElement (unqual "PartyList") root of
    Nothing -> Right []  -- PartyList is optional
    Just partyListElem ->
      let partyElems = findLocalChildren (unqual "Party") partyListElem
      in Right $ mapMaybe parseParty partyElems

-- | Parse a single Party element
parseParty :: Element -> Maybe Party
parseParty partyElem = do
  partyId <- parsePartyId partyElem
  let partyName = findLocalElement (unqual "PartyName") partyElem >>= parsePartyName
      ipiNumber = findElementText (unqual "IPI") partyElem
      isniNumber = findElementText (unqual "ISNI") partyElem
      dpid = case partyId of
        PartyIdDPID d -> Just d
        _ -> Nothing
  return Party
    { partyPartyId = partyId
    , partyPartyName = partyName
    , partyIpiNumber = ipiNumber
    , partyIsniNumber = isniNumber
    , partyDPID = dpid
    }

-- | Parse PartyId from various formats
parsePartyId :: Element -> Maybe PartyId
parsePartyId elem =
  case findLocalElement (unqual "PartyId") elem of
    Nothing -> Nothing
    Just pidElem ->
      case findElementText (unqual "DPID") pidElem of
        Just dpid -> Just (PartyIdDPID dpid)
        Nothing -> case findElementText (unqual "IPI") pidElem of
          Just ipi -> Just (PartyIdIPI ipi)
          Nothing -> case findElementText (unqual "ISNI") pidElem of
            Just isni -> Just (PartyIdISNI isni)
            Nothing -> case findElementText (unqual "ProprietaryId") pidElem of
              Just propId -> Just (PartyIdProprietary "Proprietary" propId)
              Nothing -> Nothing

-- | Parse PartyName element
parsePartyName :: Element -> Maybe PartyName
parsePartyName elem = do
  fullName <- findElementText (unqual "FullName") elem
  let namesToDisplay = map (T.pack . strContent)
                       (findLocalChildren (unqual "NameToDisplay") elem)
  return PartyName
    { pnFullName = fullName
    , pnNamesToDisplay = namesToDisplay
    }

-- | Parse ResourceList from root element
parseResourceList :: Element -> Either ParseError [Resource]
parseResourceList root =
  case findLocalElement (unqual "ResourceList") root of
    Nothing -> Left $ ParseError "Missing ResourceList" Nothing Nothing
    Just resourceListElem ->
      let soundRecs = findLocalChildren (unqual "SoundRecording") resourceListElem
          videos = findLocalChildren (unqual "MusicVideo") resourceListElem
          images = findLocalChildren (unqual "Image") resourceListElem
      in Right $ mapMaybe parseSoundRecording soundRecs ++
                 mapMaybe parseMusicVideo videos ++
                 mapMaybe parseImage images

-- | Parse SoundRecording element
parseSoundRecording :: Element -> Maybe Resource
parseSoundRecording elem = do
  ref <- findElementText (unqual "ResourceReference") elem
  title <-
    findLocalElement (unqual "ReferenceTitle") elem >>=
      findElementText (unqual "TitleText")
  let resType = ResourceTypeSoundRecording
      resIds = parseResourceIds elem
      subTitle =
        findLocalElement (unqual "ReferenceTitle") elem >>=
          findElementText (unqual "SubTitle")
      contributors = parseContributors elem
      duration = findElementText (unqual "Duration") elem >>= parseDuration
      language = findElementText (unqual "Language") elem
      explicitContent = findElementText (unqual "IsExplicit") elem == Just "true" ||
                        findElementText (unqual "ParentalWarningType") elem == Just "Explicit"
  return Resource
    { resourceReference = ResourceReference ref
    , resourceType = resType
    , resourceIds = resIds
    , resourceTitle = title
    , resourceSubTitle = subTitle
    , resourceContributors = contributors
    , resourceDuration = duration
    , resourceLanguage = fmap Language language
    , resourceExplicitContent = explicitContent
    , resourceParentResourceRef = Nothing
    }

-- | Parse MusicVideo element
parseMusicVideo :: Element -> Maybe Resource
parseMusicVideo elem = do
  ref <- findElementText (unqual "ResourceReference") elem
  title <- findLocalElement (unqual "ReferenceTitle") elem >>= findElementText (unqual "TitleText")
  let resType = ResourceTypeMusicVideo
      resIds = parseResourceIds elem
  return Resource
    { resourceReference = ResourceReference ref
    , resourceType = resType
    , resourceIds = resIds
    , resourceTitle = title
    , resourceSubTitle = Nothing
    , resourceContributors = []
    , resourceDuration = Nothing
    , resourceLanguage = Nothing
    , resourceExplicitContent = False
    , resourceParentResourceRef = Nothing
    }

-- | Parse Image element
parseImage :: Element -> Maybe Resource
parseImage elem = do
  ref <- findElementText (unqual "ResourceReference") elem
  let resType = ResourceTypeImage
      resIds = parseResourceIds elem
  return Resource
    { resourceReference = ResourceReference ref
    , resourceType = resType
    , resourceIds = resIds
    , resourceTitle = "Image"
    , resourceSubTitle = Nothing
    , resourceContributors = []
    , resourceDuration = Nothing
    , resourceLanguage = Nothing
    , resourceExplicitContent = False
    , resourceParentResourceRef = Nothing
    }

-- | Parse resource identifiers (ISRC, GRid, etc.)
parseResourceIds :: Element -> [ResourceId]
parseResourceIds elem =
  let recordingIds = findLocalChildren (unqual "SoundRecordingId") elem
      isrcs = mapMaybe (findElementText (unqual "ISRC")) recordingIds
      grids = mapMaybe (findElementText (unqual "GRid")) recordingIds
  in map (ResourceIdISRC . parseISRC) isrcs ++ map (ResourceIdGRid . GRid) grids

-- | Parse ISRC string into components
parseISRC :: Text -> ISRC
parseISRC isrc
  | T.length isrc >= 12 = ISRC
      { isrcCountryCode = T.take 2 isrc
      , isrcRegistrant = T.take 3 (T.drop 2 isrc)
      , isrcYear = T.take 2 (T.drop 5 isrc)
      , isrcDesignation = T.drop 7 isrc
      }
  | otherwise = ISRC isrc "" "" ""

-- | Parse contributors from element
parseContributors :: Element -> [ResourceContributor]
parseContributors elem =
  let creationInfo = findLocalElement (unqual "CreationInformation") elem
      creationDetails = creationInfo >>= findLocalElement (unqual "CreationDetails")
      contribElems = maybe [] (findLocalChildren (unqual "Contributor")) creationDetails
  in mapMaybe parseContributor contribElems

-- | Parse a single contributor
parseContributor :: Element -> Maybe ResourceContributor
parseContributor elem = do
  partyRef <- findElementText (unqual "PartyReference") elem
  role <- findElementText (unqual "Role") elem
  let creditText = findElementText (unqual "CreditText") elem
  return ResourceContributor
    { rcPartyReference = partyRef
    , rcRole = role
    , rcCreditText = creditText
    }

-- | Parse ReleaseList from root element
parseReleaseList :: Element -> Either ParseError [Release]
parseReleaseList root =
  case findLocalElement (unqual "ReleaseList") root of
    Nothing -> Left $ ParseError "Missing ReleaseList" Nothing Nothing
    Just releaseListElem ->
      let releaseElems = findLocalChildren (unqual "Release") releaseListElem
      in Right $ mapMaybe parseRelease releaseElems

-- | Parse a single Release element
parseRelease :: Element -> Maybe Release
parseRelease elem = do
  ref <- findElementText (unqual "ReleaseReference") elem
  relType <- findElementText (unqual "ReleaseType") elem >>= parseReleaseType
  title <-
    findLocalElement (unqual "ReferenceTitle") elem >>=
      findElementText (unqual "TitleText")
  let relIds = parseReleaseIds elem
      subTitle =
        findLocalElement (unqual "ReferenceTitle") elem >>=
          findElementText (unqual "SubTitle")
      contributors = parseReleaseContributors elem
      resourceRefs = parseResourceReferences elem
      duration = findElementText (unqual "Duration") elem >>= parseDuration
      releaseDate = findElementText (unqual "ReleaseDate") elem >>= parseDate
      rights = findLocalElement (unqual "Rights") elem
      copyrightLine =
        rights >>=
          findLocalElement (unqual "CopyrightLine") >>=
            findElementText (unqual "Line")
      phonographicCopyrightLine =
        rights >>=
          findLocalElement (unqual "PLine") >>=
            findElementText (unqual "Line")
      genre = findElementText (unqual "Genre") elem
  return Release
    { releaseReference = ReleaseReference ref
    , releaseType = relType
    , releaseIds = relIds
    , releaseTitle = title
    , releaseSubTitle = subTitle
    , releaseContributors = contributors
    , releaseResourceRefs = resourceRefs
    , releaseDuration = duration
    , releaseDate = releaseDate
    , releaseCopyrightLine = copyrightLine
    , releasePhonographicCopyrightLine = phonographicCopyrightLine
    , releaseGenre = genre
    }

-- | Parse release contributors from Release element
parseReleaseContributors :: Element -> [ReleaseContributor]
parseReleaseContributors elem =
  let contribElems = findLocalChildren (unqual "Contributor") elem
  in mapMaybe parseReleaseContributor contribElems

-- | Parse a single release contributor
parseReleaseContributor :: Element -> Maybe ReleaseContributor
parseReleaseContributor elem = do
  partyRef <- findElementText (unqual "PartyReference") elem
  role <- findElementText (unqual "Role") elem
  return ReleaseContributor
    { relcPartyReference = partyRef
    , relcRole = role
    }

-- | Parse release identifiers (UPC, GRid, etc.)
parseReleaseIds :: Element -> [ReleaseId]
parseReleaseIds elem =
  let releaseIdElem = findLocalElement (unqual "ReleaseId") elem
      upcs = maybe [] (\e -> mapMaybe (findElementText (unqual "UPC")) [e]) releaseIdElem
      grids = maybe [] (\e -> mapMaybe (findElementText (unqual "GRid")) [e]) releaseIdElem
  in map ReleaseIdUPC upcs ++ map (ReleaseIdGRid . GRid) grids

-- | Parse resource references from ExternalResourceLink elements
parseResourceReferences :: Element -> [ResourceReference]
parseResourceReferences elem =
  let links = findLocalChildren (unqual "ExternalResourceLink") elem
  in mapMaybe
       (fmap ResourceReference . findElementText (unqual "ResourceReference"))
       links

-- | Parse ReleaseType from text
parseReleaseType :: Text -> Maybe ReleaseType
parseReleaseType "Album" = Just ReleaseTypeAlbum
parseReleaseType "Single" = Just ReleaseTypeSingle
parseReleaseType "EP" = Just ReleaseTypeEP
parseReleaseType "Compilation" = Just ReleaseTypeCompilation
parseReleaseType "LiveAlbum" = Just ReleaseTypeLiveAlbum
parseReleaseType "RemixAlbum" = Just ReleaseTypeRemixAlbum
parseReleaseType "Soundtrack" = Just ReleaseTypeSoundtrack
parseReleaseType "SpokenWord" = Just ReleaseTypeSpokenWord
parseReleaseType _ = Nothing

-- | Parse ResourceGroups from root element
parseResourceGroups :: Element -> Maybe [ResourceGroup]
parseResourceGroups root =
  case findLocalElement (unqual "ResourceGroup") root of
    Nothing -> Nothing
    Just groupElem -> Just [parseResourceGroup groupElem]

-- | Parse a single ResourceGroup (supports nested groups for multi-disc albums)
parseResourceGroup :: Element -> ResourceGroup
parseResourceGroup elem =
  let seqNum = findElementText (unqual "SequenceNumber") elem >>= safeReadInt
      title = findElementText (unqual "Title") elem
      content = parseResourceGroupContent elem
      subGroups = parseSubResourceGroups elem
  in ResourceGroup
    { rgSequenceNumber = seqNum
    , rgTitle = title
    , rgContent = content
    , rgSubGroups = subGroups
    }

-- | Parse nested ResourceGroups (for multi-disc albums)
parseSubResourceGroups :: Element -> [ResourceGroup]
parseSubResourceGroups elem =
  let subGroupElems = findLocalChildren (unqual "ResourceGroup") elem
  in mapMaybe parseSubResourceGroup subGroupElems

-- | Parse a nested ResourceGroup (returns Nothing if it's the same as parent)
parseSubResourceGroup :: Element -> Maybe ResourceGroup
parseSubResourceGroup elem =
  let seqNum = findElementText (unqual "SequenceNumber") elem >>= safeReadInt
      title = findElementText (unqual "Title") elem
      content = parseResourceGroupContent elem
      -- Check if this has actual content or is just a container
      hasContent = not (null content) || not (null (parseSubResourceGroups elem))
  in if hasContent
     then Just ResourceGroup
       { rgSequenceNumber = seqNum
       , rgTitle = title
       , rgContent = content
       , rgSubGroups = parseSubResourceGroups elem
       }
     else Nothing

-- | Parse ResourceGroupContent
parseResourceGroupContent :: Element -> [ResourceGroupContent]
parseResourceGroupContent elem =
  let items = findLocalChildren (unqual "ResourceGroupContentItem") elem
  in mapMaybe parseResourceGroupContentItem items

-- | Parse a single ResourceGroupContentItem
parseResourceGroupContentItem :: Element -> Maybe ResourceGroupContent
parseResourceGroupContentItem elem = do
  ref <- findElementText (unqual "ResourceReference") elem
  let seqNum = findElementText (unqual "SequenceNumber") elem >>= safeReadInt
  return $ case seqNum of
    Just n -> RGCSequence n (ResourceReference ref)
    Nothing -> RGCResource (ResourceReference ref)

-- | Parse DealList from root element
parseDealList :: Element -> Maybe [Deal]
parseDealList root =
  findLocalElement (unqual "DealList") root >>= \dealListElem ->
    let dealElems = findLocalChildren (unqual "Deal") dealListElem
    in Just $ mapMaybe parseDeal dealElems

-- | Parse a single Deal element
parseDeal :: Element -> Maybe Deal
parseDeal elem = do
  terms <- parseDealTerms elem
  let releaseRefs = map (ReleaseReference . T.pack . strContent)
                    (findLocalChildren (unqual "ReleaseReference") elem)
      resourceRefs = []
      effectiveDate = DateYear 2024  -- Placeholder
  return Deal
    { dealDealTerms = terms
    , dealReleaseRefs = releaseRefs
    , dealResourceRefs = resourceRefs
    , dealEffectiveDate = effectiveDate
    }

-- | Parse DealTerms from Deal element
parseDealTerms :: Element -> Maybe DealTerms
parseDealTerms elem = do
  termsElem <- findLocalElement (unqual "DealTerms") elem
  territoryCodes <- parseTerritoryCodes termsElem
  let usage = findLocalElement (unqual "Usage") termsElem
  usageType <- case usage >>= findElementText (unqual "UseType") of
    Just useType -> Just useType
    Nothing -> findElementText (unqual "CommercialModelType") termsElem
  let priceType = findElementText (unqual "PriceType") termsElem
      wholesalePrice = findElementText (unqual "WholesalePricePerUnit") termsElem
      retailPrice = findElementText (unqual "RetailPricePerUnit") termsElem
      startDate = fromMaybe (DateYear 2024) $
        findElementText (unqual "ValidityStartDate") termsElem >>= parseDate
      endDate = findElementText (unqual "ValidityEndDate") termsElem >>= parseDate
      takedownDate = findElementText (unqual "TakedownDate") termsElem >>= parseDate
  return DealTerms
    { dtTerritoryCodes = territoryCodes
    , dtUsageType = usageType
    , dtPriceType = priceType
    , dtWholesalePrice = wholesalePrice
    , dtRetailPrice = retailPrice
    , dtStartDate = startDate
    , dtEndDate = endDate
    , dtTakedownDate = takedownDate
    }

-- | Parse territory codes from Deal element
parseTerritoryCodes :: Element -> Maybe [TerritoryCode]
parseTerritoryCodes elem =
  let territories = findLocalChildren (unqual "TerritoryCode") elem
      codes = map (parseTerritoryCode . T.pack . strContent) territories
  in Just codes

-- | Parse a single territory code
parseTerritoryCode :: Text -> TerritoryCode
parseTerritoryCode "Worldwide" = TerritoryWorldwide
parseTerritoryCode code = TerritoryCode code

-- | Parse duration from ISO 8601 format (PT##H##M##S)
parseDuration :: Text -> Maybe Duration
parseDuration text = do
  rest <- T.stripPrefix "PT" text
  (hours, afterHours) <- parseOptionalDurationComponent "H" rest
  (minutes, afterMinutes) <- parseOptionalDurationComponent "M" afterHours
  (seconds, trailing) <- parseOptionalDurationComponent "S" afterMinutes
  if T.null trailing
    then Just Duration
      { durationHours = hours
      , durationMinutes = minutes
      , durationSeconds = seconds
      }
    else Nothing

parseOptionalDurationComponent :: Text -> Text -> Maybe (Int, Text)
parseOptionalDurationComponent marker input =
  let (rawValue, suffix) = T.breakOn marker input
  in if T.null suffix
       then Just (0, input)
       else do
         value <- safeReadInt rawValue
         Just (value, T.drop (T.length marker) suffix)

-- | Parse date from various formats
parseDate :: Text -> Maybe Date
parseDate text
  | T.length text == 4 = DateYear <$> safeReadInt text
  | T.length text == 7 = do
      year <- safeReadInt (T.take 4 text)
      month <- safeReadInt (T.drop 5 text)
      Just (DateYearMonth year month)
  | otherwise = DateFull <$> parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack text)

-- | Parse datetime from ISO 8601 format
parseDateTime :: Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . T.unpack

-- | Helper to find element text
findElementText :: QName -> Element -> Maybe Text
findElementText qname elem =
  findLocalElement qname elem >>= Just . T.pack . strContent

-- | Find direct children by local name, regardless of the XML namespace URI.
findLocalElement :: QName -> Element -> Maybe Element
findLocalElement qname = listToMaybe . findLocalChildren qname

findLocalChildren :: QName -> Element -> [Element]
findLocalChildren qname =
  filter ((== qName qname) . qName . elName) . elChildren

findPathText :: Text -> Element -> Maybe Text
findPathText path elem = do
  target <- foldM findPathSegment elem (filter (not . T.null) (T.splitOn "/" path))
  Just (T.pack (strContent target))
  where
    findPathSegment parent segment =
      findLocalElement (unqual (T.unpack segment)) parent

-- | Require text from element, returning error if missing
requireText :: Text -> Element -> Either ParseError Text
requireText path elem =
  case findPathText path elem of
    Just t -> Right t
    Nothing -> Left $ ParseError ("Missing required field: " <> path) Nothing Nothing

-- | Safe read for Int, returns Nothing on failure
safeReadInt :: Text -> Maybe Int
safeReadInt text =
  case reads (T.unpack text) of
    [(val, "")] -> Just val
    _ -> Nothing

-- | Read for Int (legacy, uses safeReadInt)
readMaybe :: Read a => Text -> a
readMaybe text =
  case reads (T.unpack text) of
    [(val, "")] -> val
    _ -> error $ "Could not parse: " ++ T.unpack text
