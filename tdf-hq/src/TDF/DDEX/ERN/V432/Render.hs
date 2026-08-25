{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.ERN.V432.Render
  ( -- * Rendering
    renderErnMessage
  , RenderConfig(..)
  , defaultRenderConfig
  , RenderError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Text.XML.Light
import TDF.DDEX.ERN.V432.Types

-- | Configuration for ERN rendering
data RenderConfig = RenderConfig
  { rcNamespace      :: Text    -- ^ ERN namespace URI
  , rcSchemaVersion  :: Text    -- ^ Schema version (e.g., "ern/432")
  , rcLanguage       :: Text    -- ^ Document language
  , rcPrettyPrint    :: Bool    -- ^ Whether to pretty-print XML
  } deriving (Show, Eq)

-- | Default render configuration for ERN 4.3.2
defaultRenderConfig :: RenderConfig
defaultRenderConfig = RenderConfig
  { rcNamespace = "http://ddex.net/xml/ern/432"
  , rcSchemaVersion = "ern/432"
  , rcLanguage = "en"
  , rcPrettyPrint = True
  }

-- | Render error
data RenderError = RenderError
  { reMessage :: Text
  , reElement :: Maybe Text
  } deriving (Show, Eq)

-- | Render an ERN message to XML ByteString
renderErnMessage :: RenderConfig -> ErnMessage -> Either [RenderError] BL.ByteString
renderErnMessage config ern =
  let rootElem = buildRoot config ern
      xmlStr = showElement rootElem
  in Right $ BL8.pack xmlStr

-- | Build root element
buildRoot :: RenderConfig -> ErnMessage -> Element
buildRoot config ern = Element
  { elName = QName "ernNewReleaseMessage" Nothing Nothing
  , elAttribs =
      [ Attr (QName "xmlns" Nothing Nothing) (T.unpack $ rcNamespace config)
      , Attr (QName "MessageSchemaVersionId" Nothing Nothing) (T.unpack $ rcSchemaVersion config)
      , Attr (QName "BusinessProfileVersionId" Nothing Nothing) (T.unpack $ rcSchemaVersion config)
      , Attr (QName "Language" Nothing Nothing) (T.unpack $ rcLanguage config)
      ]
  , elContent =
      [ Elem $ buildMessageHeader config $ ernMessageHeader ern
      , Elem $ buildPartyList $ ernPartyList ern
      , Elem $ buildResourceList $ ernResourceList ern
      , Elem $ buildReleaseList $ ernReleaseList ern
      , Elem $ buildResourceGroup $ ernResourceGroups ern
      , Elem $ buildDealList $ ernDealList ern
      ]
  , elLine = Nothing
  }

-- | Build MessageHeader element
buildMessageHeader :: RenderConfig -> MessageHeader -> Element
buildMessageHeader _config MessageHeader{..} = Element
  { elName = QName "MessageHeader" Nothing Nothing
  , elAttribs = []
  , elContent =
      [ textNode "MessageThreadId" mhMessageThreadId
      , textNode "MessageId" (Just mhMessageId)
      , Elem $ Element
          { elName = QName "MessageSender" Nothing Nothing
          , elAttribs = []
          , elContent = [Elem $ buildPartyIdElement mhSenderPartyId]
          , elLine = Nothing
          }
      , Elem $ Element
          { elName = QName "MessageRecipient" Nothing Nothing
          , elAttribs = []
          , elContent = [Elem $ buildPartyIdElement mhRecipientPartyId]
          , elLine = Nothing
          }
      , textNode "MessageCreatedDateTime" (Just $ formatDateTime mhMessageCreatedDateTime)
      ]
  , elLine = Nothing
  }

-- | Build PartyId element
buildPartyIdElement :: PartyId -> Element
buildPartyIdElement (PartyIdDPID dpid) = Element
  { elName = QName "PartyId" Nothing Nothing
  , elAttribs = []
  , elContent = [textNode "DPID" (Just dpid)]
  , elLine = Nothing
  }
buildPartyIdElement (PartyIdIPI ipi) = Element
  { elName = QName "PartyId" Nothing Nothing
  , elAttribs = []
  , elContent = [textNode "IPI" (Just ipi)]
  , elLine = Nothing
  }
buildPartyIdElement (PartyIdISNI isni) = Element
  { elName = QName "PartyId" Nothing Nothing
  , elAttribs = []
  , elContent = [textNode "ISNI" (Just isni)]
  , elLine = Nothing
  }
buildPartyIdElement (PartyIdProprietary _ns val) = Element
  { elName = QName "PartyId" Nothing Nothing
  , elAttribs = []
  , elContent = [textNode "ProprietaryId" (Just val)]
  , elLine = Nothing
  }

-- | Build PartyList element
buildPartyList :: [Party] -> Element
buildPartyList parties = Element
  { elName = QName "PartyList" Nothing Nothing
  , elAttribs = []
  , elContent = map (Elem . buildParty) parties
  , elLine = Nothing
  }

-- | Build single Party element
buildParty :: Party -> Element
buildParty Party{..} = Element
  { elName = QName "Party" Nothing Nothing
  , elAttribs = []
  , elContent =
      [ Elem $ buildPartyIdElement partyPartyId
      ] ++
      maybe [] (\pn -> [Elem $ buildPartyName pn]) partyPartyName ++
      maybe [] (\ipi -> [textNode "IPI" (Just ipi)]) partyIpiNumber ++
      maybe [] (\isni -> [textNode "ISNI" (Just isni)]) partyIsniNumber
  , elLine = Nothing
  }

-- | Build PartyName element
buildPartyName :: PartyName -> Element
buildPartyName PartyName{..} = Element
  { elName = QName "PartyName" Nothing Nothing
  , elAttribs = []
  , elContent =
      textNode "FullName" (Just pnFullName) :
      map (\n -> textNode "NameToDisplay" (Just n)) pnNamesToDisplay
  , elLine = Nothing
  }

-- | Build ResourceList element
buildResourceList :: [Resource] -> Element
buildResourceList resources = Element
  { elName = QName "ResourceList" Nothing Nothing
  , elAttribs = []
  , elContent = map (Elem . buildResource) resources
  , elLine = Nothing
  }

-- | Build single Resource element
buildResource :: Resource -> Element
buildResource Resource{..} = Element
  { elName = QName (resourceTypeToElementName resourceType) Nothing Nothing
  , elAttribs = []
  , elContent =
      [ textNode "ResourceReference" (Just $ unResourceRef resourceReference)
      , Elem $ Element
          { elName = QName "ReferenceTitle" Nothing Nothing
          , elAttribs = []
          , elContent =
              textNode "TitleText" (Just resourceTitle) :
              maybe [] (\st -> [textNode "SubTitle" (Just st)]) resourceSubTitle
          , elLine = Nothing
          }
      , Elem $ buildResourceIds resourceIds
      ] ++
      maybe [] (\d -> [textNode "Duration" (Just $ formatDuration d)]) resourceDuration ++
      maybe [] (\(Language l) -> [textNode "Language" (Just l)]) resourceLanguage ++
      [textNode "IsExplicit" (Just $ if resourceExplicitContent then "true" else "false")]
  , elLine = Nothing
  }

-- | Build resource IDs element
buildResourceIds :: [ResourceId] -> Element
buildResourceIds ids = Element
  { elName = QName "SoundRecordingId" Nothing Nothing
  , elAttribs = []
  , elContent = concatMap buildResourceId ids
  , elLine = Nothing
  }

-- | Build single resource ID
buildResourceId :: ResourceId -> [Content]
buildResourceId (ResourceIdISRC isrc) = [textNode "ISRC" (Just $ formatISRC isrc)]
buildResourceId (ResourceIdGRid (GRid g)) = [textNode "GRid" (Just g)]

-- | Format ISRC as full string
formatISRC :: ISRC -> Text
formatISRC ISRC{..} = isrcCountryCode <> isrcRegistrant <> isrcYear <> isrcDesignation

-- | Build ReleaseList element
buildReleaseList :: [Release] -> Element
buildReleaseList releases = Element
  { elName = QName "ReleaseList" Nothing Nothing
  , elAttribs = []
  , elContent = map (Elem . buildRelease) releases
  , elLine = Nothing
  }

-- | Build single Release element
buildRelease :: Release -> Element
buildRelease Release{..} = Element
  { elName = QName "Release" Nothing Nothing
  , elAttribs = []
  , elContent =
      [ Elem $ buildReleaseIds releaseIds
      , textNode "ReleaseReference" (Just $ unReleaseRef releaseReference)
      , Elem $ Element
          { elName = QName "ReferenceTitle" Nothing Nothing
          , elAttribs = []
          , elContent =
              textNode "TitleText" (Just releaseTitle) :
              maybe [] (\st -> [textNode "SubTitle" (Just st)]) releaseSubTitle
          , elLine = Nothing
          }
      , textNode "ReleaseType" (Just $ releaseTypeToText releaseType)
      ] ++
      maybe [] (\d -> [textNode "ReleaseDate" (Just $ formatDate d)]) releaseDate
  , elLine = Nothing
  }

-- | Build release IDs element
buildReleaseIds :: [ReleaseId] -> Element
buildReleaseIds ids = Element
  { elName = QName "ReleaseId" Nothing Nothing
  , elAttribs = []
  , elContent = concatMap buildReleaseId ids
  , elLine = Nothing
  }

-- | Build single release ID
buildReleaseId :: ReleaseId -> [Content]
buildReleaseId (ReleaseIdUPC upc) = [textNode "UPC" (Just upc)]
buildReleaseId (ReleaseIdGRid (GRid g)) = [textNode "GRid" (Just g)]
buildReleaseId (ReleaseIdCatalogNumber label cat) =
  [textNode "CatalogNumber" (Just $ label <> ":" <> cat)]

-- | Build ResourceGroup element
buildResourceGroup :: [ResourceGroup] -> Element
buildResourceGroup groups = Element
  { elName = QName "ResourceGroup" Nothing Nothing
  , elAttribs = []
  , elContent = concatMap buildResourceGroupNodes groups
  , elLine = Nothing
  }

-- | Build ResourceGroup nodes
buildResourceGroupNodes :: ResourceGroup -> [Content]
buildResourceGroupNodes ResourceGroup{..} =
  maybe [] (\n -> [textNode "SequenceNumber" (Just $ T.pack $ show n)]) rgSequenceNumber ++
  maybe [] (\t -> [textNode "Title" (Just t)]) rgTitle ++
  map (Elem . buildResourceGroupContentItem) rgContent

-- | Build ResourceGroupContentItem
buildResourceGroupContentItem :: ResourceGroupContent -> Element
buildResourceGroupContentItem (RGCResource ref) = Element
  { elName = QName "ResourceGroupContentItem" Nothing Nothing
  , elAttribs = []
  , elContent =
      [ textNode "ResourceType" (Just "SoundRecording")
      , textNode "ResourceReference" (Just $ unResourceRef ref)
      ]
  , elLine = Nothing
  }
buildResourceGroupContentItem (RGCSequence seqNum ref) = Element
  { elName = QName "ResourceGroupContentItem" Nothing Nothing
  , elAttribs = []
  , elContent =
      [ textNode "SequenceNumber" (Just $ T.pack $ show seqNum)
      , textNode "ResourceType" (Just "SoundRecording")
      , textNode "ResourceReference" (Just $ unResourceRef ref)
      ]
  , elLine = Nothing
  }
buildResourceGroupContentItem (RGCDisc discNum subGroups) = Element
  { elName = QName "ResourceGroup" Nothing Nothing
  , elAttribs = []
  , elContent =
      maybe [] (\n -> [textNode "SequenceNumber" (Just $ T.pack $ show n)]) discNum ++
      concatMap buildResourceGroupNodes subGroups
  , elLine = Nothing
  }

-- | Build DealList element
buildDealList :: [Deal] -> Element
buildDealList deals = Element
  { elName = QName "DealList" Nothing Nothing
  , elAttribs = []
  , elContent = map (Elem . buildDeal) deals
  , elLine = Nothing
  }

-- | Build single Deal element
buildDeal :: Deal -> Element
buildDeal Deal{..} = Element
  { elName = QName "Deal" Nothing Nothing
  , elAttribs = []
  , elContent =
      Elem (buildDealTerms dealDealTerms) :
      map (\ref -> textNode "ReleaseReference" (Just $ unReleaseRef ref)) dealReleaseRefs
  , elLine = Nothing
  }

-- | Build DealTerms element
buildDealTerms :: DealTerms -> Element
buildDealTerms DealTerms{..} = Element
  { elName = QName "DealTerms" Nothing Nothing
  , elAttribs = []
  , elContent =
      [ textNode "CommercialModelType" (Just dtUsageType)
      , Elem $ Element
          { elName = QName "Usage" Nothing Nothing
          , elAttribs = []
          , elContent = [textNode "UseType" (Just dtUsageType)]
          , elLine = Nothing
          }
      ] ++
      map (\tc -> textNode "TerritoryCode" (Just $ territoryToText tc)) dtTerritoryCodes ++
      [ textNode "ValidityStartDate" (Just $ formatDate dtStartDate)
      ] ++
      maybe [] (\d -> [textNode "ValidityEndDate" (Just $ formatDate d)]) dtEndDate
  , elLine = Nothing
  }

-- | Helper to create text node
textNode :: Text -> Maybe Text -> Content
textNode _ Nothing = Text $ CData CDataText "" Nothing
textNode name (Just value) = Elem $ Element
  { elName = QName (T.unpack name) Nothing Nothing
  , elAttribs = []
  , elContent = [Text $ CData CDataText (T.unpack value) Nothing]
  , elLine = Nothing
  }

-- | Convert resource type to element name
resourceTypeToElementName :: ResourceType -> String
resourceTypeToElementName ResourceTypeSoundRecording = "SoundRecording"
resourceTypeToElementName ResourceTypeMusicVideo = "MusicVideo"
resourceTypeToElementName ResourceTypeImage = "Image"
resourceTypeToElementName ResourceTypeText = "Text"
resourceTypeToElementName ResourceTypeSoftware = "Software"

-- | Convert release type to text
releaseTypeToText :: ReleaseType -> Text
releaseTypeToText ReleaseTypeAlbum = "Album"
releaseTypeToText ReleaseTypeSingle = "Single"
releaseTypeToText ReleaseTypeEP = "EP"
releaseTypeToText ReleaseTypeCompilation = "Compilation"
releaseTypeToText ReleaseTypeLiveAlbum = "LiveAlbum"
releaseTypeToText ReleaseTypeRemixAlbum = "RemixAlbum"
releaseTypeToText ReleaseTypeSoundtrack = "Soundtrack"
releaseTypeToText ReleaseTypeSpokenWord = "SpokenWord"

-- | Convert territory to text
territoryToText :: TerritoryCode -> Text
territoryToText TerritoryWorldwide = "Worldwide"
territoryToText (TerritoryCode c) = c

-- | Format duration as ISO 8601
formatDuration :: Duration -> Text
formatDuration Duration{..} =
  "PT" <> T.pack (show durationHours) <> "H" <>
        T.pack (show durationMinutes) <> "M" <>
        T.pack (show durationSeconds) <> "S"

-- | Format date
formatDate :: Date -> Text
formatDate (DateYear y) = T.pack $ show y
formatDate (DateYearMonth y m) = T.pack (show y) <> "-" <> padMonth m
formatDate (DateFull d) = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" d

-- | Format datetime
formatDateTime :: UTCTime -> Text
formatDateTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Pad month with leading zero
padMonth :: Int -> Text
padMonth n
  | n < 10 = "0" <> T.pack (show n)
  | otherwise = T.pack (show n)

-- | Unwrap ResourceReference
unResourceRef :: ResourceReference -> Text
unResourceRef (ResourceReference r) = r

-- | Unwrap ReleaseReference
unReleaseRef :: ReleaseReference -> Text
unReleaseRef (ReleaseReference r) = r
