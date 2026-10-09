{-# LANGUAGE OverloadedStrings #-}

-- | A deliberately small, version-pinned ERN adapter for TDF's canonical
-- audio-release snapshot.  ERN is an export representation; none of these
-- records are persistence models.
module TDF.MusicRelease.DDEX.ERN432
  ( Ern432AudioRelease(..)
  , Ern432Counterparty(..)
  , Ern432IdentifierVerification(..)
  , Ern432ReleaseId(..)
  , Ern432ReleaseKind(..)
  , Ern432MessagePurpose(..)
  , Ern432Track(..)
  , Ern432Cover(..)
  , Ern432ExportError(..)
  , Ern432Compatibility(..)
  , Ern432Party(..)
  , Ern432Credit(..)
  , Ern432Credits(..)
  , parseErn432Credits
  , validateErn432Credits
  , ern432Compatibility
  , validateErn432AudioRelease
  , renderErn432AudioRelease
  , ern432ReleaseIdText
  , ern432AudioResourcePath
  , ern432CoverResourcePath
  ) where

import Control.Monad (unless)
import Data.Aeson (Value, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.List (nub, sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Text.XML.Light
  ( Attr(..)
  , CData(..)
  , CDataKind(..)
  , Content(..)
  , Element(..)
  , QName(..)
  , showElement
  )
import TDF.MusicRelease.Domain
  ( IdentifierType(..)
  , identifierErrorMessage
  , validateIdentifier
  )

data Ern432Compatibility = Ern432Compatibility
  { ernStandardVersion :: Text
  , ernReleaseProfile :: Text
  , ernReleaseProfileVersion :: Text
  , ernBusinessProfileVersion :: Maybe Text
  , ernAllowedValueSetVersion :: Text
  , ernStructuralDictionaryVersion :: Text
  , ernCloudStorageChoreographyVersion :: Text
  } deriving (Eq, Show)

ern432Compatibility :: Ern432Compatibility
ern432Compatibility = Ern432Compatibility
  { ernStandardVersion = "4.3.2"
  , ernReleaseProfile = "Audio"
  , ernReleaseProfileVersion = "2.3.1"
  , ernBusinessProfileVersion = Nothing
  , ernAllowedValueSetVersion = "011"
  , ernStructuralDictionaryVersion = "DD-ERN-432"
  , ernCloudStorageChoreographyVersion = "1.8.1"
  }

data Ern432IdentifierVerification
  = SyntaxOnly
  | AuthorityVerified
      { verificationAuthority :: Text
      , verificationEvidence :: Text
      , verifiedAt :: UTCTime
      }
  deriving (Eq, Show)

data Ern432Counterparty = Ern432Counterparty
  { counterpartyDpid :: Text
  , counterpartyName :: Text
  , counterpartyVerification :: Ern432IdentifierVerification
  } deriving (Eq, Show)

data Ern432ReleaseKind = ErnSingle | ErnEP | ErnAlbum
  deriving (Eq, Show)

-- ERN 4 has no UpdateIndicator. An update is another complete statement of
-- truth; a takedown is a subsequent NewReleaseMessage without a DealList.
data Ern432MessagePurpose = ErnNewRelease | ErnUpdate | ErnTakedown
  deriving (Eq, Show)

data Ern432ReleaseId
  = ErnIcpn Text
  | ErnGRid Text
  deriving (Eq, Show)

data Ern432Track = Ern432Track
  { trackRecordingId :: Text
  , trackResourceReference :: Text
  , trackReleaseReference :: Text
  , trackTitle :: Text
  , trackDisplayArtist :: Text
  , trackIsrc :: Text
  , trackDurationSeconds :: Int
  , trackPLineYear :: Int
  , trackPLineText :: Text
  , trackAudioFileUri :: Text
  , trackExplicit :: Bool
  } deriving (Eq, Show)

data Ern432Cover = Ern432Cover
  { coverResourceReference :: Text
  , coverProprietaryId :: Text
  , coverFileUri :: Text
  , coverExplicit :: Bool
  } deriving (Eq, Show)

data Ern432AudioRelease = Ern432AudioRelease
  { messagePurpose :: Ern432MessagePurpose
  , messageThreadId :: Text
  , messageId :: Text
  , messageCreatedAt :: UTCTime
  , messageLanguageAndScript :: Text
  , sender :: Ern432Counterparty
  , recipient :: Ern432Counterparty
  , catalogCredits :: Ern432Credits
  , labelPartyReference :: Text
  , labelName :: Text
  , releaseReference :: Text
  , releaseKind :: Ern432ReleaseKind
  , releaseIdentifier :: Ern432ReleaseId
  , releaseTitle :: Text
  , releaseDisplayArtist :: Text
  , releaseGenre :: Text
  , releaseDurationSeconds :: Int
  , releaseTerritories :: [Text]
  , dealStartsAt :: UTCTime
  , dealEndsAt :: Maybe UTCTime
  , tracks :: [Ern432Track]
  , cover :: Ern432Cover
  } deriving (Eq, Show)

data Ern432ExportError = Ern432ExportError
  { exportErrorField :: Text
  , exportErrorCode :: Text
  , exportErrorMessage :: Text
  } deriving (Eq, Show)

-- Local references derive from internal identities, never from names or official IDs.
data Ern432Party = Ern432Party
  { partyId :: Text
  , partyDisplayName :: Text
  , partyIdentifiers :: [(Text, Text)]
  } deriving (Eq, Show)

data Ern432Credit = Ern432Credit
  { creditPartyId :: Text
  , creditRecordingId :: Maybe Text
  , creditRole :: Text
  , creditOrder :: Int
  } deriving (Eq, Show)

data Ern432Credits = Ern432Credits
  { creditedParties :: [Ern432Party]
  , canonicalCredits :: [Ern432Credit]
  , creditedRecordingIds :: [Text]
  } deriving (Eq, Show)

-- Reads only the approved snapshot. Legal names, account links and verification
-- evidence are deliberately not exported as artist names or XML extensions.
parseErn432Credits :: Value -> Either [Ern432ExportError] Ern432Credits
parseErn432Credits snapshot = case parseEither parser snapshot of
  Left message -> Left [problem "snapshot" "invalid_credit_snapshot" (T.pack message)]
  Right result -> case validateErn432Credits result of
    [] -> Right result
    errors -> Left errors
  where
    parser = withObject "approved snapshot" $ \o -> do
      version <- o .: "schemaVersion" :: Parser Int
      unless (version == 2) (fail "Se requiere una corrección aprobada con snapshot v2.")
      parties <- o .: "parties" >>= mapM parseParty
      credits <- o .: "credits" >>= mapM parseCredit
      recordingIds <- o .: "recordings" >>= mapM (withObject "recording" (.: "id"))
      pure (Ern432Credits parties (sortOn creditSortKey credits) recordingIds)
    parseParty = withObject "party" $ \o -> do
      source <- o .: "detailsSource" :: Parser Text
      unless (source == "user_provided") (fail "Revisa y aprueba los datos legados de colaboradores.")
      Ern432Party <$> o .: "id" <*> o .: "displayName"
        <*> (o .: "identifiers" >>= mapM parseIdentifier)
    parseIdentifier = withObject "party identifier" $ \o -> do
      status <- o .: "verification_status" :: Parser Text
      unless (status `elem` ["syntax_valid","authority_verified","unvalidated"]) $
        fail "Un identificador marcado inválido no puede exportarse; corrige su evidencia en una nueva versión."
      (,) <$> o .: "identifier_type" <*> o .: "identifier_value"
    parseCredit = withObject "credit" $ \o -> Ern432Credit
      <$> o .: "music_party_id" <*> o .:? "recording_id"
      <*> o .: "credit_role" <*> o .: "display_order"

creditSortKey :: Ern432Credit -> (Int, Text, Text, Maybe Text)
creditSortKey c = (creditOrder c, creditPartyId c, creditRole c, creditRecordingId c)

validateErn432Credits :: Ern432Credits -> [Ern432ExportError]
validateErn432Credits graph = concat
  [ duplicateErrors "parties.id" (map partyId parties)
  , concatMap partyErrors (zip [0 :: Int ..] parties)
  , concatMap creditErrors (zip [0 :: Int ..] credits)
  , displayErrors "credits.release" (applicableCredits graph Nothing)
  , concat [displayErrors ("credits.recording." <> rid) (applicableCredits graph (Just rid))
           | rid <- creditedRecordingIds graph]
  ]
  where
    parties = creditedParties graph
    credits = canonicalCredits graph
    partyErrors (index,p) =
      let field = "parties[" <> T.pack (show index) <> "]"
      in anchorErrors (field <> ".id") 'P' (partyAnchor (partyId p))
         ++ required (field <> ".displayName") (partyDisplayName p)
         ++ concat [partyIdentifierErrors (field <> ".identifiers[" <> T.pack (show n) <> "]") identifier
                   | (n,identifier) <- zip [0 :: Int ..] (partyIdentifiers p), partyId p `elem` map creditPartyId credits]
    creditErrors (index,c) =
      let field = "credits[" <> T.pack (show index) <> "]"
      in [problem (field <> ".music_party_id") "unknown_party" "El colaborador no está en la versión aprobada."
          | creditPartyId c `notElem` map partyId parties]
         ++ [problem (field <> ".recording_id") "unknown_recording" "La grabación no está en la versión aprobada."
            | Just rid <- [creditRecordingId c], rid `notElem` creditedRecordingIds graph]
         ++ [problem (field <> ".credit_role") "unsupported_credit_role" "Este rol necesita un mapeo ERN explícito; corrige su clasificación sin eliminar el crédito."
            | creditRole c `notElem` ["main_artist","featured_artist","display_artist","label"]
                && contributorRole (creditRole c) == Nothing]
         ++ [problem (field <> ".display_order") "invalid_sequence" "El orden no puede ser negativo." | creditOrder c < 0]
    displayErrors field cs =
      [problem field "main_artist_missing" "Declara un artista principal en este ámbito; el nombre de display no acredita a nadie."
      | not (any ((== "main_artist") . creditRole) cs)]
      ++ [problem field "ambiguous_display_role" "Un mismo artista no puede tener roles de display contradictorios en el mismo ámbito."
         | pid <- nub (map creditPartyId cs)
         , length (nub [role | c <- cs, creditPartyId c == pid, Just role <- [displayRole (creditRole c)]]) > 1]

partyIdentifierErrors :: Text -> (Text, Text) -> [Ern432ExportError]
partyIdentifierErrors field (kind,value) = case lookup kind [("isni",ISNI),("ipi",IPI),("dpid",DPID)] of
  Nothing -> [problem field "unsupported_party_identifier" "El adaptador admite ISNI, IPI y DPID proporcionados; otros identificadores requieren un adaptador o namespace explícito, no un código inventado."]
  Just identifierType -> case validateIdentifier identifierType value of
    Left err -> [problem field "invalid_party_identifier" (identifierErrorMessage err)]
    Right _ -> []

partyAnchor :: Text -> Text
partyAnchor = ("P_" <>)

displayRole :: Text -> Maybe Text
displayRole role = lookup role
  [("main_artist","MainArtist"),("featured_artist","FeaturedArtist"),("display_artist","Artist")]

contributorRole :: Text -> Maybe Text
contributorRole role = lookup role
  [("main_artist","Artist"),("featured_artist","Artist"),("composer","Composer")
  ,("lyricist","Lyricist"),("performer","Performer"),("producer","StudioProducer")
  ,("engineer","Engineer"),("mixer","MixingEngineer"),("mastering_engineer","MasteringEngineer")
  ,("publisher","MusicPublisher")]

-- Release-scoped credits apply to every recording, matching canonical submission
-- validation. Recording-scoped credits never leak into a sibling track/release.
applicableCredits :: Ern432Credits -> Maybe Text -> [Ern432Credit]
applicableCredits graph recording = sortOn creditSortKey
  [c | c <- canonicalCredits graph, creditRecordingId c == Nothing || creditRecordingId c == recording]

validateErn432AudioRelease :: Ern432AudioRelease -> [Ern432ExportError]
validateErn432AudioRelease release = concat
  [ required "messageThreadId" (messageThreadId release)
  , required "messageId" (messageId release)
  , languageErrors (messageLanguageAndScript release)
  , counterpartyErrors "sender" (sender release)
  , counterpartyErrors "recipient" (recipient release)
  , validateErn432Credits (catalogCredits release)
  , anchorErrors "labelPartyReference" 'P' (labelPartyReference release)
  , anchorErrors "releaseReference" 'R' (releaseReference release)
  , required "labelName" (labelName release)
  , required "releaseTitle" (releaseTitle release)
  , required "releaseDisplayArtist" (releaseDisplayArtist release)
  , required "releaseGenre" (releaseGenre release)
  , releaseIdErrors (releaseIdentifier release)
  , positive "releaseDurationSeconds" (releaseDurationSeconds release)
  , [problem "availability.endsAt" "deal_period_invalid" "El final del deal debe ser posterior a su inicio efectivo."
    | messagePurpose release /= ErnTakedown, Just end <- [dealEndsAt release], end <= dealStartsAt release]
  , if null (releaseTerritories release)
      then [problem "releaseTerritories" "territories_missing" "DDEX requiere al menos un territorio para el deal."]
      else concatMap territoryErrors (zip [0 :: Int ..] (releaseTerritories release))
  , if null (tracks release)
      then [problem "tracks" "tracks_missing" "El perfil Audio requiere al menos una grabación de sonido."]
      else concatMap trackErrors (zip [0 :: Int ..] (tracks release))
  , coverErrors (cover release)
  , duplicateReferenceErrors release
  , [problem "tracks" "snapshot_recording_mismatch" "Los recursos deben coincidir con las grabaciones del snapshot aprobado."
    | sortOn id (nub (map trackRecordingId (tracks release)))
        /= sortOn id (nub (creditedRecordingIds (catalogCredits release)))]
  ]

renderErn432AudioRelease :: Ern432AudioRelease -> Either [Ern432ExportError] ByteString
renderErn432AudioRelease release =
  case validateErn432AudioRelease release of
    [] -> Right (BL.fromStrict (TE.encodeUtf8 document))
    errors -> Left errors
  where
    compatibility = ern432Compatibility
    rootOpen = T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      , "<ern:NewReleaseMessage xmlns:ern=\"http://ddex.net/xml/ern/432\""
      , " ReleaseProfileVersionId=\"", ernReleaseProfile compatibility, "\""
      , " LanguageAndScriptCode=\"", xmlAttribute (messageLanguageAndScript release), "\""
      , " AvsVersionId=\"11\">"
      ]
    body = map showElement $
      [ messageHeaderElement release
      , partyListElement release
      , resourceListElement release
      , releaseListElement release
      ] ++ case messagePurpose release of
        ErnTakedown -> []
        ErnNewRelease -> [dealListElement release]
        ErnUpdate -> [dealListElement release]
    document = rootOpen <> T.pack (concat body) <> "</ern:NewReleaseMessage>\n"

messageHeaderElement :: Ern432AudioRelease -> Element
messageHeaderElement release = el "MessageHeader" []
  [ textEl "MessageThreadId" [] (messageThreadId release)
  , textEl "MessageId" [] (messageId release)
  , counterpartyElement "MessageSender" (sender release)
  , counterpartyElement "MessageRecipient" (recipient release)
  , textEl "MessageCreatedDateTime" [] (formatUtc (messageCreatedAt release))
  ]

counterpartyElement :: Text -> Ern432Counterparty -> Element
counterpartyElement elementName party = el elementName []
  [ textEl "PartyId" [] (counterpartyDpid party)
  , el "PartyName" [] [textEl "FullName" [] (counterpartyName party)]
  ]

partyListElement :: Ern432AudioRelease -> Element
partyListElement release = el "PartyList" []
  ([partyElement (labelPartyReference release) (labelName release) | null (labelCredits release Nothing)]
    ++ map creditedPartyElement (sortOn partyId usedParties))
  where
    graph = catalogCredits release
    usedParties = [p | p <- creditedParties graph, partyId p `elem` map creditPartyId (canonicalCredits graph)]

creditedPartyElement :: Ern432Party -> Element
creditedPartyElement p = el "Party" []
  ([textEl "PartyReference" [] (partyAnchor (partyId p))
   ,el "PartyName" [] [textEl "FullName" [] (partyDisplayName p)]]
   ++ [el "PartyId" [] [textEl tag [] (normalise kind value)]
      | (kind,value) <- sortOn id (nub (partyIdentifiers p))
      , Just tag <- [lookup kind [("isni","ISNI"),("ipi","IpiNameNumber"),("dpid","DPID")]]])
  where
    normalise kind value = case lookup kind [("isni",ISNI),("ipi",IPI),("dpid",DPID)] of
      Just identifierType -> either (const value) id (validateIdentifier identifierType value)
      Nothing -> value

partyElement :: Text -> Text -> Element
partyElement reference name = el "Party" []
  [ textEl "PartyReference" [] reference
  , el "PartyName" [] [textEl "FullName" [] name]
  ]

resourceListElement :: Ern432AudioRelease -> Element
resourceListElement release = el "ResourceList" []
  (map (trackResourceElement release) (zip [1 :: Int ..] (tracks release))
    ++ [coverResourceElement release (cover release)])

trackResourceElement :: Ern432AudioRelease -> (Int, Ern432Track) -> Element
trackResourceElement release (sequenceNumber, track) = el "SoundRecording" [] $
  [ textEl "ResourceReference" [] (trackResourceReference track)
  , textEl "Type" [] "MusicalWorkSoundRecording"
  , el "SoundRecordingEdition" []
      [ el "ResourceId" [] [textEl "ISRC" [] (trackIsrc track)]
      , el "PLine" []
          [ textEl "Year" [] (T.pack (show (trackPLineYear track)))
          , textEl "PLineText" [] (trackPLineText track)
          ]
      , el "TechnicalDetails" []
          [ textEl "TechnicalResourceDetailsReference" [] ("T" <> T.pack (show sequenceNumber))
          , el "DeliveryFile" []
              [ textEl "Type" [] "AudioFile"
              , el "File" [] [textEl "URI" [] (trackAudioFileUri track)]
              ]
          ]
      ]
  , textEl "DisplayTitleText" [] (trackTitle track)
  , displayTitleElement (trackTitle track)
  , textEl "DisplayArtistName" territoryDefaultAttributes (trackDisplayArtist track)
  ] ++ displayArtistElements cs ++ contributorElements cs ++
  [ textEl "Duration" [] (durationText (trackDurationSeconds track))
  , textEl "ParentalWarningType" [] (explicitText (trackExplicit track))
  ]
  where cs = applicableCredits (catalogCredits release) (Just (trackRecordingId track))

coverResourceElement :: Ern432AudioRelease -> Ern432Cover -> Element
coverResourceElement release artwork = el "Image" []
  [ textEl "ResourceReference" [] (coverResourceReference artwork)
  , textEl "Type" [] "FrontCoverImage"
  , el "ResourceId" []
      [ textEl "ProprietaryId" [attribute "Namespace" (counterpartyDpid (sender release))] (coverProprietaryId artwork) ]
  , textEl "ParentalWarningType" [] (explicitText (coverExplicit artwork))
  , el "TechnicalDetails" []
      [ textEl "TechnicalResourceDetailsReference" [] "TArtwork"
      , el "File" [] [textEl "URI" [] (coverFileUri artwork)]
      ]
  ]

releaseListElement :: Ern432AudioRelease -> Element
releaseListElement release = el "ReleaseList" []
  (mainReleaseElement release : map (trackReleaseElement release) (tracks release))

mainReleaseElement :: Ern432AudioRelease -> Element
mainReleaseElement release = el "Release" [] $
  [ textEl "ReleaseReference" [] (releaseReference release)
  , textEl "ReleaseType" [] (releaseKindText (releaseKind release))
  , el "ReleaseId" [] [releaseIdElement (releaseIdentifier release)]
  , textEl "DisplayTitleText" [] (releaseTitle release)
  , displayTitleElement (releaseTitle release)
  , textEl "DisplayArtistName" territoryDefaultAttributes (releaseDisplayArtist release)
  ] ++ displayArtistElements (applicableCredits (catalogCredits release) Nothing)
    ++ labelElements release Nothing ++
  [ textEl "Duration" [] (durationText (releaseDurationSeconds release))
  , genreElement (releaseGenre release)
  , textEl "ParentalWarningType" [] (overallExplicitText (tracks release))
  , el "ResourceGroup" []
      (textEl "SequenceNumber" [] "1"
        : map resourceGroupItem (zip [1 :: Int ..] (tracks release))
        ++ [textEl "LinkedReleaseResourceReference" [] (coverResourceReference (cover release))])
  ]

trackReleaseElement :: Ern432AudioRelease -> Ern432Track -> Element
trackReleaseElement release track = el "TrackRelease" [] $
  [ textEl "ReleaseReference" [] (trackReleaseReference track)
  , el "ReleaseId" []
      [ textEl "ProprietaryId" [attribute "Namespace" (counterpartyDpid (sender release))]
          ("TDF-TRACK-" <> ern432ReleaseIdText (releaseIdentifier release) <> "-"
            <> T.toUpper (T.filter (`notElem` ['-', ' ']) (trackIsrc track)))
      ]
  , textEl "ReleaseResourceReference" [] (trackResourceReference track)
  ] ++ labelElements release (Just (trackRecordingId track)) ++ [genreElement (releaseGenre release)]

dealListElement :: Ern432AudioRelease -> Element
dealListElement release = el "DealList" []
  [ el "ReleaseDeal" []
      (map (textEl "DealReleaseReference" [])
        (releaseReference release : map trackReleaseReference (tracks release))
        ++ [ el "Deal" []
              [ el "DealTerms" []
                  (map (textEl "TerritoryCode" []) (releaseTerritories release)
                    ++ [ el "ValidityPeriod" [] (textEl "StartDateTime" [] (formatUtc (dealStartsAt release))
                          : [textEl "EndDateTime" [] (formatUtc end) | Just end <- [dealEndsAt release]])
                       -- This adapter represents TDF's free on-demand policy,
                       -- not arbitrary recipient subscription/radio rights.
                       , textEl "CommercialModelType" [] "FreeOfChargeModel"
                       , textEl "UseType" [] "OnDemandStream"
                       ])
              ]
           ])
  ]

releaseIdElement :: Ern432ReleaseId -> Element
releaseIdElement releaseId = case releaseId of
  ErnIcpn _ -> textEl "ICPN" [] (ern432ReleaseIdText releaseId)
  ErnGRid _ -> textEl "GRid" [] (ern432ReleaseIdText releaseId)

-- Cloud Storage 1.8.1 clause 5.3: optional label/hierarchy omitted.
-- Validation still rejects invalid/provided IDs; normalization issues no code.
ern432ReleaseIdText :: Ern432ReleaseId -> Text
ern432ReleaseIdText identifier = T.toUpper (T.filter (`notElem` ['-', ' ']) value)
  where value = case identifier of ErnIcpn x -> x; ErnGRid x -> x

ern432AudioResourcePath :: Ern432ReleaseId -> Int -> Text
ern432AudioResourcePath identifier sequenceNumber = "resources/"
  <> ern432ReleaseIdText identifier <> "_T" <> T.pack (show sequenceNumber)
  <> "_SoundRecording.m4a"

ern432CoverResourcePath :: Ern432ReleaseId -> Text
ern432CoverResourcePath identifier = "resources/" <> ern432ReleaseIdText identifier
  <> "_TArtwork_CoverArt.jpg"

resourceGroupItem :: (Int, Ern432Track) -> Element
resourceGroupItem (sequenceNumber, track) = el "ResourceGroupContentItem" []
  [ textEl "SequenceNumber" [] (T.pack (show sequenceNumber))
  , textEl "ReleaseResourceReference" [] (trackResourceReference track)
  ]

displayTitleElement :: Text -> Element
displayTitleElement title = el "DisplayTitle" territoryDefaultAttributes
  [textEl "TitleText" [] title]

displayArtistElements :: [Ern432Credit] -> [Element]
displayArtistElements cs =
  [el "DisplayArtist" [attribute "SequenceNumber" (T.pack (show n))]
    [textEl "ArtistPartyReference" [] (partyAnchor pid), textEl "DisplayArtistRole" [] role]
  | (n,(pid,role)) <- zip [1 :: Int ..] (nub
      [(creditPartyId c,role) | c <- cs, Just role <- [displayRole (creditRole c)]])]

contributorElements :: [Ern432Credit] -> [Element]
contributorElements cs =
  [el "Contributor" [attribute "SequenceNumber" (T.pack (show n))]
    (textEl "ContributorPartyReference" [] (partyAnchor pid)
      : [el "Role" [] [textEl "Value" [] role] | role <- roles pid])
  | (n,pid) <- zip [1 :: Int ..] (nub [creditPartyId c | c <- cs, contributorRole (creditRole c) /= Nothing])]
  where roles pid = nub (mapMaybe (contributorRole . creditRole) [c | c <- cs, creditPartyId c == pid])

labelCredits :: Ern432AudioRelease -> Maybe Text -> [Text]
labelCredits release scope = nub [partyAnchor (creditPartyId c)
  | c <- applicableCredits (catalogCredits release) scope, creditRole c == "label"]

labelElements :: Ern432AudioRelease -> Maybe Text -> [Element]
labelElements release scope = map (textEl "ReleaseLabelReference" [attribute "ApplicableTerritoryCode" "Worldwide"])
  (case labelCredits release scope of [] -> [labelPartyReference release]; refs -> refs)

genreElement :: Text -> Element
genreElement genre = el "DisplayGenre" [attribute "ApplicableTerritoryCode" "Worldwide"]
  [textEl "GenreText" [] genre]

territoryDefaultAttributes :: [Attr]
territoryDefaultAttributes =
  [ attribute "ApplicableTerritoryCode" "Worldwide"
  , attribute "IsDefault" "true"
  ]

el :: Text -> [Attr] -> [Element] -> Element
el name attributes children = Element
  { elName = QName (T.unpack name) Nothing Nothing
  , elAttribs = attributes
  , elContent = map Elem children
  , elLine = Nothing
  }

textEl :: Text -> [Attr] -> Text -> Element
textEl name attributes value = Element
  { elName = QName (T.unpack name) Nothing Nothing
  , elAttribs = attributes
  , elContent = [Text (CData CDataText (T.unpack value) Nothing)]
  , elLine = Nothing
  }

attribute :: Text -> Text -> Attr
attribute name value = Attr (QName (T.unpack name) Nothing Nothing) (T.unpack value)

xmlAttribute :: Text -> Text
xmlAttribute = T.concatMap escape
  where
    escape '&' = "&amp;"
    escape '<' = "&lt;"
    escape '"' = "&quot;"
    escape '\'' = "&apos;"
    escape character = T.singleton character

formatUtc :: UTCTime -> Text
formatUtc = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

durationText :: Int -> Text
durationText totalSeconds =
  let (hours, afterHours) = totalSeconds `divMod` 3600
      (minutes, seconds) = afterHours `divMod` 60
      hoursPart = if hours > 0 then T.pack (show hours) <> "H" else ""
      minutesPart = if minutes > 0 then T.pack (show minutes) <> "M" else ""
  in "PT" <> hoursPart <> minutesPart <> T.pack (show seconds) <> "S"

explicitText :: Bool -> Text
explicitText True = "Explicit"
explicitText False = "NotExplicit"

overallExplicitText :: [Ern432Track] -> Text
overallExplicitText = explicitText . any trackExplicit

releaseKindText :: Ern432ReleaseKind -> Text
releaseKindText ErnSingle = "Single"
releaseKindText ErnEP = "EP"
releaseKindText ErnAlbum = "Album"

required :: Text -> Text -> [Ern432ExportError]
required field value
  | T.null (T.strip value) = [problem field "required" "Este campo es obligatorio para la exportación ERN."]
  | otherwise = []

positive :: Text -> Int -> [Ern432ExportError]
positive field value
  | value <= 0 = [problem field "invalid_duration" "La duración debe ser mayor que cero."]
  | otherwise = []

languageErrors :: Text -> [Ern432ExportError]
languageErrors language
  | T.length language >= 2 && T.all validLanguageCharacter language = []
  | otherwise = [problem "messageLanguageAndScript" "invalid_language" "Usa una etiqueta de idioma y escritura compatible con IETF BCP 47."]
  where
    validLanguageCharacter char = isAlphaNum char || char == '-'

counterpartyErrors :: Text -> Ern432Counterparty -> [Ern432ExportError]
counterpartyErrors field party =
  required (field <> ".name") (counterpartyName party)
    ++ case validateIdentifier DPID (counterpartyDpid party) of
         Left identifierProblem ->
           [problem (field <> ".dpid") "invalid_dpid" (identifierErrorMessage identifierProblem)]
         Right _ -> []
    ++ case counterpartyVerification party of
         SyntaxOnly ->
           [problem (field <> ".dpid") "dpid_not_authority_verified" "Verifica el DPID y conserva evidencia de la autoridad antes de exportar."]
         AuthorityVerified authority evidence _
           | T.null (T.strip authority) || T.null (T.strip evidence) ->
               [problem (field <> ".dpid") "dpid_verification_evidence_missing" "La verificación del DPID requiere autoridad y evidencia auditables."]
           | otherwise -> []

releaseIdErrors :: Ern432ReleaseId -> [Ern432ExportError]
releaseIdErrors releaseId = case releaseId of
  ErnIcpn value ->
    let identifierType = if T.length (T.filter (/= ' ') value) == 12 then UPC else EAN
    in case validateIdentifier identifierType value of
         Left _ -> [problem "releaseIdentifier" "invalid_icpn" "El ICPN debe ser un UPC de 12 o EAN de 13 dígitos con checksum válido."]
         Right _ -> []
  ErnGRid value ->
    case validateIdentifier GRid value of
      Left _ -> [problem "releaseIdentifier" "invalid_grid" "El GRid debe ser proporcionado y sintácticamente válido."]
      Right _ -> []

trackErrors :: (Int, Ern432Track) -> [Ern432ExportError]
trackErrors (index, track) = concat
  [ anchorErrors (prefix <> ".resourceReference") 'A' (trackResourceReference track)
  , anchorErrors (prefix <> ".releaseReference") 'R' (trackReleaseReference track)
  , required (prefix <> ".title") (trackTitle track)
  , required (prefix <> ".displayArtist") (trackDisplayArtist track)
  , required (prefix <> ".pLineText") (trackPLineText track)
  , safeResourceUriErrors (prefix <> ".audioFileUri") (trackAudioFileUri track)
  , positive (prefix <> ".durationSeconds") (trackDurationSeconds track)
  , if trackPLineYear track < 1000 || trackPLineYear track > 9999
      then [problem (prefix <> ".pLineYear") "invalid_year" "El año de la línea P debe tener cuatro dígitos."]
      else []
  , case validateIdentifier ISRC (trackIsrc track) of
      Left _ -> [problem (prefix <> ".isrc") "invalid_isrc" "Proporciona un ISRC sintácticamente válido; TDF no lo emite."]
      Right _ -> []
  ]
  where
    prefix = "tracks[" <> T.pack (show index) <> "]"

coverErrors :: Ern432Cover -> [Ern432ExportError]
coverErrors artwork = concat
  [ anchorErrors "cover.resourceReference" 'A' (coverResourceReference artwork)
  , required "cover.proprietaryId" (coverProprietaryId artwork)
  , safeResourceUriErrors "cover.fileUri" (coverFileUri artwork)
  ]

anchorErrors :: Text -> Char -> Text -> [Ern432ExportError]
anchorErrors field expectedPrefix value
  | T.length value >= 2
      && T.head value == expectedPrefix
      && T.all (\char -> isAlphaNum char || char `elem` ['_', '-']) (T.tail value) = []
  | otherwise = [problem field "invalid_local_anchor" ("La referencia local debe empezar por " <> T.singleton expectedPrefix <> " y contener solo letras, números, _ o -.")]

territoryErrors :: (Int, Text) -> [Ern432ExportError]
territoryErrors (index, territory)
  | territory == "Worldwide" = []
  | T.length territory == 2 && T.all (\char -> char >= 'A' && char <= 'Z') territory = []
  | otherwise = [problem ("releaseTerritories[" <> T.pack (show index) <> "]") "invalid_territory" "Usa Worldwide o un código ISO 3166-1 alpha-2 en mayúsculas."]

safeResourceUriErrors :: Text -> Text -> [Ern432ExportError]
safeResourceUriErrors field uri
  | T.null uri = [problem field "required" "La ruta del recurso es obligatoria."]
  | T.isPrefixOf "/" uri || ".." `elem` T.splitOn "/" uri || T.any (== '\\') uri =
      [problem field "unsafe_resource_uri" "La ruta del paquete debe ser relativa, normalizada y no puede escapar de su directorio."]
  | otherwise = []

duplicateReferenceErrors :: Ern432AudioRelease -> [Ern432ExportError]
duplicateReferenceErrors release = concat
  [ duplicates "tracks.resourceReference" (map trackResourceReference (tracks release) ++ [coverResourceReference (cover release)])
  , duplicates "tracks.releaseReference" (releaseReference release : map trackReleaseReference (tracks release))
  , duplicates "parties.reference" (labelPartyReference release : map (partyAnchor . partyId) (creditedParties (catalogCredits release)))
  ]
  where
    duplicates field values
      | length values == length (nub values) = []
      | otherwise = [problem field "duplicate_local_anchor" "Las referencias locales DDEX deben ser únicas dentro del mensaje."]

problem :: Text -> Text -> Text -> Ern432ExportError
problem = Ern432ExportError

duplicateErrors :: Text -> [Text] -> [Ern432ExportError]
duplicateErrors field values =
  [problem field "duplicate_local_anchor" "Las identidades deben ser únicas." | length values /= length (nub values)]
