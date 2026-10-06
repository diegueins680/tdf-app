{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.DDEX.ERN432Spec
  ( spec
  , validErn432AudioRelease
  , multiPartyRelease
  , creditSnapshot
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.List (find, isInfixOf)
import Data.Time (UTCTime)
import qualified Data.Text as T
import Test.Hspec
import TDF.MusicRelease.DDEX.ERN432

spec :: Spec
spec = describe "TDF.MusicRelease.DDEX.ERN432" $ do
  it "names files using the supplied release ID and matching technical references" $ do
    ern432AudioResourcePath (ErnIcpn "012345678905") 12
      `shouldBe` "resources/012345678905_T12_SoundRecording.m4a"
    ern432CoverResourcePath (ErnIcpn "012345678905")
      `shouldBe` "resources/012345678905_TArtwork_CoverArt.jpg"
    ern432ReleaseIdText (ErnGRid "A1-2425G-ABC1234002-M") `shouldBe` "A12425GABC1234002M"

  it "uses the same normalized provided identifier in the XML as in file names" $ do
    renderErn432AudioRelease validErn432AudioRelease
      { releaseIdentifier = ErnGRid "A1-2425G-ABC1234002-M" }
      `shouldSatisfy` either (const False) (contains "<GRid>A12425GABC1234002M</GRid>")

  it "pins a compatible ERN 4.3.2 Audio matrix without an ERN 3 Business Profile" $ do
    ernStandardVersion ern432Compatibility `shouldBe` "4.3.2"
    ernReleaseProfile ern432Compatibility `shouldBe` "Audio"
    ernReleaseProfileVersion ern432Compatibility `shouldBe` "2.3.1"
    ernBusinessProfileVersion ern432Compatibility `shouldBe` Nothing
    ernAllowedValueSetVersion ern432Compatibility `shouldBe` "011"
    ernStructuralDictionaryVersion ern432Compatibility `shouldBe` "DD-ERN-432"

  it "renders the namespace-qualified root and unqualified ERN children" $ do
    let rendered = renderErn432AudioRelease validErn432AudioRelease
    rendered `shouldSatisfy` either (const False) (contains "<ern:NewReleaseMessage xmlns:ern=\"http://ddex.net/xml/ern/432\"")
    rendered `shouldSatisfy` either (const False) (contains "ReleaseProfileVersionId=\"Audio\"")
    rendered `shouldSatisfy` either (const False) (contains "AvsVersionId=\"11\"")
    rendered `shouldSatisfy` either (const False) (not . contains "BusinessProfile")
    rendered `shouldSatisfy` either (const False) (contains "<MessageHeader>")

  it "renders updates as complete ERN 4 statements and takedowns without deals" $ do
    let updateXml = renderErn432AudioRelease validErn432AudioRelease { messagePurpose=ErnUpdate }
        takedownXml = renderErn432AudioRelease validErn432AudioRelease { messagePurpose=ErnTakedown }
    updateXml `shouldSatisfy` either (const False) (contains "<DealList>")
    updateXml `shouldSatisfy` either (const False) (not . contains "UpdateIndicator")
    takedownXml `shouldSatisfy` either (const False) (not . contains "<DealList>")

  it "keeps track-release identifiers stable across initial, update and takedown messages" $ do
    let initial = validErn432AudioRelease
        expected = ["TDF-TRACK-" <> T.unpack (ern432ReleaseIdText (releaseIdentifier initial))
          <> "-" <> T.unpack (trackIsrc track) | track <- tracks initial]
    mapM_ (\operation -> do
      let rendered = renderErn432AudioRelease initial
            {messageId="TDF-new-message",messagePurpose=operation}
      expected `shouldSatisfy` (not . null)
      mapM_ (\identifier -> rendered `shouldSatisfy` either (const False) (contains identifier)) expected
      rendered `shouldSatisfy` either (const False) (not . contains "TDF-new-message-R1"))
      [ErnNewRelease,ErnUpdate,ErnTakedown]

  it "blocks export when a DPID has only syntax validation" $ do
    let release = validErn432AudioRelease
          { sender = (sender validErn432AudioRelease)
              { counterpartyVerification = SyntaxOnly }
          }
        errors = validateErn432AudioRelease release
    exportErrorCode <$> find ((== "sender.dpid") . exportErrorField) errors
      `shouldBe` Just "dpid_not_authority_verified"
    renderErn432AudioRelease release `shouldSatisfy` isLeft

  it "exports only the free on-demand policy and retains its end date" $ do
    let rendered = renderErn432AudioRelease validErn432AudioRelease
          { dealEndsAt = Just (utc "2027-01-01 00:00:00 UTC") }
    rendered `shouldSatisfy` either (const False) (contains "<CommercialModelType>FreeOfChargeModel</CommercialModelType>")
    rendered `shouldSatisfy` either (const False) (contains "<UseType>OnDemandStream</UseType>")
    rendered `shouldSatisfy` either (const False) (contains "<EndDateTime>2027-01-01T00:00:00Z</EndDateTime>")
    rendered `shouldSatisfy` either (const False) (not . contains "SubscriptionModel")

  it "rejects empty/reversed deal periods but permits an expired deal's takedown" $ do
    let release = validErn432AudioRelease { dealEndsAt = Just (dealStartsAt validErn432AudioRelease) }
    renderErn432AudioRelease release `shouldSatisfy` isLeft
    renderErn432AudioRelease release { messagePurpose = ErnTakedown }
      `shouldSatisfy` either (const False) (not . contains "DealList")

  it "reports field-level errors for missing and invalid export identifiers" $ do
    case tracks validErn432AudioRelease of
      [] -> expectationFailure "The valid ERN fixture must contain at least one track"
      firstTrack : remainingTracks -> do
        let release = validErn432AudioRelease
              { releaseIdentifier = ErnIcpn "123"
              , tracks = firstTrack { trackIsrc = "NOT-AN-ISRC" } : remainingTracks
              }
            errorCodes = map exportErrorCode (validateErn432AudioRelease release)
        errorCodes `shouldContain` ["invalid_icpn", "invalid_isrc"]

  it "rejects duplicate anchors and package path traversal" $ do
    case tracks validErn432AudioRelease of
      [] -> expectationFailure "The valid ERN fixture must contain at least one track"
      firstTrack : remainingTracks -> do
        let release = validErn432AudioRelease
              { cover = (cover validErn432AudioRelease)
                  { coverResourceReference = trackResourceReference firstTrack
                  , coverFileUri = "../private/master.wav"
                  }
              , tracks = firstTrack : remainingTracks
              }
            errorCodes = map exportErrorCode (validateErn432AudioRelease release)
        errorCodes `shouldContain` ["duplicate_local_anchor"]
        errorCodes `shouldContain` ["unsafe_resource_uri"]

  it "reads versioned party names and credits without exporting legal names or account links" $ do
    parseErn432Credits creditSnapshot `shouldBe` Right (catalogCredits multiPartyRelease)
    let rendered = renderErn432AudioRelease multiPartyRelease
    rendered `shouldSatisfy` either (const False) (contains "Guest &amp; Friend")
    rendered `shouldSatisfy` either (const False) (not . contains "PRIVATE LEGAL NAME")
    rendered `shouldSatisfy` either (const False) (not . contains "Uncredited contact")
    rendered `shouldSatisfy` either (const False) (contains "<IpiNameNumber>00000000001</IpiNameNumber>")

  it "groups multiple roles once per contributor and sequences display artists" $ do
    let rendered = renderErn432AudioRelease multiPartyRelease
    rendered `shouldSatisfy` either (const False) (contains
      "<Contributor SequenceNumber=\"1\"><ContributorPartyReference>P_artist</ContributorPartyReference><Role><Value>Artist</Value></Role><Role><Value>Composer</Value></Role><Role><Value>Lyricist</Value></Role></Contributor>")
    rendered `shouldSatisfy` either (const False) (contains
      "<DisplayArtist SequenceNumber=\"2\"><ArtistPartyReference>P_guest</ArtistPartyReference><DisplayArtistRole>FeaturedArtist</DisplayArtistRole></DisplayArtist>")

  it "does not leak a recording-scoped featured artist into its sibling or release display" $ do
    let Right xml = renderErn432AudioRelease multiPartyRelease
    count "<DisplayArtistRole>FeaturedArtist</DisplayArtistRole>" xml `shouldBe` 1
    count "<ContributorPartyReference>P_guest</ContributorPartyReference>" xml `shouldBe` 1
    count "<ContributorPartyReference>P_artist</ContributorPartyReference>" xml `shouldBe` 2

  it "preserves all mapped technical and musical contributor roles" $ do
    let graph = catalogCredits multiPartyRelease
        roles = ["performer","producer","engineer","mixer","mastering_engineer","publisher"]
        updated = multiPartyRelease {catalogCredits = graph
          {canonicalCredits = canonicalCredits graph ++ [Ern432Credit "guest" (Just "recording-1") r 2 | r <- roles]}}
    let rendered = renderErn432AudioRelease updated
    mapM_ (\role -> rendered `shouldSatisfy` either (const False) (contains ("<Value>" ++ role ++ "</Value>")))
      ["Performer","StudioProducer","Engineer","MixingEngineer","MasteringEngineer","MusicPublisher"]

  it "uses explicit label identities without inventing an artist credit for them" $ do
    let graph = catalogCredits multiPartyRelease
        updated = multiPartyRelease {catalogCredits = graph
          {canonicalCredits = canonicalCredits graph ++ [Ern432Credit "unused" Nothing "label" 3]}}
        rendered = renderErn432AudioRelease updated
    rendered `shouldSatisfy` either (const False) (contains ">P_unused</ReleaseLabelReference>")
    rendered `shouldSatisfy` either (const False) (not . contains ">P_unused</ContributorPartyReference>")
    rendered `shouldSatisfy` either (const False) (not . contains "<PartyReference>PLabel</PartyReference>")

  it "fails closed for legacy snapshots, missing parties and malformed identifiers" $ do
    parseErn432Credits (replace "schemaVersion" (1 :: Int) creditSnapshot) `shouldSatisfy` isLeft
    parseErn432Credits (replace "parties" ([] :: [Value]) creditSnapshot) `shouldSatisfy` isLeft
    let graph = catalogCredits multiPartyRelease
    mapM_ (\identifier -> validateErn432Credits graph
      {creditedParties = [Ern432Party "artist" "Artist" [identifier], Ern432Party "guest" "Guest" []]}
        `shouldSatisfy` (not . null)) [("isni","INVALID"),("ipi","123"),("proprietary","unknown-namespace")]

  it "rejects dangling references, unsupported roles and ambiguous display roles" $ do
    let graph = catalogCredits multiPartyRelease
    mapM_ (\credit -> validateErn432Credits graph
      {canonicalCredits = canonicalCredits graph ++ [credit]} `shouldSatisfy` (not . null))
      [Ern432Credit "missing" Nothing "composer" 0
      ,Ern432Credit "artist" (Just "not-in-snapshot") "composer" 0
      ,Ern432Credit "artist" Nothing "other" 0
      ,Ern432Credit "artist" Nothing "featured_artist" 0
      ,Ern432Credit "artist" Nothing "composer" (-1)]

  it "does not rehabilitate an identifier explicitly marked invalid just because its syntax passes" $ do
    let invalid = object ["id" .= ("artist" :: String), "displayName" .= ("Artist" :: String)
          ,"detailsSource" .= ("user_provided" :: String)
          ,"identifiers" .= [object ["identifier_type" .= ("ipi" :: String)
            ,"identifier_value" .= ("00000000001" :: String), "verification_status" .= ("invalid" :: String)]]]
    parseErn432Credits (replace "parties" [invalid] creditSnapshot) `shouldSatisfy` either
      (any (isInfixOf "marcado inv" . show . exportErrorMessage)) (const False)

  it "does not manufacture a main artist from display text or from another recording" $ do
    let graph = catalogCredits multiPartyRelease
    renderErn432AudioRelease multiPartyRelease {catalogCredits = graph {canonicalCredits =
      [Ern432Credit "artist" (Just "recording-1") "main_artist" 0]}} `shouldSatisfy` isLeft

  it "rejects resource sets that differ from the approved snapshot" $ do
    renderErn432AudioRelease multiPartyRelease {tracks = take 1 (tracks multiPartyRelease)} `shouldSatisfy` isLeft

  it "keeps identical credit semantics for updates and takedowns" $ do
    let Right original = renderErn432AudioRelease multiPartyRelease
    mapM_ (\purpose -> case renderErn432AudioRelease multiPartyRelease {messagePurpose = purpose} of
      Left errors -> expectationFailure (show errors)
      Right xml -> count "<Contributor " xml `shouldBe` count "<Contributor " original)
      [ErnUpdate,ErnTakedown]

validErn432AudioRelease :: Ern432AudioRelease
validErn432AudioRelease = Ern432AudioRelease
  { messagePurpose = ErnNewRelease
  , messageThreadId = "TDF-SYNTHETIC-THREAD-1"
  , messageId = "TDF-SYNTHETIC-MESSAGE-1"
  , messageCreatedAt = utc "2026-09-12 12:00:00 UTC"
  , messageLanguageAndScript = "en"
  , sender = Ern432Counterparty
      { counterpartyDpid = "PADPIDA0000000001A"
      , counterpartyName = "TDF Synthetic Sender"
      , counterpartyVerification = AuthorityVerified
          { verificationAuthority = "synthetic-test-authority"
          , verificationEvidence = "fixture-only"
          , verifiedAt = utc "2026-09-12 11:00:00 UTC"
          }
      }
  , recipient = Ern432Counterparty
      { counterpartyDpid = "PADPIDA0000000002B"
      , counterpartyName = "TDF Synthetic Recipient"
      , counterpartyVerification = AuthorityVerified
          { verificationAuthority = "synthetic-test-authority"
          , verificationEvidence = "fixture-only"
          , verifiedAt = utc "2026-09-12 11:00:00 UTC"
          }
      }
  , catalogCredits = Ern432Credits
      [Ern432Party "artist" "Synthetic Artist" []]
      [Ern432Credit "artist" Nothing "main_artist" 0]
      ["recording-1"]
  , labelPartyReference = "PLabel"
  , labelName = "Synthetic Label"
  , releaseReference = "R0"
  , releaseKind = ErnSingle
  , releaseIdentifier = ErnIcpn "012345678905"
  , releaseTitle = "Synthetic Single"
  , releaseDisplayArtist = "Synthetic Artist"
  , releaseGenre = "Pop"
  , releaseDurationSeconds = 180
  , releaseTerritories = ["Worldwide"]
  , dealStartsAt = utc "2026-09-12 12:00:00 UTC"
  , dealEndsAt = Nothing
  , tracks =
      [ Ern432Track
          { trackRecordingId = "recording-1"
          , trackResourceReference = "A1"
          , trackReleaseReference = "R1"
          , trackTitle = "Synthetic Track"
          , trackDisplayArtist = "Synthetic Artist"
          , trackIsrc = "USTDF2600001"
          , trackDurationSeconds = 180
          , trackPLineYear = 2026
          , trackPLineText = "2026 Synthetic Label"
          , trackAudioFileUri = "resources/01-synthetic-track.wav"
          , trackExplicit = False
          }
      ]
  , cover = Ern432Cover
      { coverResourceReference = "A2"
      , coverProprietaryId = "TDF-SYNTHETIC-COVER-1"
      , coverFileUri = "resources/cover.jpg"
      , coverExplicit = False
      }
  }

utc :: String -> UTCTime
utc = read

contains :: String -> BL8.ByteString -> Bool
contains needle = isInfixOf needle . BL8.unpack

count :: String -> BL8.ByteString -> Int
count needle = length . filter (isInfixOf needle) . windows . BL8.unpack
  where windows [] = []; windows xs = take (length needle) xs : windows (drop 1 xs)

replace :: Data.Aeson.ToJSON a => Data.Aeson.Key -> a -> Value -> Value
replace key value (Data.Aeson.Object o) = Data.Aeson.Object (KM.insert key (Data.Aeson.toJSON value) o)
replace _ _ value = value

multiPartyRelease :: Ern432AudioRelease
multiPartyRelease = validErn432AudioRelease
  { releaseKind = ErnAlbum
  , catalogCredits = Ern432Credits
      [Ern432Party "artist" "Synthetic Artist" []
      ,Ern432Party "guest" "Guest & Friend" [("ipi","00000000001")]
      ,Ern432Party "unused" "Uncredited contact" []]
      [Ern432Credit "artist" Nothing "main_artist" 0
      ,Ern432Credit "artist" Nothing "composer" 1
      ,Ern432Credit "artist" Nothing "lyricist" 2
      ,Ern432Credit "guest" (Just "recording-1") "featured_artist" 3]
      ["recording-1","recording-2"]
  , tracks = case tracks validErn432AudioRelease of
      [t] -> [t {trackDisplayArtist = "Synthetic Artist feat. Guest & Friend"}, t
        {trackRecordingId = "recording-2", trackResourceReference = "A3", trackReleaseReference = "R2",trackIsrc = "USTDF2600002"}]
      _ -> []
  }

creditSnapshot :: Value
creditSnapshot = object
  [ "schemaVersion" .= (2 :: Int)
  , "parties" .= [object
      ["id" .= partyId p,"displayName" .= partyDisplayName p,"legalName" .= ("PRIVATE LEGAL NAME" :: String)
      ,"tdfPartyId" .= (123 :: Int),"detailsSource" .= ("user_provided" :: String)
      ,"identifiers" .= [object ["identifier_type" .= k,"identifier_value" .= v,
        "verification_status" .= ("syntax_valid" :: String)] | (k,v) <- partyIdentifiers p]]
      | p <- creditedParties (catalogCredits multiPartyRelease)]
  , "credits" .= [object
      ["music_party_id" .= creditPartyId c,"recording_id" .= creditRecordingId c
      ,"credit_role" .= creditRole c,"display_order" .= creditOrder c]
      | c <- canonicalCredits (catalogCredits multiPartyRelease)]
  , "recordings" .= [object ["id" .= rid] | rid <- creditedRecordingIds (catalogCredits multiPartyRelease)]
  ]
