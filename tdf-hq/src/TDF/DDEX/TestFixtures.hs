{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.TestFixtures
  ( -- * Test Fixtures
    validSingleXml
  , validAlbumXml
  , validMultiDiscAlbumXml
  , invalidMissingIsrcXml
  , invalidMalformedXml
  , validRinMessageXml
    -- * Expected Results
  , expectedSingleParseResult
  , expectedAlbumParseResult
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import TDF.DDEX.ERN.V432.Types

-- | Valid single release XML fixture
validSingleXml :: BL.ByteString
validSingleXml = BL8.pack $ T.unpack $ T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<ernNewReleaseMessage xmlns=\"http://ddex.net/xml/ern/432\""
  , "                      MessageSchemaVersionId=\"ern/432\""
  , "                      Language=\"en\">"
  , "  <MessageHeader>"
  , "    <MessageThreadId>THREAD-001</MessageThreadId>"
  , "    <MessageId>MSG-20260805-001</MessageId>"
  , "    <MessageSender>"
  , "      <PartyId>DPID:TDF001</PartyId>"
  , "    </MessageSender>"
  , "    <MessageRecipient>"
  , "      <PartyId>DPID:DSP001</PartyId>"
  , "    </MessageRecipient>"
  , "    <MessageCreatedDateTime>2026-08-05T12:00:00Z</MessageCreatedDateTime>"
  , "  </MessageHeader>"
  , "  <PartyList>"
  , "    <Party>"
  , "      <PartyId><DPID>DPID:ARTIST001</DPID></PartyId>"
  , "      <PartyName><FullName>Test Artist</FullName></PartyName>"
  , "    </Party>"
  , "  </PartyList>"
  , "  <ResourceList>"
  , "    <SoundRecording>"
  , "      <SoundRecordingReference>A1</SoundRecordingReference>"
  , "      <ReferenceTitle><TitleText>Test Song</TitleText></ReferenceTitle>"
  , "      <SoundRecordingId><ISRC>USTDF2600001</ISRC></SoundRecordingId>"
  , "      <Duration>PT3M30S</Duration>"
  , "      <IsExplicit>false</IsExplicit>"
  , "    </SoundRecording>"
  , "  </ResourceList>"
  , "  <ReleaseList>"
  , "    <Release>"
  , "      <ReleaseReference>R1</ReleaseReference>"
  , "      <ReleaseId><UPC>012345678901</UPC></ReleaseId>"
  , "      <ReferenceTitle><TitleText>Test Single</TitleText></ReferenceTitle>"
  , "      <ReleaseType>Single</ReleaseType>"
  , "      <ReleaseDate>2026-08-05</ReleaseDate>"
  , "    </Release>"
  , "  </ReleaseList>"
  , "  <ResourceGroup>"
  , "    <ResourceGroupContentItem>"
  , "      <ResourceReference>A1</ResourceReference>"
  , "    </ResourceGroupContentItem>"
  , "  </ResourceGroup>"
  , "  <DealList>"
  , "    <Deal>"
  , "      <DealTerms>"
  , "        <TerritoryCode>Worldwide</TerritoryCode>"
  , "        <UseType>OnDemandStream</UseType>"
  , "        <ValidityStartDate>2026-08-05</ValidityStartDate>"
  , "      </DealTerms>"
  , "      <ReleaseReference>R1</ReleaseReference>"
  , "    </Deal>"
  , "  </DealList>"
  , "</ernNewReleaseMessage>"
  ]

-- | Valid album release XML fixture
validAlbumXml :: BL.ByteString
validAlbumXml = BL8.pack $ T.unpack $ T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<ernNewReleaseMessage xmlns=\"http://ddex.net/xml/ern/432\""
  , "                      MessageSchemaVersionId=\"ern/432\""
  , "                      Language=\"en\">"
  , "  <MessageHeader>"
  , "    <MessageThreadId>THREAD-002</MessageThreadId>"
  , "    <MessageId>MSG-20260805-002</MessageId>"
  , "    <MessageSender><PartyId>DPID:TDF001</PartyId></MessageSender>"
  , "    <MessageRecipient><PartyId>DPID:DSP001</PartyId></MessageRecipient>"
  , "    <MessageCreatedDateTime>2026-08-05T14:00:00Z</MessageCreatedDateTime>"
  , "  </MessageHeader>"
  , "  <PartyList>"
  , "    <Party>"
  , "      <PartyId><DPID>DPID:ARTIST001</DPID></PartyId>"
  , "      <PartyName><FullName>Test Artist</FullName></PartyName>"
  , "    </Party>"
  , "  </PartyList>"
  , "  <ResourceList>"
  , "    <SoundRecording>"
  , "      <SoundRecordingReference>A1</SoundRecordingReference>"
  , "      <ReferenceTitle><TitleText>Track One</TitleText></ReferenceTitle>"
  , "      <SoundRecordingId><ISRC>USTDF2600001</ISRC></SoundRecordingId>"
  , "      <Duration>PT3M30S</Duration>"
  , "    </SoundRecording>"
  , "    <SoundRecording>"
  , "      <SoundRecordingReference>A2</SoundRecordingReference>"
  , "      <ReferenceTitle><TitleText>Track Two</TitleText></ReferenceTitle>"
  , "      <SoundRecordingId><ISRC>USTDF2600002</ISRC></SoundRecordingId>"
  , "      <Duration>PT4M15S</Duration>"
  , "    </SoundRecording>"
  , "    <SoundRecording>"
  , "      <SoundRecordingReference>A3</SoundRecordingReference>"
  , "      <ReferenceTitle><TitleText>Track Three</TitleText></ReferenceTitle>"
  , "      <SoundRecordingId><ISRC>USTDF2600003</ISRC></SoundRecordingId>"
  , "      <Duration>PT5M45S</Duration>"
  , "    </SoundRecording>"
  , "  </ResourceList>"
  , "  <ReleaseList>"
  , "    <Release>"
  , "      <ReleaseReference>R1</ReleaseReference>"
  , "      <ReleaseId><UPC>012345678902</UPC></ReleaseId>"
  , "      <ReferenceTitle><TitleText>Test Album</TitleText></ReferenceTitle>"
  , "      <ReleaseType>Album</ReleaseType>"
  , "      <ReleaseDate>2026-08-05</ReleaseDate>"
  , "    </Release>"
  , "  </ReleaseList>"
  , "  <ResourceGroup>"
  , "    <ResourceGroupContentItem>"
  , "      <SequenceNumber>1</SequenceNumber>"
  , "      <ResourceReference>A1</ResourceReference>"
  , "    </ResourceGroupContentItem>"
  , "    <ResourceGroupContentItem>"
  , "      <SequenceNumber>2</SequenceNumber>"
  , "      <ResourceReference>A2</ResourceReference>"
  , "    </ResourceGroupContentItem>"
  , "    <ResourceGroupContentItem>"
  , "      <SequenceNumber>3</SequenceNumber>"
  , "      <ResourceReference>A3</ResourceReference>"
  , "    </ResourceGroupContentItem>"
  , "  </ResourceGroup>"
  , "  <DealList>"
  , "    <Deal>"
  , "      <DealTerms>"
  , "        <TerritoryCode>Worldwide</TerritoryCode>"
  , "        <UseType>OnDemandStream</UseType>"
  , "        <ValidityStartDate>2026-08-05</ValidityStartDate>"
  , "      </DealTerms>"
  , "      <ReleaseReference>R1</ReleaseReference>"
  , "    </Deal>"
  , "  </DealList>"
  , "</ernNewReleaseMessage>"
  ]

-- | Valid multi-disc album XML fixture
validMultiDiscAlbumXml :: BL.ByteString
validMultiDiscAlbumXml = BL8.pack $ T.unpack $ T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<ernNewReleaseMessage xmlns=\"http://ddex.net/xml/ern/432\""
  , "                      MessageSchemaVersionId=\"ern/432\">"
  , "  <MessageHeader>"
  , "    <MessageId>MSG-20260805-003</MessageId>"
  , "    <MessageSender><PartyId>DPID:TDF001</PartyId></MessageSender>"
  , "    <MessageRecipient><PartyId>DPID:DSP001</PartyId></MessageRecipient>"
  , "    <MessageCreatedDateTime>2026-08-05T16:00:00Z</MessageCreatedDateTime>"
  , "  </MessageHeader>"
  , "  <PartyList />"
  , "  <ResourceList>"
  , "    <SoundRecording>"
  , "      <SoundRecordingReference>A1</SoundRecordingReference>"
  , "      <ReferenceTitle><TitleText>Disc 1 Track 1</TitleText></ReferenceTitle>"
  , "      <SoundRecordingId><ISRC>USTDF2600010</ISRC></SoundRecordingId>"
  , "    </SoundRecording>"
  , "    <SoundRecording>"
  , "      <SoundRecordingReference>A2</SoundRecordingReference>"
  , "      <ReferenceTitle><TitleText>Disc 2 Track 1</TitleText></ReferenceTitle>"
  , "      <SoundRecordingId><ISRC>USTDF2600020</ISRC></SoundRecordingId>"
  , "    </SoundRecording>"
  , "  </ResourceList>"
  , "  <ReleaseList>"
  , "    <Release>"
  , "      <ReleaseReference>R1</ReleaseReference>"
  , "      <ReleaseId><UPC>012345678903</UPC></ReleaseId>"
  , "      <ReferenceTitle><TitleText>Multi-Disc Album</TitleText></ReferenceTitle>"
  , "      <ReleaseType>Album</ReleaseType>"
  , "    </Release>"
  , "  </ReleaseList>"
  , "  <ResourceGroup>"
  , "    <ResourceGroup>"
  , "      <SequenceNumber>1</SequenceNumber>"
  , "      <Title>Disc 1</Title>"
  , "      <ResourceGroupContentItem>"
  , "        <ResourceReference>A1</ResourceReference>"
  , "      </ResourceGroupContentItem>"
  , "    </ResourceGroup>"
  , "    <ResourceGroup>"
  , "      <SequenceNumber>2</SequenceNumber>"
  , "      <Title>Disc 2</Title>"
  , "      <ResourceGroupContentItem>"
  , "        <ResourceReference>A2</ResourceReference>"
  , "      </ResourceGroupContentItem>"
  , "    </ResourceGroup>"
  , "  </ResourceGroup>"
  , "  <DealList />"
  , "</ernNewReleaseMessage>"
  ]

-- | Invalid XML: Missing required ISRC
invalidMissingIsrcXml :: BL.ByteString
invalidMissingIsrcXml = BL8.pack $ T.unpack $ T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<ernNewReleaseMessage xmlns=\"http://ddex.net/xml/ern/432\">"
  , "  <MessageHeader>"
  , "    <MessageId>MSG-INVALID-001</MessageId>"
  , "    <MessageSender><PartyId>DPID:TDF001</PartyId></MessageSender>"
  , "    <MessageRecipient><PartyId>DPID:DSP001</PartyId></MessageRecipient>"
  , "    <MessageCreatedDateTime>2026-08-05T12:00:00Z</MessageCreatedDateTime>"
  , "  </MessageHeader>"
  , "  <PartyList />"
  , "  <ResourceList>"
  , "    <SoundRecording>"
  , "      <SoundRecordingReference>A1</SoundRecordingReference>"
  , "      <ReferenceTitle><TitleText>Track Without ISRC</TitleText></ReferenceTitle>"
  , "      <!-- Missing ISRC - business rule violation -->"
  , "    </SoundRecording>"
  , "  </ResourceList>"
  , "  <ReleaseList>"
  , "    <Release>"
  , "      <ReleaseReference>R1</ReleaseReference>"
  , "      <ReleaseId><UPC>INVALID</UPC></ReleaseId>"
  , "      <ReferenceTitle><TitleText>Invalid Release</TitleText></ReferenceTitle>"
  , "      <ReleaseType>Single</ReleaseType>"
  , "    </Release>"
  , "  </ReleaseList>"
  , "  <ResourceGroup />"
  , "  <DealList />"
  , "</ernNewReleaseMessage>"
  ]

-- | Invalid XML: Malformed structure
invalidMalformedXml :: BL.ByteString
invalidMalformedXml = BL8.pack "<?xml version=\"1.0\"?><unclosed>"

-- | Valid RIN message XML fixture (placeholder)
validRinMessageXml :: BL.ByteString
validRinMessageXml = BL8.pack $ T.unpack $ T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<rinReleaseMessage xmlns=\"http://ddex.net/xml/rin/21\""
  , "                   MessageSchemaVersionId=\"rin/21\">"
  , "  <!-- RIN message structure placeholder -->"
  , "</rinReleaseMessage>"
  ]

-- | Expected parse result for single
expectedSingleParseResult :: ErnMessage
expectedSingleParseResult = ErnMessage
  { ernMessageHeader = MessageHeader
      { mhMessageId = "MSG-20260805-001"
      , mhMessageThreadId = Just "THREAD-001"
      , mhSenderPartyId = PartyIdDPID "DPID:TDF001"
      , mhRecipientPartyId = PartyIdDPID "DPID:DSP001"
      , mhMessageCreatedDateTime = read "2026-08-05 12:00:00 UTC"
      , mhMessageAuditTrail = Nothing
      }
  , ernPartyList = []
  , ernResourceList = []
  , ernReleaseList = []
  , ernResourceGroups = []
  , ernDealList = []
  }

-- | Expected parse result for album
expectedAlbumParseResult :: ErnMessage
expectedAlbumParseResult = ErnMessage
  { ernMessageHeader = MessageHeader
      { mhMessageId = "MSG-20260805-002"
      , mhMessageThreadId = Just "THREAD-002"
      , mhSenderPartyId = PartyIdDPID "DPID:TDF001"
      , mhRecipientPartyId = PartyIdDPID "DPID:DSP001"
      , mhMessageCreatedDateTime = read "2026-08-05 14:00:00 UTC"
      , mhMessageAuditTrail = Nothing
      }
  , ernPartyList = []
  , ernResourceList = []
  , ernReleaseList = []
  , ernResourceGroups = []
  , ernDealList = []
  }
