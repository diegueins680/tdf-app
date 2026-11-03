import type { ArtistResearchSource } from '../api/artistEnrichment';
import { artistLinkIssues } from './artistEnrichmentReview';

const source = (overrides: Partial<ArtistResearchSource> = {}): ArtistResearchSource => ({
  arsId: 1,
  arsArtistId: 115,
  arsSourceUrl: 'https://images.example.test/artist.jpg',
  arsSourceType: 'link_validation_valid',
  arsSupportedFields: 'heroImageUrl',
  arsRetrievedAt: '2026-10-26T10:00:00Z',
  ...overrides,
});
const legacyRights = source({
  arsId: 2,
  arsSourceType: 'link_validation_broken',
  arsAttribution: JSON.stringify({ reason: 'third_party_hotlink_requires_authorized_ingestion', valid: false }),
});

describe('artist link availability and image rights', () => {
  it('keeps a working external photo out of broken-link results while retaining its rights review', () => {
    expect(artistLinkIssues(115, ['heroImageUrl'], [legacyRights, source()])).toEqual({
      brokenFields: [], rightsReviewFields: ['heroImageUrl'],
    });
  });

  it('preserves a real network failure even when a newer rights observation exists', () => {
    expect(artistLinkIssues(115, ['heroImageUrl'], [
      source({ arsSourceType: 'link_validation_broken', arsAttribution: '{"httpStatus":404}' }),
      source({ arsId: 2, arsSourceType: 'media_rights_review' }),
    ])).toEqual({ brokenFields: ['heroImageUrl'], rightsReviewFields: ['heroImageUrl'] });
  });

  it('does not attach an old image warning to a newer replacement URL', () => {
    expect(artistLinkIssues(115, [], [legacyRights, source({
      arsId: 3, arsSourceUrl: 'https://api.tdfrecords.net/uploads/replacement.webp',
    })])).toEqual({ brokenFields: [], rightsReviewFields: [] });
  });

  it('does not attach an old URL failure to an unchecked replacement awaiting rights review', () => {
    expect(artistLinkIssues(115, ['heroImageUrl'], [
      source({ arsSourceType: 'link_validation_broken' }),
      source({ arsId: 3, arsSourceType: 'media_rights_review', arsSourceUrl: 'https://images.example.test/new.jpg' }),
    ])).toEqual({ brokenFields: [], rightsReviewFields: ['heroImageUrl'] });
  });

  it('retains broken-link evidence with malformed attribution and isolates other artists', () => {
    expect(artistLinkIssues(115, ['heroImageUrl'], [
      source({ arsSourceType: 'link_validation_broken', arsAttribution: '{invalid' }),
      source({ arsId: 9, arsArtistId: 116 }),
    ])).toEqual({ brokenFields: ['heroImageUrl'], rightsReviewFields: [] });
  });

  it('uses the latest observation independent of input order, with a stable ID tie-break', () => {
    const broken = source({ arsId: 3, arsSourceType: 'link_validation_broken' });
    const repaired = source({ arsId: 4 });
    expect(artistLinkIssues(115, ['heroImageUrl'], [repaired, broken, legacyRights])).toEqual({
      brokenFields: [], rightsReviewFields: ['heroImageUrl'],
    });
  });

  it('preserves server-reported failures when detailed evidence is unavailable', () => {
    expect(artistLinkIssues(115, ['websiteUrl'], [])).toEqual({
      brokenFields: ['websiteUrl'], rightsReviewFields: [],
    });
  });
});
