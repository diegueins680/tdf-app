import type { ArtistResearchSource } from '../api/artistEnrichment';

const newerFirst = (left: ArtistResearchSource, right: ArtistResearchSource) =>
  (Date.parse(right.arsRetrievedAt) || 0) - (Date.parse(left.arsRetrievedAt) || 0)
  || right.arsId - left.arsId;

const isImageRightsReview = (source: ArtistResearchSource): boolean => {
  if (source.arsSourceType === 'media_rights_review') return true;
  if (source.arsSourceType !== 'link_validation_broken') return false;
  try {
    return (JSON.parse(source.arsAttribution ?? '{}') as { reason?: string })?.reason
      === 'third_party_hotlink_requires_authorized_ingestion';
  } catch {
    return false;
  }
};

// Availability and permission to ingest an external image are separate facts.
// Recognize the legacy policy records without hiding a real HTTP failure.
export function artistLinkIssues(
  artistId: number,
  reportedBrokenFields: readonly string[],
  sources: readonly ArtistResearchSource[],
) {
  const network = new Map<string, ArtistResearchSource>();
  const rights = new Map<string, ArtistResearchSource>();
  for (const source of sources.filter(item => item.arsArtistId === artistId).sort(newerFirst)) {
    const rightsReview = isImageRightsReview(source);
    if (!rightsReview && source.arsSourceType !== 'link_validation_valid'
      && source.arsSourceType !== 'link_validation_broken') continue;
    const target = rightsReview ? rights : network;
    for (const field of source.arsSupportedFields.split(',').map(value => value.trim()).filter(Boolean)) {
      if (!target.has(field)) target.set(field, source);
    }
  }
  const broken = new Set(reportedBrokenFields);
  for (const [field, observation] of network) {
    if (observation.arsSourceType === 'link_validation_broken') broken.add(field);
    else broken.delete(field);
  }
  const rightsReviewFields: string[] = [];
  for (const [field, review] of rights) {
    const observation = network.get(field);
    const differentUrl = observation && observation.arsSourceUrl !== review.arsSourceUrl;
    if (differentUrl && newerFirst(observation, review) < 0) continue;
    rightsReviewFields.push(field);
    if (!observation || differentUrl) broken.delete(field);
  }
  return { brokenFields: [...broken], rightsReviewFields };
}
