import { recordThumbnailCandidates } from './resolveRecordThumbnail';

const resource = { providerCode: 'youtube', externalCode: 'f2BabxM1Pjc', availability: 'available' as const };
test('verified thumbnail variants belong to the resource, with editorial priority', () => {
  const candidates = recordThumbnailCandidates({ ...resource, thumbnailUrl: 'https://example.org/editorial.jpg', providerMetadata: {
    id: resource.externalCode, thumbnails: [
      { url: 'https://i.ytimg.com/vi/f2BabxM1Pjc/maxresdefault.jpg' },
      { url: 'https://i.ytimg.com/vi/WRONGVIDEO1/maxresdefault.jpg' },
      { url: 'http://i.ytimg.com/vi/f2BabxM1Pjc/maxresdefault.jpg' },
    ],
  } });
  expect(candidates).toEqual(['https://example.org/editorial.jpg', 'https://i.ytimg.com/vi/f2BabxM1Pjc/maxresdefault.jpg', 'https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg', 'https://i.ytimg.com/vi/f2BabxM1Pjc/mqdefault.jpg']);
});
test('does not construct YouTube thumbnails for another provider', () => {
  expect(recordThumbnailCandidates({ ...resource, providerCode: 'vimeo', providerMetadata: { id: resource.externalCode, thumbnails: [{ url: 'https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg' }] } })).toEqual([]);
});
test('verified unavailability suppresses stale provider thumbnails', () => {
  expect(recordThumbnailCandidates({ ...resource, availability: 'unavailable', thumbnailUrl: 'https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg' })).toEqual([]);
});
