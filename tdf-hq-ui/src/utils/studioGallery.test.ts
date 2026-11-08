import { parseStudioGallery } from './studioGallery';
import { studioGallerySeed } from '../testFixtures/studioGallery';

it.each([
  { photos: [studioGallerySeed.photos[0], studioGallerySeed.photos[0]] },
  { photos: [{ ...studioGallerySeed.photos[0], imageUrl: 'javascript:alert(1)' }] },
  { photos: [{ ...studioGallerySeed.photos[0], imageUrl: '//example.com/p.jpg' }] },
  { photos: [{ ...studioGallerySeed.photos[0], sourceUrl: 'http://example.com' }] },
  { photos: [{ ...studioGallerySeed.photos[0], capturedAt: '2025-02-30' }] },
  { photos: [{ ...studioGallerySeed.photos[0], visible: 'false' }] },
  { photos: [{ ...studioGallerySeed.photos[0], title: '' }] },
])('rejects an invalid manifest %#', changes => {
  expect(() => parseStudioGallery({ schemaVersion: 1, ...changes })).toThrow();
});

it('preserves month-only dates and accepts an intentionally empty gallery', () => {
  expect(parseStudioGallery(studioGallerySeed).photos[0]!.capturedAt).toBe('2024-06');
  expect(parseStudioGallery({ schemaVersion: 1, photos: [] }).photos).toEqual([]);
});

it('keeps incomplete draft fields editable without accepting them as public content', () => {
  const draft = { schemaVersion: 1, photos: [{ ...studioGallerySeed.photos[0], title: '', imageUrl: 'https:' }] };
  expect(parseStudioGallery(draft, true).photos).toHaveLength(1);
  expect(() => parseStudioGallery(draft)).toThrow();
});
