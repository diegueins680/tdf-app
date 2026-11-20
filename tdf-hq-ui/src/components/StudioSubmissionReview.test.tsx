import { addReviewedPhotos } from './StudioSubmissionReview';
import type { Submission } from '../api/studioSubmissions';
import type { StudioGalleryPayload } from '../utils/studioGallery';

const row = (batch: number, position: number): Submission => ({
  id: `uuid-${batch}-${position}`,
  batch,
  position,
  name: `photo-${batch}-${position}.jpg`,
  state: 'pending',
  credit: 'TDF',
});
it('inserts late approvals within the original batch and preserves existing photos and retries', () => {
  const original: StudioGalleryPayload = {
    schemaVersion: 1,
    photos: [
      {
        id: 'archive',
        title: 'Archive',
        alt: 'Studio',
        imageUrl: 'https://example.com/a.jpg',
        visible: true,
      },
    ],
  };
  const first = addReviewedPhotos(original, [row(1, 2), row(2, 1)]);
  const second = addReviewedPhotos(first, [row(1, 0), row(2, 0), row(1, 2)]);
  expect(second.photos.map((photo) => photo.title)).toEqual([
    'photo-2-0',
    'photo-2-1',
    'photo-1-0',
    'photo-1-2',
    'Archive',
  ]);
  expect(original.photos).toHaveLength(1);
});
