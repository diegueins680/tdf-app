import { API_BASE_URL } from '../api/client';
import type { DirectoryEntityType } from '../api/directory';
import { safePublicImageUrl } from './eventSharing';

export type DirectoryPreviewKind = DirectoryEntityType;

// The backend resolves the canonical preview image (cover, featured image,
// linked profile media, avatar/logo, first portfolio image). Clients only
// make that URL absolute and fall back to the kind's placeholder when the
// backend reports no valid media or the image fails to load.
export const DIRECTORY_IMAGE_FALLBACKS: Record<DirectoryPreviewKind, string> = {
  profile: '/artist-fallback.svg',
  classified: '/directory-fallback.svg',
  event: '/event-fallback.svg',
  venue: '/directory-fallback.svg',
};

const currentOrigin = () => (typeof window === 'undefined' ? 'http://localhost' : window.location.origin);

export const directoryFallbackImageUrl = (kind: DirectoryPreviewKind): string =>
  new URL(DIRECTORY_IMAGE_FALLBACKS[kind], currentOrigin()).toString();

export const resolveDirectoryPreviewImage = (value: unknown): string | undefined =>
  safePublicImageUrl(value, API_BASE_URL || currentOrigin());
