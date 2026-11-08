export const STUDIO_GALLERY_SLUG = 'studio-gallery';
export const STUDIO_GALLERY_LIMIT = 200;

export interface StudioPhoto {
  id: string;
  title: string;
  alt: string;
  imageUrl: string;
  thumbnailUrl?: string;
  credit?: string;
  capturedAt?: string;
  sourceUrl?: string;
  visible: boolean;
}

export interface StudioGalleryPayload {
  schemaVersion: 1;
  photos: StudioPhoto[];
}

const record = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null && !Array.isArray(value);

export function isStudioImageUrl(value: string): boolean {
  if (/^\/assets\/tdf-studio\/[a-z0-9-]+-(640|1280)\.webp$/.test(value)) return true;
  try {
    const url = new URL(value);
    return url.protocol === 'https:' && !url.username && !url.password;
  } catch { return false; }
}

// Incomplete fields stay editable in drafts; publication consumers use strict validation.
export function parseStudioGallery(value: unknown, allowIncomplete = false): StudioGalleryPayload {
  if (!record(value) || value['schemaVersion'] !== 1 || !Array.isArray(value['photos'])) {
    throw new Error('La galería necesita schemaVersion: 1 y una lista de fotos.');
  }
  if (value['photos'].length > STUDIO_GALLERY_LIMIT) throw new Error(`Máximo ${STUDIO_GALLERY_LIMIT} fotos por galería.`);
  const ids = new Set<string>();
  const photos = value['photos'].map((photo: unknown, index: number): StudioPhoto => {
    const fail = (message: string): never => { throw new Error(`Foto ${index + 1}: ${message}`); };
    if (!record(photo)) return fail('formato inválido.');
    for (const field of ['id', 'title', 'alt', 'imageUrl'] as const) {
      if (typeof photo[field] !== 'string' || (!allowIncomplete && !photo[field].trim())) fail(`completa ${field}.`);
    }
    const item = photo as unknown as StudioPhoto;
    if (!/^[a-zA-Z0-9-]{1,100}$/.test(item.id) || ids.has(item.id)) fail('identificador inválido o repetido.');
    ids.add(item.id);
    if (item.title.length > 200 || item.alt.length > 1000) fail('título o descripción demasiado largo.');
    if (typeof item.visible !== 'boolean') fail('elige la visibilidad.');
    for (const field of ['thumbnailUrl', 'credit', 'capturedAt', 'sourceUrl'] as const) {
      if (item[field] !== undefined && (typeof item[field] !== 'string' || item[field].length > 2000)) fail(`revisa ${field}.`);
    }
    if (!allowIncomplete && (!isStudioImageUrl(item.imageUrl) || (item.thumbnailUrl && !isStudioImageUrl(item.thumbnailUrl)))) fail('usa una URL de imagen HTTPS pública.');
    if (!allowIncomplete && item.sourceUrl && (!isStudioImageUrl(item.sourceUrl) || !item.sourceUrl.startsWith('https://'))) fail('la fuente debe usar HTTPS.');
    if (!allowIncomplete && item.capturedAt) {
      if (!/^\d{4}-(0[1-9]|1[0-2])(-\d{2})?$/.test(item.capturedAt)) fail('usa una fecha AAAA-MM o AAAA-MM-DD.');
      const date = item.capturedAt.length === 7 ? `${item.capturedAt}-01` : item.capturedAt;
      const parsed = new Date(`${date}T00:00:00Z`);
      if (!Number.isFinite(parsed.getTime()) || parsed.toISOString().slice(0, 10) !== date) fail('fecha inexistente.');
    }
    return item;
  });
  return { schemaVersion: 1, photos };
}

export function studioPhotoCaption(photo: StudioPhoto): string {
  const date = photo.capturedAt ? new Intl.DateTimeFormat('es-EC', {
    year: 'numeric', month: 'short', ...(photo.capturedAt.length > 7 ? { day: 'numeric' as const } : {}), timeZone: 'UTC',
  }).format(new Date(`${photo.capturedAt.length === 7 ? `${photo.capturedAt}-01` : photo.capturedAt}T00:00:00Z`)) : '';
  return [date, photo.credit].filter(Boolean).join(' · ');
}
