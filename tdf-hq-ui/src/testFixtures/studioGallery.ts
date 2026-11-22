import { readFileSync } from 'node:fs';
import { parseStudioGallery } from '../utils/studioGallery';
// Exercise the exact initial revision shipped by the append-only migration.
const migration = readFileSync(new URL('../../../tdf-hq/sql/2026-11-07_studio_gallery.sql', import.meta.url), 'utf8');
export const studioGallerySeed = parseStudioGallery(JSON.parse(migration.split('$studio_gallery_seed$')[1]!));
