/** A lineup entry: an artist already on the platform, or just a name to create. */
export interface EventLineupArtistDraft {
  artistId: string | null;
  artistName: string;
}

export const MAX_EVENT_ARTISTS = 50;
export const MAX_ARTIST_NAME_LENGTH = 200;

const artistNameKey = (name: string) => name.trim().replace(/\s+/g, ' ').toLocaleLowerCase();

/** Trims, drops blanks and duplicates (by id, or by name for new artists) and caps the lineup. */
export function normalizeLineup(artists: readonly EventLineupArtistDraft[]): EventLineupArtistDraft[] {
  const seenIds = new Set<string>();
  const seenNames = new Set<string>();
  const lineup: EventLineupArtistDraft[] = [];
  for (const artist of artists) {
    const trimmedId = artist.artistId?.trim();
    // A blank identifier means a new artist, the same as a missing one.
    const artistId = trimmedId === undefined || trimmedId === '' ? null : trimmedId;
    const artistName = artist.artistName.trim().replace(/\s+/g, ' ').slice(0, MAX_ARTIST_NAME_LENGTH);
    if (artistId) {
      if (seenIds.has(artistId)) continue;
      seenIds.add(artistId);
    } else {
      const key = artistNameKey(artistName);
      if (!key || seenNames.has(key)) continue;
      seenNames.add(key);
    }
    lineup.push({ artistId, artistName });
    if (lineup.length >= MAX_EVENT_ARTISTS) break;
  }
  return lineup;
}
