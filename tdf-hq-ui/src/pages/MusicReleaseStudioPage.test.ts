import { defaultMusicReleaseContent, musicReleaseDraftFromVersion, musicReleaseSlug } from './MusicReleaseStudioPage';
import type { MusicReleaseVersion } from '../api/musicReleases';

describe('MusicReleaseStudioPage canonical draft', () => {
  it('keeps version-local names and identifiers while reusing a linked identity by partyId only', () => {
    const version = {
      displayArtist: 'Artista', tracks: [{ ...defaultMusicReleaseContent('Artista').tracks[0],
        trackId: 'track-id', recordingId: 'recording-id' }],
      parties: [{ id: 'party-id', tdfPartyId: 42, displayName: 'Nombre de esta versión',
        legalName: 'Nombre legal de esta versión', partyKind: 'person', detailsSource: 'user_provided',
        identifiers: [{ identifier_type: 'proprietary', identifier_value: 'synthetic:version' }] }],
      credits: [], rights: [], availability: [], identifiers: [],
    } as unknown as MusicReleaseVersion;
    expect(musicReleaseDraftFromVersion(version).content.parties[0]).toEqual({
      clientRef: 'party-party-id', partyId: 'party-id', tdfPartyId: null,
      displayName: 'Nombre de esta versión', legalName: 'Nombre legal de esta versión', partyKind: 'person',
      identifiers: [{ type: 'proprietary', value: 'synthetic:version' }],
    });
  });
  it('builds stable lowercase slugs without leaking accents or repeated separators', () => {
    expect(musicReleaseSlug('  Canción Única — Edición 2026  ')).toBe('cancion-unica-edicion-2026');
  });

  it('starts master and composition declarations independently at 10000 basis points', () => {
    const draft = defaultMusicReleaseContent('Artista sintético');
    expect(draft.tracks).toHaveLength(1);
    expect(draft.tracks[0]?.previewStartMs).toBeNull();
    expect(draft.tracks[0]?.previewDurationMs).toBeNull();
    expect(draft.rightsDeclarations.map((rights) => rights.scope)).toEqual(['master', 'composition']);
    expect(draft.rightsDeclarations.map((rights) => rights.splits.reduce(
      (total, split) => total + split.basisPoints,
      0,
    ))).toEqual([10000, 10000]);
    expect(draft.parties[0]?.tdfPartyId).toBeNull();
  });
});
