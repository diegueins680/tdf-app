import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Stack } from '@mui/material';
import { musicReleases, type ArtistFeaturedTrack, type MusicPublicRelease, type MusicPublicReleaseSummary } from '../api/musicReleases';
import { useOptionalPlayer } from './PlayerProvider';
import { publicMusicTrack } from './publicMusicTrack';
import ArtistFeaturedTrackEditor from './ArtistFeaturedTrackEditor';
import type { ArtistReleaseDTO } from '../api/types';
import { resolveReleasePlaybackAction } from '../features/releases/ReleasePlayerActions';
import type { PlayerTrack } from './types';

const EMPTY_LEGACY_RELEASES: ArtistReleaseDTO[] = [];

/** Public detail responses supply membership; asset access is reauthorized by the engine. */
export function artistTracks(releases: MusicPublicRelease[], artistId: number): PlayerTrack[] {
  const seen = new Set<string>();
  return releases.filter((release) => release.artistPartyId === artistId).flatMap((release) =>
    [...release.tracks].sort((a, b) => a.discNumber - b.discNumber || a.trackNumber - b.trackNumber)
      .filter((track) => track.sources.length > 0 && !seen.has(track.recordingId) && Boolean(seen.add(track.recordingId)))
      .map((track) => publicMusicTrack({ ...track, releaseId: release.id, releaseVersionId: release.versionId })));

}

export function legacyArtistTracks(releases: ArtistReleaseDTO[], artistId: number, artistName: string): PlayerTrack[] {
  return releases.flatMap((release): PlayerTrack[] => {
    if (release.arArtistId !== artistId) return [];
    const action = resolveReleasePlaybackAction(release, {});
    if (action?.kind !== 'native') return [];
    return [{ id: `legacy-release-${release.arReleaseId}`, releaseId: `legacy-release-${release.arReleaseId}`,
      title: release.arTitle, artist: artistName, artworkUrl: release.arCoverImageUrl,
      sources: [{ url: action.url, quality: 'high', mediaType: 'audio/mpeg' }] }];
  });
}

export function prioritizeFeaturedTrack(tracks: PlayerTrack[], artistId: number, featured: ArtistFeaturedTrack | null): PlayerTrack[] {
  if (!featured?.available || featured.artistPartyId !== artistId) return tracks;
  const index = tracks.findIndex((track) => featured.recordingId
    ? track.recordingId === featured.recordingId : track.id === `legacy-release-${featured.legacyReleaseId}`);
  if (index < 1) return tracks;
  return [tracks[index]!, ...tracks.slice(0, index), ...tracks.slice(index + 1)];
}

export default function ArtistProfilePlayback({ profileKey, artistId, artistName, releases, legacyReleases = EMPTY_LEGACY_RELEASES, loading, failed, retryCatalog, canManage = false }: {
  profileKey: string; artistId: number | null; artistName: string; canManage?: boolean;
  releases: MusicPublicReleaseSummary[] | undefined; legacyReleases?: ArtistReleaseDTO[]; loading: boolean; failed: boolean; retryCatalog?: () => void;
}) {
  const player = useOptionalPlayer();
  const beginArtist = player?.beginArtist;
  const loadArtist = player?.loadArtist;
  const loadedKey = useRef<string | null>(null);
  const [state, setState] = useState<'loading' | 'loaded' | 'empty' | 'error'>('loading');
  const [attempt, setAttempt] = useState(0);
  const [catalogTracks, setCatalogTracks] = useState<PlayerTrack[]>([]);
  useEffect(() => {
    beginArtist?.(profileKey);
    loadedKey.current = null;
    setState('loading');
    setCatalogTracks((current) => current.length ? [] : current);
  }, [beginArtist, profileKey]);

  useEffect(() => {
    if (!loadArtist || !artistId || loading || loadedKey.current === profileKey) return;
    const fallbackTracks = legacyArtistTracks(legacyReleases, artistId, artistName);
    if (failed && !fallbackTracks.length) { setState('error'); return; }
    if (!releases && !failed) return;
    const controller = new AbortController();
    const preference = new AbortController();
    const cancelPreference = () => preference.abort();
    controller.signal.addEventListener('abort', cancelPreference, { once: true });
    const preferenceTimer = window.setTimeout(cancelPreference, 2000);
    const featured = Promise.resolve().then(() => musicReleases.getArtistFeaturedTrack(artistId, preference.signal))
      .catch(() => null).finally(() => {
        window.clearTimeout(preferenceTimer); controller.signal.removeEventListener('abort', cancelPreference);
      });
    const timer = window.setTimeout(() => { controller.abort(); setState('error'); }, 15000);
    void (async () => {
      const details: MusicPublicRelease[] = [];
      let errors = failed;
      try {
        // Sequential reads bound concurrency; every response is scoped to this profile lifetime.
        for (const release of releases ?? []) {
          if (controller.signal.aborted) return;
          try { details.push(await musicReleases.getPublic(release.slug, controller.signal)); }
          catch { errors = true; }
        }
        if (controller.signal.aborted) { setState('error'); return; }
        const tracks = prioritizeFeaturedTrack([...artistTracks(details, artistId), ...fallbackTracks], artistId, await featured);
        if (controller.signal.aborted) return;
        setCatalogTracks(tracks);
        loadArtist(profileKey, tracks);
        loadedKey.current = profileKey;
        setState(tracks.length ? 'loaded' : errors ? 'error' : 'empty');
      } finally { window.clearTimeout(timer); }
    })();
    return () => { window.clearTimeout(timer); window.clearTimeout(preferenceTimer); controller.abort(); preference.abort(); };
  }, [artistId, artistName, attempt, failed, legacyReleases, loadArtist, loading, profileKey, releases]);

  if (!player) return null;
  const exhausted = state === 'empty' || (state === 'loaded' && player.status === 'unavailable');
  return <Stack spacing={1}>
    {canManage && artistId && state !== 'loading' && <ArtistFeaturedTrackEditor artistId={artistId} tracks={catalogTracks} />}
    {state === 'loading' && <span role="status">Preparando música de {artistName}…</span>}
    {state === 'error' && <Alert severity="warning" action={<Button onClick={() => {
      loadedKey.current = null; retryCatalog?.(); setAttempt((value) => value + 1);
    }}>Reintentar</Button>}>No pudimos comprobar la música de este artista. Revisa tu conexión.</Alert>}
    {exhausted && <Alert severity="info" action={<Button href="/inicio#radio">Ir a la radio</Button>}>
      Este artista aún no tiene música disponible para escuchar.
    </Alert>}
    {state === 'loaded' && !exhausted && <Button onClick={() => { void player.play(); }}>
      Escuchar a {artistName}
    </Button>}
  </Stack>;
}
