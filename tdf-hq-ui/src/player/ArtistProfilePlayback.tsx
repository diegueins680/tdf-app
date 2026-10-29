import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Stack } from '@mui/material';
import { musicReleases, type MusicPublicRelease, type MusicPublicReleaseSummary } from '../api/musicReleases';
import { useOptionalPlayer } from './PlayerProvider';
import { musicSourceQuality } from './sourceMetadata';
import type { PlayerTrack } from './types';

/** Public detail responses supply membership; asset access is reauthorized by the engine. */
export function artistTracks(releases: MusicPublicRelease[], artistId: number): PlayerTrack[] {
  const seen = new Set<string>();
  return releases.filter((release) => release.artistPartyId === artistId).flatMap((release) =>
    [...release.tracks].sort((a, b) => a.discNumber - b.discNumber || a.trackNumber - b.trackNumber)
      .filter((track) => track.sources.length > 0 && !seen.has(track.recordingId) && Boolean(seen.add(track.recordingId)))
      .map((track) => ({
        id: track.trackId, recordingId: track.recordingId, releaseId: release.id,
        releaseVersionId: release.versionId, title: track.title, artist: track.displayArtist,
        durationMs: track.durationMs, previewEndMs: 30000,
        sources: track.sources.map((source) => ({ url: '', assetId: source.assetId,
          quality: musicSourceQuality(source.technicalMetadata, source.mediaType),
          mediaType: source.mediaType, preview: source.role === 'preview_audio' })),
      })));
}

export default function ArtistProfilePlayback({ profileKey, artistId, artistName, releases, loading, failed, retryCatalog }: {
  profileKey: string; artistId: number | null; artistName: string;
  releases: MusicPublicReleaseSummary[] | undefined; loading: boolean; failed: boolean; retryCatalog?: () => void;
}) {
  const player = useOptionalPlayer();
  const beginArtist = player?.beginArtist;
  const loadArtist = player?.loadArtist;
  const loadedKey = useRef<string | null>(null);
  const [state, setState] = useState<'loading' | 'loaded' | 'empty' | 'error'>('loading');
  const [attempt, setAttempt] = useState(0);
  useEffect(() => {
    beginArtist?.(profileKey);
    loadedKey.current = null;
    setState('loading');
  }, [beginArtist, profileKey]);

  useEffect(() => {
    if (!loadArtist || !artistId || loading || loadedKey.current === profileKey) return;
    if (failed) { setState('error'); return; }
    if (!releases) return;
    const controller = new AbortController();
    const timer = window.setTimeout(() => { controller.abort(); setState('error'); }, 15000);
    void (async () => {
      const details: MusicPublicRelease[] = [];
      let errors = false;
      try {
        // Sequential reads bound concurrency; every response is scoped to this profile lifetime.
        for (const release of releases) {
          if (controller.signal.aborted) return;
          try { details.push(await musicReleases.getPublic(release.slug, controller.signal)); }
          catch { errors = true; }
        }
        if (controller.signal.aborted) { setState('error'); return; }
        const tracks = artistTracks(details, artistId);
        loadArtist(profileKey, tracks);
        loadedKey.current = profileKey;
        setState(tracks.length ? 'loaded' : errors ? 'error' : 'empty');
      } finally { window.clearTimeout(timer); }
    })();
    return () => { window.clearTimeout(timer); controller.abort(); };
  }, [artistId, attempt, failed, loadArtist, loading, profileKey, releases]);

  if (!player) return null;
  const exhausted = state === 'empty' || (state === 'loaded' && player.status === 'unavailable');
  return <Stack spacing={1}>
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
