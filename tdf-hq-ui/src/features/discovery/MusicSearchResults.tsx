import { Alert, Box, Button, CircularProgress, Stack, Typography } from '@mui/material';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import { useQuery } from '@tanstack/react-query';
import { useId, useState } from 'react';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink } from 'react-router-dom';
import { musicReleases, type MusicPublicTrackSearchHit } from '../../api/musicReleases';
import { publicMusicTrack } from '../../player/publicMusicTrack';
import { PLAYER_LOAD_TRACK_EVENT } from '../../player/types';

const PAGE_SIZE = 12;

export default function MusicSearchResults({ query, changingQuery }: { query: string; changingQuery: boolean }) {
  const { t } = useTranslation();
  const headingId = useId();
  const canonicalQuery = query.trim().replace(/\s+/gu, ' ');
  const [page, setPage] = useState({ query: canonicalQuery, offset: 0 });
  // Derive the first page immediately, before an effect could expose an old page
  // under a new query. Directory filters and its URL/history stay independent.
  const offset = page.query === canonicalQuery ? page.offset : 0;
  if (page.query !== canonicalQuery) setPage({ query: canonicalQuery, offset: 0 });
  const results = useQuery({
    queryKey: ['music', 'track-search', canonicalQuery, offset],
    queryFn: ({ signal }) => musicReleases.searchTracks(canonicalQuery, { limit: PAGE_SIZE + 1, offset, signal }),
    enabled: canonicalQuery.length > 0 && !changingQuery,
    retry: false,
    staleTime: 0,
    gcTime: 0,
    // No placeholder/previous-query data can acquire playback actions.
  });
  const ready = !changingQuery && results.isSuccess && !results.isFetching;
  const hits = ready ? results.data.slice(0, PAGE_SIZE) : [];
  const play = (selected: MusicPublicTrackSearchHit) => {
    if (!ready || selected.sources.length === 0) return;
    const queue = hits.filter((hit) => hit.sources.length > 0).map(publicMusicTrack);
    const track = queue.find((item) => item.id === selected.trackId && item.recordingId === selected.recordingId);
    if (track) window.dispatchEvent(new CustomEvent(PLAYER_LOAD_TRACK_EVENT, { detail: { track, queue, autoplay: true } }));
  };
  const changePage = (next: number) => setPage({ query: canonicalQuery, offset: next });

  return <Box component="section" aria-labelledby={headingId} sx={{ py: 2 }}>
    <Typography component="h2" variant="h5" fontWeight={800} id={headingId}>{t('directorySearch.music.heading')}</Typography>
    <Typography color="text.secondary" sx={{ mb: 2 }}>{t('directorySearch.music.scope')}</Typography>
    {!canonicalQuery ? <Typography>{t('directorySearch.music.prompt')}</Typography> : changingQuery || results.isFetching ?
      <Stack direction="row" alignItems="center" spacing={1} role="status"><CircularProgress size={20} aria-label={t('directorySearch.music.loading')} /><Typography>{t('directorySearch.music.loading')}</Typography></Stack> :
      results.isError ? <Alert severity="error" action={<Button onClick={() => { void results.refetch(); }} sx={{ minHeight: 44 }}>{t('directorySearch.music.retry')}</Button>}>{t('directorySearch.music.error')}</Alert> :
        ready && hits.length === 0 ? <Typography role="status">{t(offset ? 'directorySearch.music.emptyPage' : 'directorySearch.music.empty')}</Typography> : null}
    {hits.length > 0 && <Stack component="ul" spacing={1} sx={{ p: 0, listStyle: 'none' }}>
      {hits.map((hit) => <Box component="li" key={`${hit.releaseVersionId}:${hit.trackId}`} sx={{ border: 1, borderColor: 'divider', borderRadius: 2, p: 2, overflowWrap: 'anywhere' }}>
        <Typography component="h3" variant="h6">{hit.title}</Typography>
        <Typography>{hit.displayArtist} · {hit.releaseTitle}</Typography>
        <Stack direction="row" spacing={1} flexWrap="wrap" sx={{ mt: 1 }}>
          <Button startIcon={<PlayArrowIcon />} onClick={() => play(hit)} disabled={hit.sources.length === 0}
            aria-label={t('directorySearch.music.play', { title: hit.title })} sx={{ minHeight: 44 }}>
            {t('directorySearch.music.play', { title: hit.title })}
          </Button>
          <Button component={RouterLink} to={`/musica/${encodeURIComponent(hit.releaseSlug)}`} sx={{ minHeight: 44 }}>
            {t('directorySearch.music.release', { title: hit.releaseTitle })}
          </Button>
        </Stack>
      </Box>)}
    </Stack>}
    {canonicalQuery && !changingQuery && <Stack direction="row" gap={1} flexWrap="wrap" alignItems="center" sx={{ mt: 2 }}>
      <Button onClick={() => changePage(Math.max(0, offset - PAGE_SIZE))} disabled={offset === 0 || results.isFetching} sx={{ minHeight: 44 }}>{t('directorySearch.music.previous')}</Button>
      <Typography aria-live="polite">{t('directorySearch.music.page', { page: offset / PAGE_SIZE + 1 })}</Typography>
      <Button onClick={() => changePage(offset + PAGE_SIZE)} disabled={!ready || results.data.length <= PAGE_SIZE} sx={{ minHeight: 44 }}>{t('directorySearch.music.next')}</Button>
    </Stack>}
  </Box>;
}
