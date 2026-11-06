import { Alert, Box, Button, Card, CardContent, Chip, List, ListItem, Stack, Typography } from '@mui/material';
import { PLAYER_LOAD_TRACK_EVENT, type LoadPlayerTrackDetail } from '../../player/types';
import { sessionPlayerTrack, type NativeSession, type SessionChapter } from './nativeSessions';

export function NativeSessionCard({ session }: { session: NativeSession }) {
  const play = (mode: 'audio' | 'video', chapter?: SessionChapter) => {
    const track = sessionPlayerTrack(session, chapter);
    const detail: LoadPlayerTrackDetail = { track, queue: chapter ? session.chapters.map(c => sessionPlayerTrack(session, c)) : [track],
      mediaMode: mode, autoplay: true };
    window.dispatchEvent(new CustomEvent(PLAYER_LOAD_TRACK_EVENT, { detail }));
  };
  const ready = session.state === 'ready';
  return <Card component="article" aria-label={session.title} sx={{ my: 2 }}>
    {session.posterUrl && <Box component="img" src={session.posterUrl} alt="" loading="lazy" sx={{ width: '100%', maxHeight: 300, objectFit: 'cover' }} />}
    <CardContent>
      <Chip label="Sesión TDF · Video y audio" size="small" />
      <Typography component="h3" variant="h6" sx={{ mt: 1 }}>{session.title}</Typography>
      <Typography color="text.secondary">{session.artist} · {Math.floor(session.durationMs / 60000)}:{String(Math.floor(session.durationMs / 1000) % 60).padStart(2, '0')}</Typography>
      <Stack direction="row" spacing={1} sx={{ my: 2 }}>
        <Button variant="contained" disabled={!ready} onClick={() => play('video')}>Ver sesión</Button>
        <Button variant="outlined" disabled={!ready} onClick={() => play('audio')}>Escuchar</Button>
      </Stack>
      {!ready && <Alert severity={session.state === 'failed' ? 'warning' : 'info'}>
        {session.state === 'processing' ? 'Estamos preparando el video y el audio.' : session.state === 'failed' ? 'No se pudo preparar esta sesión. Inténtalo más tarde.' : 'La reproducción en TDF estará disponible cuando recibamos el archivo original.'}
      </Alert>}
      {session.chapters.length > 0 && <List aria-label={`Canciones de ${session.title}`} disablePadding>
        {session.chapters.map((chapter, index) => <ListItem key={chapter.id} disableGutters sx={{ gap: 1, flexWrap: 'wrap' }}>
          <Typography sx={{ flex: '1 1 160px' }}>{index + 1}. {chapter.title}</Typography>
          <Typography variant="caption">{Math.floor((chapter.endMs - chapter.startMs) / 60000)}:{String(Math.floor((chapter.endMs - chapter.startMs) / 1000) % 60).padStart(2, '0')}</Typography>
          <Button size="small" disabled={!ready} aria-label={`Ver ${chapter.title}`} onClick={() => play('video', chapter)}>Ver</Button>
          <Button size="small" disabled={!ready} aria-label={`Escuchar ${chapter.title}`} onClick={() => play('audio', chapter)}>Escuchar</Button>
        </ListItem>)}
      </List>}
      <Box component="details" sx={{ mt: 1 }}>
        <summary>Sobre la sesión y créditos</summary>
        {session.description && <Typography sx={{ whiteSpace: 'pre-line', mt: 1 }}>{session.description}</Typography>}
        {session.recordedAt && <Typography>Grabación: {session.recordedAt.slice(0, 10)}</Typography>}
        {session.publishedAt && <Typography>Publicación: {session.publishedAt.slice(0, 10)}</Typography>}
        {session.credits.map((c, i) => <Typography key={`${c.role}:${i}`} variant="body2">{c.role}: {c.name}</Typography>)}
      </Box>
    </CardContent>
  </Card>;
}
