import { useEffect, useRef, useState } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Card, CardContent, Chip, CircularProgress, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { SocialEventsAPI, type SocialEventDTO } from '../../api/socialEvents';
import { resolveApiBase } from '../../config/apiBase';
import { useOwnedSessionDraft } from '../../session/useOwnedSessionDraft';

interface PosterAttempt { file: File; isCurrentOwner: () => boolean }

export default function EventPreparationPanel({ event, ownerId }: { event: SocialEventDTO; ownerId: number }) {
  const eventId = event.eventId!;
  const queryClient = useQueryClient();
  const input = useRef<HTMLInputElement>(null);
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const uploaded = useRef<{ file: File; url: string } | null>(null);
  const [feedback, setFeedback] = useState('');
  const draft = useOwnedSessionDraft<File | null>({ ownerId, scope: eventId, baseline: null, empty: null });
  const mutation = useMutation({
    mutationFn: async (attempt: PosterAttempt) => {
      const ensureOwner = () => {
        if (!mounted.current || !attempt.isCurrentOwner()) throw new Error('Tu sesión cambió. Vuelve al evento antes de guardar.');
      };
      ensureOwner();
      if (!['image/jpeg', 'image/png', 'image/webp'].includes(attempt.file.type)) {
        throw new Error('Selecciona una imagen JPG, PNG o WebP.');
      }
      if (!attempt.file.size || attempt.file.size > 10 * 1024 * 1024) {
        throw new Error('La imagen debe pesar entre 1 byte y 10 MB.');
      }
      let url = uploaded.current?.file === attempt.file ? uploaded.current.url : '';
      if (!url) {
        const result = await SocialEventsAPI.uploadEventImage(eventId, attempt.file);
        ensureOwner();
        try {
          const parsed = new URL(result.eiuPublicUrl);
          if (parsed.protocol !== 'https:' || parsed.username || parsed.password) throw new Error();
          url = parsed.toString();
        } catch {
          throw new Error('No se pudo confirmar la carga. Conservamos la imagen para reintentar.');
        }
        uploaded.current = { file: attempt.file, url };
      }
      ensureOwner();
      // The upload endpoint attaches the poster atomically. Confirm it without
      // issuing a full event update that could overwrite another editor's work.
      const latest = await SocialEventsAPI.getEvent(eventId);
      ensureOwner();
      if (latest.eventImageUrl !== url) throw new Error('La carga terminó, pero falta confirmar el afiche. Reintenta la verificación.');
      return latest;
    },
    onSuccess: (_event, attempt) => {
      if (!mounted.current || !attempt.isCurrentOwner()) return;
      uploaded.current = null;
      draft.setDraft(null);
      if (input.current) input.current.value = '';
      setFeedback('Afiche guardado.');
      void queryClient.invalidateQueries({ queryKey: ['social-event', eventId] });
      void queryClient.invalidateQueries({ queryKey: ['social-events'] });
    },
  });
  return <Card variant="outlined" component="section" aria-labelledby="event-preparation-title">
    <CardContent><Stack spacing={1.5}>
      <Typography id="event-preparation-title" component="h2" variant="h6">Preparar evento</Typography>
      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
        <Chip label={event.eventIsPublic ? 'Visible al público' : 'Borrador privado'} color={event.eventIsPublic ? 'success' : 'default'} />
        <Chip label={event.eventImageUrl ? 'Afiche cargado' : 'Falta el afiche'} color={event.eventImageUrl ? 'success' : 'warning'} />
        <Chip label={event.eventTicketPurchaseEnabled ? 'Venta habilitada' : 'Venta pendiente'} />
      </Stack>
      <Typography variant="body2">Guarda el afiche aquí y prepara el cronograma y las publicaciones. Revisa la visibilidad, los cupos y las condiciones de venta antes de difundir el enlace.</Typography>
      <Box>
        <Typography component="label" htmlFor="event-poster-file" variant="body2" fontWeight={700}>Afiche del evento</Typography>
        <input id="event-poster-file" ref={input} type="file" accept="image/jpeg,image/png,image/webp" disabled={!draft.ready || mutation.isPending} onChange={(change) => {
          draft.setDraft(change.target.files?.[0] ?? null);
          uploaded.current = null;
          setFeedback('');
          mutation.reset();
        }} style={{ display: 'block', marginTop: 8, maxWidth: '100%' }} />
        <Typography variant="caption" color="text.secondary">JPG, PNG o WebP · máximo 10 MB. Se conserva la imagen actual hasta guardar.</Typography>
      </Box>
      {mutation.error && <Alert severity="error">{mutation.error instanceof Error ? mutation.error.message : 'No se pudo guardar. Reintenta con la imagen seleccionada.'}</Alert>}
      {feedback && <Alert severity="success" role="status">{feedback}</Alert>}
      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
        <Button variant="contained" disabled={!draft.ready || !draft.draft || mutation.isPending} onClick={() => {
          if (draft.draft) mutation.mutate({ file: draft.draft, isCurrentOwner: draft.isCurrentOwner });
        }}>{mutation.isPending ? <><CircularProgress size={16} sx={{ mr: 1 }} color="inherit" />Guardando afiche…</> : mutation.isError ? 'Reintentar afiche' : 'Guardar afiche'}</Button>
        <Button component={RouterLink} to={`/social/eventos/${eventId}/logistica`}>Cronograma y equipo</Button>
        <Button component="a" href={`${resolveApiBase()}/campaign-platform/#campanas`} target="_blank" rel="noopener noreferrer">Abrir campañas</Button>
      </Stack>
    </Stack></CardContent>
  </Card>;
}
