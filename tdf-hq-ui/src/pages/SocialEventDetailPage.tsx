import { useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import ImageIcon from '@mui/icons-material/Image';
import LinkIcon from '@mui/icons-material/Link';
import { Alert, Avatar, Box, Button, ButtonBase, Card, CardContent, Chip, CircularProgress, Divider, Stack, TextField, Typography } from '@mui/material';
import { Link as RouterLink, useParams } from 'react-router-dom';
import PageShell, { EmptyState } from '../components/PageShell';
import { SocialEventsAPI, type SocialEventMomentCreateDTO } from '../api/socialEvents';
import { useSession } from '../session/SessionContext';

const formatDate = (value?: string | null) => {
  if (!value) return '';
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? value : date.toLocaleString('es-EC', { dateStyle: 'medium', timeStyle: 'short' });
};

const inferredMediaType = (url: string): 'image' | 'video' => /\.(mp4|mov|webm|m4v)(?:$|[?#])/i.test(url) ? 'video' : 'image';

export default function SocialEventDetailPage() {
  const { eventId = '' } = useParams();
  const { session } = useSession();
  const queryClient = useQueryClient();
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [caption, setCaption] = useState('');
  const [mediaUrl, setMediaUrl] = useState('');
  const [file, setFile] = useState<File | null>(null);
  const eventQuery = useQuery({
    queryKey: ['social-event', eventId],
    queryFn: () => SocialEventsAPI.getEvent(eventId),
    enabled: Boolean(eventId),
  });
  const momentsQuery = useQuery({
    queryKey: ['social-event-moments', eventId],
    queryFn: () => SocialEventsAPI.listMoments(eventId),
    enabled: Boolean(eventId),
  });
  const postMutation = useMutation({
    mutationFn: async () => {
      if (!session) throw new Error('Inicia sesión para publicar en el evento.');
      let url = mediaUrl.trim();
      if (file) {
        if (!file.type.startsWith('image/')) throw new Error('Por ahora puedes subir imágenes; usa un enlace para publicar video.');
        const uploaded = await SocialEventsAPI.uploadMomentImage(eventId, file);
        url = uploaded.eiuPublicUrl;
      }
      if (!url) throw new Error('Selecciona una imagen o pega un enlace de imagen/video.');
      const payload: SocialEventMomentCreateDTO = {
        emCreateCaption: caption.trim() || undefined,
        emCreateMediaUrl: url,
        emCreateMediaType: inferredMediaType(url),
      };
      return SocialEventsAPI.createMoment(eventId, payload);
    },
    onSuccess: () => {
      setCaption('');
      setMediaUrl('');
      setFile(null);
      void queryClient.invalidateQueries({ queryKey: ['social-event-moments', eventId] });
    },
  });

  const event = eventQuery.data;
  const error = eventQuery.error ?? momentsQuery.error ?? postMutation.error;

  return (
    <PageShell
      title={event?.eventTitle ?? 'Evento'}
      subtitle={event ? `${formatDate(event.eventStart)} · ${event.eventType ?? 'Evento'}` : undefined}
      loading={eventQuery.isLoading}
      actions={<Button component={RouterLink} to="/social/eventos" startIcon={<ArrowBackIcon />}>Eventos</Button>}
    >
      <Stack spacing={2.5}>
        {error && <Alert severity="error">{error instanceof Error ? error.message : 'No se pudo cargar el evento.'}</Alert>}
        {event && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={1.5}>
                {event.eventImageUrl && <Box component="img" src={event.eventImageUrl} alt={`Afiche de ${event.eventTitle}`} sx={{ width: '100%', maxHeight: 360, objectFit: 'cover', borderRadius: 2 }} />}
                <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                  <Chip label={formatDate(event.eventStart)} color="primary" />
                  {event.eventStatus && <Chip label={event.eventStatus} variant="outlined" />}
                  {event.eventCapacity && <Chip label={`${event.eventCapacity} personas`} variant="outlined" />}
                </Stack>
                {event.eventDescription && <Typography sx={{ whiteSpace: 'pre-wrap' }}>{event.eventDescription}</Typography>}
              </Stack>
            </CardContent>
          </Card>
        )}

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="h6">Publicar media</Typography>
              {!session ? (
                <Alert severity="info">Inicia sesión para añadir imágenes o enlaces de video a este evento.</Alert>
              ) : (
                <>
                  <TextField label="Describe tu publicación" value={caption} onChange={(event) => setCaption(event.target.value)} multiline minRows={2} inputProps={{ maxLength: 1000 }} />
                  <TextField label="Enlace de imagen o video" value={mediaUrl} onChange={(event) => setMediaUrl(event.target.value)} disabled={Boolean(file)} placeholder="https://…" InputProps={{ startAdornment: <LinkIcon sx={{ mr: 1, color: 'text.secondary' }} /> }} />
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                    <Button component="label" variant="outlined" startIcon={<ImageIcon />}>
                      Seleccionar imagen
                      <input ref={fileInputRef} hidden type="file" accept="image/jpeg,image/png,image/webp,image/gif,image/bmp" onChange={(event) => setFile(event.target.files?.[0] ?? null)} />
                    </Button>
                    <Typography variant="caption" color="text.secondary" sx={{ flex: 1 }}>{file ? file.name : 'Imágenes de hasta 10 MB. Para video, pega un enlace público.'}</Typography>
                    <Button variant="contained" onClick={() => postMutation.mutate()} disabled={postMutation.isPending || (!file && !mediaUrl.trim())}>
                      {postMutation.isPending ? <CircularProgress size={20} color="inherit" /> : 'Publicar'}
                    </Button>
                  </Stack>
                </>
              )}
            </Stack>
          </CardContent>
        </Card>

        <Stack spacing={1.5}>
          <Typography variant="h5">Publicaciones</Typography>
          {momentsQuery.isLoading ? <CircularProgress size={24} /> : momentsQuery.data?.length === 0 ? (
            <EmptyState icon={<ImageIcon fontSize="inherit" />} title="Todavía no hay publicaciones" description="Sé la primera persona en compartir un momento de este evento." />
          ) : momentsQuery.data?.map((moment) => (
            <Card key={moment.emId ?? `${moment.emAuthorPartyId}-${moment.emCreatedAt}`} variant="outlined">
              <CardContent>
                <Stack spacing={1.25}>
                  {moment.emAuthorPartyId ? (
                    <ButtonBase
                      component={RouterLink}
                      to={`/perfil/${encodeURIComponent(moment.emAuthorPartyId)}`}
                      sx={{ alignSelf: 'flex-start', borderRadius: 2, textAlign: 'left' }}
                    >
                      <Stack direction="row" spacing={1} alignItems="center">
                        <Avatar>{moment.emAuthorName.slice(0, 1).toUpperCase()}</Avatar>
                        <Box>
                          <Typography fontWeight={700}>{moment.emAuthorName}</Typography>
                          <Typography variant="caption" color="text.secondary">Publicado {formatDate(moment.emCreatedAt)}</Typography>
                        </Box>
                      </Stack>
                    </ButtonBase>
                  ) : (
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Avatar>{moment.emAuthorName.slice(0, 1).toUpperCase()}</Avatar>
                      <Box>
                        <Typography fontWeight={700}>{moment.emAuthorName}</Typography>
                        <Typography variant="caption" color="text.secondary">Publicado {formatDate(moment.emCreatedAt)}</Typography>
                      </Box>
                    </Stack>
                  )}
                  {moment.emMediaType === 'video' ? <Box component="video" src={moment.emMediaUrl} controls sx={{ width: '100%', maxHeight: 520, borderRadius: 2, bgcolor: 'black' }} /> : <Box component="img" src={moment.emMediaUrl} alt={moment.emCaption ?? `Publicación de ${moment.emAuthorName}`} sx={{ width: '100%', maxHeight: 520, objectFit: 'contain', borderRadius: 2, bgcolor: 'action.hover' }} />}
                  {moment.emCaption && <><Divider /><Typography sx={{ whiteSpace: 'pre-wrap' }}>{moment.emCaption}</Typography></>}
                </Stack>
              </CardContent>
            </Card>
          ))}
        </Stack>
      </Stack>
    </PageShell>
  );
}
