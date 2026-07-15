import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import TravelExploreIcon from '@mui/icons-material/TravelExplore';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import PageShell, { EmptyState } from '../components/PageShell';
import {
  SocialDiscoveryAPI,
  type DiscoveryReviewStatus,
  type SocialDiscoveryPost,
} from '../api/socialDiscovery';

const reviewLabels: Record<DiscoveryReviewStatus, string> = {
  pending: 'Pendiente',
  approved: 'Aprobado',
  dismissed: 'Descartado',
};

const formatDate = (value?: string | null) => {
  if (!value) return 'Sin fecha de publicación';
  const parsed = new Date(value);
  return Number.isNaN(parsed.getTime()) ? value : parsed.toLocaleString('es-EC');
};

interface ReviewDraft {
  status: DiscoveryReviewStatus;
  notes: string;
}

const initialDraft = (post: SocialDiscoveryPost): ReviewDraft => ({
  status: post.reviewStatus,
  notes: post.reviewNotes ?? '',
});

export default function RecentlyDiscoveredEventsPage() {
  const queryClient = useQueryClient();
  const [status, setStatus] = useState<DiscoveryReviewStatus>('pending');
  const [drafts, setDrafts] = useState<Record<string, ReviewDraft>>({});
  const postsQuery = useQuery({
    queryKey: ['social-discovery-posts', status],
    queryFn: () => SocialDiscoveryAPI.list({ status, limit: 100 }),
  });
  const reviewMutation = useMutation({
    mutationFn: ({ postId, draft }: { postId: string; draft: ReviewDraft }) =>
      SocialDiscoveryAPI.review(postId, { status: draft.status, notes: draft.notes.trim() || undefined }),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['social-discovery-posts'] });
    },
  });

  const posts = postsQuery.data ?? [];
  const mutationError = reviewMutation.error instanceof Error ? reviewMutation.error.message : null;
  const subtitle = useMemo(
    () => 'Publicaciones de cuentas profesionales conectadas que contienen señales de conciertos, eventos o entradas.',
    [],
  );

  const readDraft = (post: SocialDiscoveryPost) => drafts[post.id] ?? initialDraft(post);
  const writeDraft = (post: SocialDiscoveryPost, update: Partial<ReviewDraft>) => {
    setDrafts((current) => ({
      ...current,
      [post.id]: { ...(current[post.id] ?? initialDraft(post)), ...update },
    }));
  };

  return (
    <PageShell
      title="Eventos descubiertos"
      subtitle={subtitle}
      loading={postsQuery.isLoading}
      actions={(
        <FormControl size="small" sx={{ minWidth: 148 }}>
          <InputLabel id="discovery-status-label">Revisión</InputLabel>
          <Select
            labelId="discovery-status-label"
            label="Revisión"
            value={status}
            onChange={(event) => setStatus(event.target.value as DiscoveryReviewStatus)}
          >
            <MenuItem value="pending">Pendientes</MenuItem>
            <MenuItem value="approved">Aprobados</MenuItem>
            <MenuItem value="dismissed">Descartados</MenuItem>
          </Select>
        </FormControl>
      )}
    >
      <Stack spacing={2}>
        <Alert severity="info">
          Esta cola muestra candidatos detectados por texto. Confirma fecha, lugar y enlace antes de crear un evento público.
        </Alert>
        {postsQuery.isError && (
          <Alert severity="error">{postsQuery.error instanceof Error ? postsQuery.error.message : 'No se pudieron cargar los candidatos.'}</Alert>
        )}
        {mutationError && <Alert severity="error">No se pudo guardar la revisión: {mutationError}</Alert>}
        {!postsQuery.isLoading && !postsQuery.isError && posts.length === 0 && (
          <EmptyState
            icon={<TravelExploreIcon fontSize="inherit" />}
            title="No hay eventos descubiertos en esta cola"
            description="Cuando una cuenta profesional conectada publique una señal de evento, aparecerá aquí para revisión."
          />
        )}
        {posts.map((post) => {
          const draft = readDraft(post);
          const mediaUrl = post.mediaUrls[0];
          const saving = reviewMutation.isPending && reviewMutation.variables?.postId === post.id;
          return (
            <Card key={post.id} variant="outlined">
              <CardContent>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2.5}>
                  {mediaUrl && (
                    <Box
                      component="img"
                      src={mediaUrl}
                      alt="Vista previa de publicación descubierta"
                      sx={{ width: { xs: '100%', md: 220 }, maxHeight: 220, objectFit: 'cover', borderRadius: 1 }}
                    />
                  )}
                  <Stack spacing={1.5} sx={{ minWidth: 0, flex: 1 }}>
                    <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                      <Typography variant="subtitle1" fontWeight={700}>
                        {post.sourceHandle ? `@${post.sourceHandle.replace(/^@/, '')}` : 'Cuenta conectada'}
                      </Typography>
                      <Chip size="small" label={reviewLabels[post.reviewStatus]} color={post.reviewStatus === 'approved' ? 'success' : post.reviewStatus === 'dismissed' ? 'default' : 'warning'} />
                      <Typography variant="caption" color="text.secondary">{formatDate(post.postedAt ?? post.fetchedAt)}</Typography>
                    </Stack>
                    <Stack direction="row" spacing={0.75} flexWrap="wrap" useFlexGap>
                      {post.detectedTerms.map((term) => <Chip key={term} size="small" label={term} variant="outlined" />)}
                    </Stack>
                    <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap' }}>
                      {post.caption?.trim() || 'La publicación no incluye texto.'}
                    </Typography>
                    {post.permalink && (
                      <Button component="a" href={post.permalink} target="_blank" rel="noreferrer" size="small" startIcon={<OpenInNewIcon />} sx={{ alignSelf: 'flex-start' }}>
                        Abrir publicación fuente
                      </Button>
                    )}
                    <Divider />
                    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.25} alignItems={{ sm: 'flex-start' }}>
                      <FormControl size="small" sx={{ minWidth: 145 }}>
                        <InputLabel id={`review-status-${post.id}`}>Decisión</InputLabel>
                        <Select
                          labelId={`review-status-${post.id}`}
                          label="Decisión"
                          value={draft.status}
                          onChange={(event) => writeDraft(post, { status: event.target.value as DiscoveryReviewStatus })}
                        >
                          <MenuItem value="pending">Pendiente</MenuItem>
                          <MenuItem value="approved">Aprobar</MenuItem>
                          <MenuItem value="dismissed">Descartar</MenuItem>
                        </Select>
                      </FormControl>
                      <TextField
                        label="Notas de revisión"
                        value={draft.notes}
                        onChange={(event) => writeDraft(post, { notes: event.target.value })}
                        size="small"
                        multiline
                        minRows={1}
                        fullWidth
                        inputProps={{ maxLength: 2000 }}
                      />
                      <Button
                        variant="contained"
                        onClick={() => reviewMutation.mutate({ postId: post.id, draft })}
                        disabled={saving}
                        sx={{ minWidth: 120 }}
                      >
                        {saving ? <CircularProgress size={20} color="inherit" /> : 'Guardar'}
                      </Button>
                    </Stack>
                  </Stack>
                </Stack>
              </CardContent>
            </Card>
          );
        })}
      </Stack>
    </PageShell>
  );
}
