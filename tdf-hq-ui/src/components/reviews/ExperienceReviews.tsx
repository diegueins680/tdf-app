import { useMemo, useRef, useState } from 'react';
import { useInfiniteQuery, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import FlagOutlinedIcon from '@mui/icons-material/FlagOutlined';
import VerifiedIcon from '@mui/icons-material/Verified';
import {
  Alert,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  IconButton,
  Rating,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import { Directory } from '../../api/directory';
import {
  Reviews,
  type ExperienceReviewEligibility,
  type ExperienceReviewTargetKind,
} from '../../api/reviews';
import { useSession } from '../../session/SessionContext';

interface Props {
  targetKind: ExperienceReviewTargetKind;
  targetId: string;
  title?: string;
}

const sourceLabel: Record<ExperienceReviewEligibility['sourceKind'], string> = {
  event_ticket_order: 'Compra de entrada verificada',
  marketplace_order: 'Pedido de marketplace verificado',
  service_booking: 'Reserva completada',
  service_storefront_order: 'Servicio completado',
};

export default function ExperienceReviews({ targetKind, targetId, title = 'Reseñas' }: Props) {
  const { session } = useSession();
  const queryClient = useQueryClient();
  const [rating, setRating] = useState<number | null>(null);
  const [body, setBody] = useState('');
  const createAttempt = useRef<{ fingerprint: string; key: string } | null>(null);

  const reviewsQuery = useInfiniteQuery({
    queryKey: ['experience-reviews', targetKind, targetId],
    queryFn: ({ pageParam }) => Reviews.list(targetKind, targetId, pageParam),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (page) => page.nextCursor ?? undefined,
    enabled: Boolean(targetId),
  });
  const eligibilityQuery = useQuery({
    queryKey: ['experience-review-eligibility', targetKind, targetId],
    queryFn: () => Reviews.eligibility(targetKind, targetId),
    enabled: Boolean(session && targetId),
  });
  const eligibility = eligibilityQuery.data?.[0];
  const normalizedBody = body.trim();
  const bodyValid = normalizedBody.length === 0 || normalizedBody.length >= 10;

  const createMutation = useMutation({
    mutationFn: () => {
      if (!eligibility || !rating) throw new Error('No hay una interacción elegible para reseñar.');
      const request = {
        targetKind,
        targetId,
        sourceKind: eligibility.sourceKind,
        sourceId: eligibility.sourceId,
        rating,
        body: normalizedBody || undefined,
      };
      const fingerprint = JSON.stringify(request);
      if (createAttempt.current?.fingerprint !== fingerprint) {
        createAttempt.current = { fingerprint, key: crypto.randomUUID() };
      }
      return Reviews.create(request, createAttempt.current.key);
    },
    onSuccess: async () => {
      createAttempt.current = null;
      setRating(null);
      setBody('');
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['experience-reviews', targetKind, targetId] }),
        queryClient.invalidateQueries({ queryKey: ['experience-review-eligibility', targetKind, targetId] }),
      ]);
    },
  });

  const reportMutation = useMutation({
    mutationFn: (reviewId: string) => Directory.report({
      targetKind: 'review',
      targetId: reviewId,
      reasonCode: 'inappropriate_content',
    }),
  });

  const reviews = reviewsQuery.data?.pages.flatMap((page) => page.items) ?? [];
  const summary = reviewsQuery.data?.pages[0]?.summary;
  const summaryLabel = useMemo(() => {
    if (!summary || summary.count === 0) return 'Aún no hay reseñas';
    return `${Number(summary.average ?? 0).toFixed(1)} de 5 · ${summary.count} ${summary.count === 1 ? 'reseña' : 'reseñas'}`;
  }, [summary]);

  return (
    <Card variant="outlined" component="section" aria-labelledby={`reviews-${targetKind}-${targetId}`}>
      <CardContent>
        <Stack spacing={2}>
          <Box>
            <Typography id={`reviews-${targetKind}-${targetId}`} variant="h5" fontWeight={800}>{title}</Typography>
            <Stack direction="row" spacing={1} alignItems="center" mt={0.5}>
              <Rating value={summary?.average ?? 0} precision={0.1} readOnly aria-label={summaryLabel} />
              <Typography variant="body2" color="text.secondary">{summaryLabel}</Typography>
            </Stack>
          </Box>

          {session && eligibility && (
            <Stack spacing={1.25} sx={{ p: 2, borderRadius: 2, bgcolor: 'action.hover' }}>
              <Stack direction="row" spacing={1} alignItems="center">
                <VerifiedIcon color="success" fontSize="small" />
                <Typography fontWeight={700}>{sourceLabel[eligibility.sourceKind]}</Typography>
              </Stack>
              <Rating
                value={rating}
                onChange={(_, value) => setRating(value)}
                size="large"
                aria-label="Calificación de una a cinco estrellas"
              />
              <TextField
                label="Cuéntanos cómo fue (opcional)"
                value={body}
                onChange={(event) => setBody(event.target.value)}
                multiline
                minRows={3}
                inputProps={{ minLength: 10, maxLength: 2000 }}
                error={!bodyValid}
                helperText={!bodyValid ? 'Escribe al menos 10 caracteres o deja el comentario vacío.' : `${normalizedBody.length}/2000`}
              />
              {createMutation.error && <Alert severity="error">{createMutation.error instanceof Error ? createMutation.error.message : 'No se pudo publicar la reseña.'}</Alert>}
              <Button
                variant="contained"
                onClick={() => createMutation.mutate()}
                disabled={!rating || !bodyValid || createMutation.isPending}
                sx={{ alignSelf: 'flex-start' }}
              >
                {createMutation.isPending ? <CircularProgress size={20} color="inherit" /> : 'Publicar reseña verificada'}
              </Button>
            </Stack>
          )}

          {session && eligibilityQuery.isSuccess && !eligibility && (
            <Alert severity="info">Podrás reseñar después de completar una compra, asistencia o servicio relacionado.</Alert>
          )}
          {!session && <Alert severity="info">Inicia sesión para publicar una reseña después de completar una interacción.</Alert>}
          {reviewsQuery.isLoading && <CircularProgress size={24} />}
          {reviewsQuery.error && <Alert severity="error">No se pudieron cargar las reseñas.</Alert>}

          {reviews.map((review, index) => (
            <Box key={review.id}>
              {index > 0 && <Divider sx={{ mb: 2 }} />}
              <Stack direction="row" spacing={1.25} alignItems="flex-start">
                <Avatar src={review.author.avatarUrl ?? undefined}>{review.author.name.slice(0, 1).toUpperCase()}</Avatar>
                <Box sx={{ flex: 1, minWidth: 0 }}>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={0.75} alignItems={{ sm: 'center' }}>
                    <Typography fontWeight={700}>{review.author.name}</Typography>
                    <Chip icon={<VerifiedIcon />} label="Interacción verificada" size="small" color="success" variant="outlined" />
                  </Stack>
                  <Rating value={review.rating} readOnly size="small" aria-label={`${review.rating} de 5 estrellas`} />
                  {review.body && <Typography sx={{ mt: 0.5, whiteSpace: 'pre-wrap' }}>{review.body}</Typography>}
                  <Typography variant="caption" color="text.secondary">
                    {new Date(review.createdAt).toLocaleDateString()}
                  </Typography>
                </Box>
                {session && (
                  <Tooltip title="Reportar reseña">
                    <IconButton
                      size="small"
                      aria-label={`Reportar reseña de ${review.author.name}`}
                      onClick={() => reportMutation.mutate(review.id)}
                      disabled={reportMutation.isPending}
                    >
                      <FlagOutlinedIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                )}
              </Stack>
            </Box>
          ))}
          {reviewsQuery.hasNextPage && (
            <Button
              variant="outlined"
              onClick={() => { void reviewsQuery.fetchNextPage(); }}
              disabled={reviewsQuery.isFetchingNextPage}
              sx={{ alignSelf: 'flex-start' }}
            >
              {reviewsQuery.isFetchingNextPage ? 'Cargando…' : 'Ver más reseñas'}
            </Button>
          )}
          {reportMutation.isSuccess && <Alert severity="success">Gracias. La reseña fue enviada a moderación.</Alert>}
        </Stack>
      </CardContent>
    </Card>
  );
}
