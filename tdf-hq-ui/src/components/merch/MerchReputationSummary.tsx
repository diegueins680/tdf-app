import {
  Alert,
  Box,
  Button,
  Chip,
  CircularProgress,
  LinearProgress,
  Stack,
  Typography,
} from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { MerchReputation, type MerchReputationSummary as Summary } from '../../api/merchReputation';
import { ApiError } from '../../api/client';

const confidenceLabel: Record<NonNullable<Summary['confidence']>, string> = {
  new: 'Evidencia inicial',
  limited: 'Evidencia limitada',
  moderate: 'Evidencia moderada',
  strong: 'Evidencia sólida',
};

const dimensionLabel: Record<string, string> = {
  description_accuracy: 'Conforme a la descripción',
  product_quality: 'Calidad del producto',
  preparation_dispatch: 'Preparación y despacho',
  communication: 'Comunicación',
  packaging: 'Empaque',
  problem_resolution: 'Resolución de problemas',
};

export function MerchReputationSummary({ summary, compact = false }: { summary: Summary; compact?: boolean }) {
  const reviewCount = summary.verifiedReviewCount ?? summary.verifiedPurchaseReviewCount ?? 0;
  const isNew = summary.state === 'new_store';

  return (
    <Stack
      spacing={compact ? 0.75 : 1.5}
      aria-label={summary.subjectKind === 'product' ? 'Valoración del producto' : 'Reputación comercial de la tienda'}
    >
      {summary.subjectKind !== 'product' && (
        <Typography variant="overline" color="text.secondary" fontWeight={800}>
          Reputación comercial
        </Typography>
      )}
      {isNew ? (
        <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
          <Chip label="Tienda nueva" color="info" size="small" />
          <Typography variant="body2" color="text.secondary">
            Aún no hay cinco órdenes evaluables; no asignamos una nota artificial.
          </Typography>
        </Stack>
      ) : summary.state === 'unrated' || summary.rating == null ? (
        <Typography variant="body2" color="text.secondary">Sin evaluaciones verificadas todavía.</Typography>
      ) : (
        <Stack direction="row" spacing={1} alignItems="baseline" flexWrap="wrap">
          <Typography component="span" variant={compact ? 'h6' : 'h4'} fontWeight={900}>
            {summary.rating.toFixed(1)}
          </Typography>
          <Typography component="span" aria-label={summary.rating + ' de 5'}>★ / 5</Typography>
          <Typography variant="body2" color="text.secondary">
            {reviewCount} {reviewCount === 1 ? 'evaluación verificada' : 'evaluaciones verificadas'}
          </Typography>
        </Stack>
      )}
      {!compact && summary.confidence && (
        <Chip
          size="small"
          variant="outlined"
          label={confidenceLabel[summary.confidence]}
          sx={{ alignSelf: 'flex-start' }}
        />
      )}
      {!compact && summary.objectiveSignals?.identityVerified && (
        <Chip size="small" color="success" label="Identidad verificada" sx={{ alignSelf: 'flex-start' }} />
      )}
      {!compact && (summary.dimensions?.length ?? 0) > 0 && (
        <Stack spacing={1}>
          {summary.dimensions?.map((dimension) => (
            <Box key={dimension.code}>
              <Stack direction="row" justifyContent="space-between" gap={1}>
                <Typography variant="body2">{dimensionLabel[dimension.code] ?? dimension.code}</Typography>
                <Typography variant="body2" fontWeight={700}>
                  {dimension.average?.toFixed(1) ?? '—'} / 5
                </Typography>
              </Stack>
              <LinearProgress
                variant="determinate"
                value={((dimension.average ?? 0) / 5) * 100}
                aria-label={dimensionLabel[dimension.code] ?? dimension.code}
                sx={{ mt: 0.5, height: 6, borderRadius: 3 }}
              />
            </Box>
          ))}
        </Stack>
      )}
      {!compact && (
        <Button href="/reputacion/como-se-calcula" size="small" sx={{ alignSelf: 'flex-start' }}>
          Cómo se calcula
        </Button>
      )}
    </Stack>
  );
}

export function ArtistMerchStores({ artistPartyId }: { artistPartyId: number }) {
  const query = useQuery({
    queryKey: ['artist-merch-stores', artistPartyId],
    queryFn: () => MerchReputation.artistStores(artistPartyId),
    retry: false,
  });

  if (query.isLoading) {
    return (
      <Stack direction="row" spacing={1} alignItems="center" role="status">
        <CircularProgress size={18} />
        <Typography variant="body2">Cargando tiendas…</Typography>
      </Stack>
    );
  }
  if (query.isError) {
    if (query.error instanceof ApiError && query.error.status === 404) return null;
    return (
      <Alert
        severity="info"
        action={<Button color="inherit" size="small" onClick={() => void query.refetch()}>Reintentar</Button>}
      >
        Las tiendas y su reputación comercial no están disponibles ahora.
      </Alert>
    );
  }
  if (!query.data?.length) return null;

  return (
    <Stack spacing={1.5} component="section" aria-labelledby="artist-merch-heading">
      <Typography id="artist-merch-heading" component="h2" variant="h6" fontWeight={800}>
        Tiendas de merch
      </Typography>
      <Alert severity="info">
        Estas notas describen la experiencia comercial de cada tienda. No miden la calidad artística,
        popularidad ni reputación profesional del artista.
      </Alert>
      <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))' }, gap: 1.5 }}>
        {query.data.map((store) => (
          <Box
            key={store.id ?? store.storeId}
            component="article"
            sx={{ border: 1, borderColor: 'divider', borderRadius: 2, p: 2 }}
          >
            <Typography component="h3" fontWeight={800}>{store.name ?? store.storeName}</Typography>
            <MerchReputationSummary summary={{ ...store, subjectKind: 'store' }} compact />
          </Box>
        ))}
      </Box>
    </Stack>
  );
}
