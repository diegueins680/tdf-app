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
import { useTranslation } from 'react-i18next';
import { MerchReputation, type MerchReputationSummary as Summary } from '../../api/merchReputation';
import { ApiError } from '../../api/client';

export function MerchReputationSummary({ summary, compact = false }: { summary: Summary; compact?: boolean }) {
  const { i18n } = useTranslation();
  const english = (i18n.resolvedLanguage ?? i18n.language ?? 'es').toLowerCase().startsWith('en');
  const confidenceLabel: Record<NonNullable<Summary['confidence']>, string> = english
    ? { new: 'Initial evidence', limited: 'Limited evidence', moderate: 'Moderate evidence', strong: 'Strong evidence' }
    : { new: 'Evidencia inicial', limited: 'Evidencia limitada', moderate: 'Evidencia moderada', strong: 'Evidencia sólida' };
  const dimensionLabel: Record<string, string> = english
    ? {
        description_accuracy: 'Matches the description', product_quality: 'Product quality',
        preparation_dispatch: 'Preparation and dispatch', communication: 'Communication',
        packaging: 'Packaging', problem_resolution: 'Problem resolution',
      }
    : {
        description_accuracy: 'Conforme a la descripción', product_quality: 'Calidad del producto',
        preparation_dispatch: 'Preparación y despacho', communication: 'Comunicación',
        packaging: 'Empaque', problem_resolution: 'Resolución de problemas',
      };
  const reviewCount = summary.verifiedReviewCount ?? summary.verifiedPurchaseReviewCount ?? 0;
  const isNew = summary.state === 'new_store';

  return (
    <Stack
      spacing={compact ? 0.75 : 1.5}
      aria-label={summary.subjectKind === 'product'
        ? (english ? 'Product rating' : 'Valoración del producto')
        : (english ? 'Store commercial reputation' : 'Reputación comercial de la tienda')}
    >
      {summary.subjectKind !== 'product' && (
        <Typography variant="overline" color="text.secondary" fontWeight={800}>
          {english ? 'Commercial reputation' : 'Reputación comercial'}
        </Typography>
      )}
      {isNew ? (
        <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
          <Chip label={english ? 'New store' : 'Tienda nueva'} color="info" size="small" />
          <Typography variant="body2" color="text.secondary">
            {english
              ? 'There are not yet five eligible orders; we do not assign an artificial rating.'
              : 'Aún no hay cinco órdenes evaluables; no asignamos una nota artificial.'}
          </Typography>
        </Stack>
      ) : summary.state === 'unrated' || summary.rating == null ? (
        <Typography variant="body2" color="text.secondary">
          {english ? 'No verified reviews yet.' : 'Sin evaluaciones verificadas todavía.'}
        </Typography>
      ) : (
        <Stack direction="row" spacing={1} alignItems="baseline" flexWrap="wrap">
          <Typography component="span" variant={compact ? 'h6' : 'h4'} fontWeight={900}>
            {summary.rating.toFixed(1)}
          </Typography>
          <Typography component="span" aria-label={`${summary.rating} ${english ? 'out of' : 'de'} 5`}>★ / 5</Typography>
          <Typography variant="body2" color="text.secondary">
            {reviewCount} {english
              ? (reviewCount === 1 ? 'verified review' : 'verified reviews')
              : (reviewCount === 1 ? 'evaluación verificada' : 'evaluaciones verificadas')}
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
        <Chip size="small" color="success" label={english ? 'Verified identity' : 'Identidad verificada'} sx={{ alignSelf: 'flex-start' }} />
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
          {english ? 'How it is calculated' : 'Cómo se calcula'}
        </Button>
      )}
    </Stack>
  );
}

export function ArtistMerchStores({ artistPartyId }: { artistPartyId: number }) {
  const { i18n } = useTranslation();
  const english = (i18n.resolvedLanguage ?? i18n.language ?? 'es').toLowerCase().startsWith('en');
  const query = useQuery({
    queryKey: ['artist-merch-stores', artistPartyId],
    queryFn: () => MerchReputation.artistStores(artistPartyId),
    retry: false,
  });

  if (query.isLoading) {
    return (
      <Stack direction="row" spacing={1} alignItems="center" role="status">
        <CircularProgress size={18} />
        <Typography variant="body2">{english ? 'Loading stores…' : 'Cargando tiendas…'}</Typography>
      </Stack>
    );
  }
  if (query.isError) {
    if (query.error instanceof ApiError && query.error.status === 404) return null;
    return (
      <Alert
        severity="info"
        action={<Button color="inherit" size="small" onClick={() => void query.refetch()}>{english ? 'Retry' : 'Reintentar'}</Button>}
      >
        {english
          ? 'Stores and their commercial reputation are unavailable right now.'
          : 'Las tiendas y su reputación comercial no están disponibles ahora.'}
      </Alert>
    );
  }
  if (!query.data?.length) return null;

  return (
    <Stack spacing={1.5} component="section" aria-labelledby="artist-merch-heading">
      <Typography id="artist-merch-heading" component="h2" variant="h6" fontWeight={800}>
        {english ? 'Merch stores' : 'Tiendas de merch'}
      </Typography>
      <Alert severity="info">
        {english
          ? 'These ratings describe each store’s commercial experience. They do not measure the artist’s quality, popularity, or professional reputation.'
          : 'Estas notas describen la experiencia comercial de cada tienda. No miden la calidad artística, popularidad ni reputación profesional del artista.'}
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
