import { Alert, Box, Chip, CircularProgress, LinearProgress, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { Reputation } from '../../api/reputation';

interface Props {
  partyId: number;
}

const LOADING_INDICATOR_SIZE_PX = 20;
const REPUTATION_HEADING_FONT_WEIGHT = 800;
const REPUTATION_SCORE_FONT_WEIGHT = 900;
const CATEGORY_SCORE_FONT_WEIGHT = 700;

/** Public-only presentation. It deliberately does not accept reviewer data. */
export default function PublicReputationSummary({ partyId }: Props) {
  const query = useQuery({
    queryKey: ['public-reputation', partyId],
    queryFn: () => Reputation.getPublic(partyId),
    enabled: Number.isSafeInteger(partyId) && partyId > 0,
  });

  const loading = query.isLoading;
  if (loading) {
    return (
      <Box role="status" aria-live="polite" aria-busy="true" sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
        <CircularProgress size={LOADING_INDICATOR_SIZE_PX} aria-hidden="true" />
        <Typography variant="body2" color="text.secondary">Cargando reputación…</Typography>
      </Box>
    );
  }

  if (query.isError || !query.data) return null;
  const reputation = query.data;
  const empty = reputation.categories.length === 0;
  if (reputation.status === 'forming') {
    return <Alert severity="info">Reputación en formación. Aún no hay suficientes interacciones verificadas para mostrar una puntuación pública.</Alert>;
  }

  return (
    <Box component="section" aria-labelledby="public-reputation-heading" sx={{ border: 1, borderColor: 'divider', borderRadius: 3, p: 2.5 }}>
      <Stack direction="row" justifyContent="space-between" alignItems="baseline" gap={2} flexWrap="wrap">
        <Box>
          <Typography id="public-reputation-heading" variant="h6" fontWeight={REPUTATION_HEADING_FONT_WEIGHT}>Reputación verificada</Typography>
          <Typography variant="body2" color="text.secondary">Agregada de interacciones verificadas; no refleja preferencias personales.</Typography>
        </Box>
        <Typography variant="h4" fontWeight={REPUTATION_SCORE_FONT_WEIGHT}>{Number(reputation.score).toFixed(0)}<Typography component="span" variant="body1">/100</Typography></Typography>
      </Stack>
      <Stack direction="row" spacing={1} sx={{ mt: 1.5 }} flexWrap="wrap" useFlexGap>
        <Chip size="small" label={`${reputation.verifiedInteractions} interacciones verificadas`} />
        <Chip size="small" variant="outlined" label={`Confianza ${reputation.confidence}`} />
      </Stack>
      <Stack spacing={1.25} sx={{ mt: 2 }}>
        {empty ? (
          <Typography variant="body2" color="text.secondary">
            Aún no hay categorías con suficientes interacciones verificadas.
          </Typography>
        ) : reputation.categories.map((category) => (
          <Box key={category.slug}>
            <Stack direction="row" justifyContent="space-between" gap={1}>
              <Typography variant="body2">{category.slug.replace(/-/g, ' ')}</Typography>
              <Typography variant="body2" fontWeight={CATEGORY_SCORE_FONT_WEIGHT}>{Number(category.score).toFixed(0)}</Typography>
            </Stack>
            <LinearProgress
              variant="determinate"
              value={Number(category.score)}
              aria-label={`${category.slug}: ${Number(category.score).toFixed(0)} de 100`}
              sx={{ mt: 0.5, height: 7, borderRadius: 5 }}
            />
          </Box>
        ))}
      </Stack>
    </Box>
  );
}
