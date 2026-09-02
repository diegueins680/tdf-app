import { Alert, Box, Chip, CircularProgress, LinearProgress, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { Reputation } from '../../api/reputation';

type Props = { partyId: number };

/** Public-only presentation. It deliberately does not accept reviewer data. */
export default function PublicReputationSummary({ partyId }: Props) {
  const query = useQuery({
    queryKey: ['public-reputation', partyId],
    queryFn: () => Reputation.getPublic(partyId),
    enabled: Number.isSafeInteger(partyId) && partyId > 0,
  });

  if (query.isLoading) return <CircularProgress size={20} aria-label="Cargando reputación" />;
  if (query.isError || !query.data) return null;
  const reputation = query.data;
  if (reputation.status === 'forming') {
    return <Alert severity="info">Reputación en formación. Aún no hay suficientes interacciones verificadas para mostrar una puntuación pública.</Alert>;
  }

  return (
    <Box component="section" aria-labelledby="public-reputation-heading" sx={{ border: 1, borderColor: 'divider', borderRadius: 3, p: 2.5 }}>
      <Stack direction="row" justifyContent="space-between" alignItems="baseline" gap={2} flexWrap="wrap">
        <Box>
          <Typography id="public-reputation-heading" variant="h6" fontWeight={800}>Reputación verificada</Typography>
          <Typography variant="body2" color="text.secondary">Agregada de interacciones verificadas; no refleja preferencias personales.</Typography>
        </Box>
        <Typography variant="h4" fontWeight={900}>{Number(reputation.score).toFixed(0)}<Typography component="span" variant="body1">/100</Typography></Typography>
      </Stack>
      <Stack direction="row" spacing={1} sx={{ mt: 1.5 }} flexWrap="wrap" useFlexGap>
        <Chip size="small" label={`${reputation.verifiedInteractions} interacciones verificadas`} />
        <Chip size="small" variant="outlined" label={`Confianza ${reputation.confidence}`} />
      </Stack>
      <Stack spacing={1.25} sx={{ mt: 2 }}>
        {reputation.categories.map((category) => (
          <Box key={category.slug}>
            <Stack direction="row" justifyContent="space-between" gap={1}>
              <Typography variant="body2">{category.slug.replaceAll('-', ' ')}</Typography>
              <Typography variant="body2" fontWeight={700}>{Number(category.score).toFixed(0)}</Typography>
            </Stack>
            <LinearProgress variant="determinate" value={Number(category.score)} aria-label={`${category.slug}: ${Number(category.score).toFixed(0)} de 100`} sx={{ mt: 0.5, height: 7, borderRadius: 5 }} />
          </Box>
        ))}
      </Stack>
    </Box>
  );
}
