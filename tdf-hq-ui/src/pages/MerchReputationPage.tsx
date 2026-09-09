import { useQuery } from '@tanstack/react-query';
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
  Stack,
  Typography,
} from '@mui/material';
import { useParams } from 'react-router-dom';
import { MerchReputation } from '../api/merchReputation';
import { MerchReputationSummary } from '../components/merch/MerchReputationSummary';

export default function MerchReputationPage({ kind }: { kind: 'store' | 'product' }) {
  const params = useParams();
  const subjectId = kind === 'store' ? params['storeId'] : params['productId'];
  const summary = useQuery({
    queryKey: ['merch-reputation', kind, subjectId],
    queryFn: () => kind === 'store'
      ? MerchReputation.store(subjectId!)
      : MerchReputation.product(subjectId!),
    enabled: Boolean(subjectId),
    retry: false,
  });
  const reviews = useQuery({
    queryKey: ['merch-reviews', kind, subjectId],
    queryFn: () => MerchReputation.reviews(kind, subjectId!),
    enabled: Boolean(subjectId),
    retry: false,
  });

  if (summary.isLoading) {
    return (
      <Stack direction="row" spacing={1} alignItems="center" role="status">
        <CircularProgress size={20} />Cargando reputación…
      </Stack>
    );
  }
  if (summary.isError || !summary.data) {
    return (
      <Alert
        severity="info"
        action={<Button color="inherit" onClick={() => void summary.refetch()}>Reintentar</Button>}
      >
        Esta reputación no está disponible.
      </Alert>
    );
  }

  return (
    <Box sx={{ maxWidth: 880, mx: 'auto', py: 3 }}>
      <Stack spacing={2.5}>
        <Box>
          <Typography component="h1" variant="h4" fontWeight={900}>
            {summary.data.storeName ?? summary.data.productName ?? summary.data.name}
          </Typography>
          {kind === 'store' && (
            <Typography color="text.secondary">
              Reputación comercial, independiente de cualquier reputación artística o profesional.
            </Typography>
          )}
        </Box>
        <Card variant="outlined"><CardContent><MerchReputationSummary summary={summary.data} /></CardContent></Card>
        <Divider />
        <Typography component="h2" variant="h5" fontWeight={800}>Evaluaciones verificadas</Typography>
        {reviews.isLoading && <CircularProgress size={20} aria-label="Cargando evaluaciones" />}
        {reviews.isError && (
          <Alert severity="error" action={<Button color="inherit" onClick={() => void reviews.refetch()}>Reintentar</Button>}>
            No pudimos cargar las evaluaciones.
          </Alert>
        )}
        {reviews.data?.items.length === 0 && <Alert severity="info">Sin evaluaciones todavía.</Alert>}
        {reviews.data?.items.map((review) => (
          <Card variant="outlined" component="article" key={review.id}>
            <CardContent>
              <Stack spacing={1.25}>
                <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                  <Avatar src={review.author.avatarUrl ?? undefined} alt="">
                    {review.author.name.slice(0, 1).toUpperCase()}
                  </Avatar>
                  <Typography fontWeight={700}>{review.author.name}</Typography>
                  <Chip size="small" color="success" label="Compra verificada" />
                  <Typography aria-label={review.rating + ' de 5'}>{review.rating} ★</Typography>
                </Stack>
                {review.comment && <Typography sx={{ whiteSpace: 'pre-wrap' }}>{review.comment}</Typography>}
                {review.images.map((image) => (
                  <Box
                    key={image.assetId}
                    component="img"
                    src={image.url}
                    alt={image.altText}
                    loading="lazy"
                    sx={{ maxWidth: '100%', maxHeight: 320, objectFit: 'contain', borderRadius: 1 }}
                  />
                ))}
                {review.sellerResponse && (
                  <Box sx={{ borderLeft: 3, borderColor: 'primary.main', pl: 2 }}>
                    <Typography variant="overline" fontWeight={800}>Respuesta de la tienda</Typography>
                    <Typography>{review.sellerResponse.body}</Typography>
                  </Box>
                )}
              </Stack>
            </CardContent>
          </Card>
        ))}
      </Stack>
    </Box>
  );
}
