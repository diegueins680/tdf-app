import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Card, CardContent, Stack, TextField, Typography } from '@mui/material';
import { useParams } from 'react-router-dom';
import { MerchReputation } from '../api/merchReputation';

interface SellerReview {
  reviewId: string;
  orderId: string;
  status: string;
  rating: number;
  comment?: string | null;
}

const sellerReviews = (value: Record<string, unknown> | undefined): SellerReview[] => {
  if (!value || !Array.isArray(value['reviews'])) return [];
  return value['reviews'].filter((review): review is SellerReview => (
    typeof review === 'object' && review !== null
    && typeof (review as SellerReview).reviewId === 'string'
    && typeof (review as SellerReview).orderId === 'string'
    && typeof (review as SellerReview).rating === 'number'
  ));
};

export default function MerchSellerReputationPage() {
  const { storeId = '' } = useParams();
  const queryClient = useQueryClient();
  const [replyingTo, setReplyingTo] = useState<string | null>(null);
  const [body, setBody] = useState('');
  const query = useQuery({
    queryKey: ['seller-merch-reputation', storeId],
    queryFn: () => MerchReputation.sellerStore(storeId),
    enabled: Boolean(storeId),
    retry: false,
  });
  const respond = useMutation({
    mutationFn: (reviewId: string) => MerchReputation.respond(reviewId, body, 0),
    onSuccess: async () => {
      setReplyingTo(null);
      setBody('');
      await queryClient.invalidateQueries({ queryKey: ['seller-merch-reputation', storeId] });
    },
  });

  if (query.isError) {
    return <Alert severity="error">No tienes permiso para ver esta tienda o la función está deshabilitada.</Alert>;
  }

  const reviews = sellerReviews(query.data);
  return (
    <Box sx={{ maxWidth: 900, mx: 'auto' }}>
      <Stack spacing={2}>
        <Typography component="h1" variant="h4" fontWeight={900}>Reputación comercial</Typography>
        <Alert severity="info">
          La orden se muestra aquí solo para que el equipo autorizado relacione la experiencia.
          Nunca se publica. Una crítica negativa no activa sanciones financieras automáticas.
        </Alert>
        {query.isLoading && <Typography role="status">Cargando…</Typography>}
        {reviews.length === 0 && !query.isLoading && <Alert severity="info">Sin evaluaciones todavía.</Alert>}
        {reviews.map((review) => (
          <Card variant="outlined" key={review.reviewId}>
            <CardContent>
              <Stack spacing={1}>
                <Typography fontWeight={800}>{review.rating} ★ · Compra verificada</Typography>
                <Typography variant="caption">Orden privada: {review.orderId}</Typography>
                {review.comment && <Typography>{review.comment}</Typography>}
                {replyingTo === review.reviewId ? (
                  <Stack spacing={1}>
                    <TextField
                      label="Respuesta pública"
                      multiline
                      minRows={3}
                      value={body}
                      onChange={(event) => setBody(event.target.value)}
                      inputProps={{ maxLength: 2000 }}
                    />
                    {respond.isError && <Alert severity="error">No se pudo guardar la respuesta.</Alert>}
                    <Stack direction="row" gap={1}>
                      <Button
                        variant="contained"
                        disabled={body.trim().length < 2 || respond.isPending}
                        onClick={() => respond.mutate(review.reviewId)}
                      >
                        Publicar respuesta
                      </Button>
                      <Button onClick={() => setReplyingTo(null)}>Cancelar</Button>
                    </Stack>
                  </Stack>
                ) : (
                  <Button sx={{ alignSelf: 'flex-start' }} onClick={() => setReplyingTo(review.reviewId)}>
                    Responder
                  </Button>
                )}
              </Stack>
            </CardContent>
          </Card>
        ))}
      </Stack>
    </Box>
  );
}
