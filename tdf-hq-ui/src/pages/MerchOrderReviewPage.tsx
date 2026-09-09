import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CircularProgress,
  Stack,
  Typography,
} from '@mui/material';
import { useParams } from 'react-router-dom';
import { MerchReputation, type MerchReviewSubmit } from '../api/merchReputation';
import MerchReviewForm from '../components/merch/MerchReviewForm';

type Selection =
  | { kind: 'store'; revision: number }
  | { kind: 'product'; lineId: string; productName: string; revision: number };

export default function MerchOrderReviewPage() {
  const { orderId = '' } = useParams();
  const queryClient = useQueryClient();
  const [selection, setSelection] = useState<Selection | null>(null);
  const [sent, setSent] = useState(false);
  const eligibility = useQuery({
    queryKey: ['merch-review-eligibility', orderId],
    queryFn: () => MerchReputation.eligibility(orderId),
    enabled: Boolean(orderId),
    retry: false,
  });
  const mutation = useMutation({
    mutationFn: (review: MerchReviewSubmit) => {
      if (!selection) throw new Error('Selecciona una evaluación.');
      return selection.kind === 'store'
        ? MerchReputation.submitStore(orderId, review)
        : MerchReputation.submitProduct(orderId, selection.lineId, review);
    },
    onSuccess: async () => {
      setSent(true);
      setSelection(null);
      await queryClient.invalidateQueries({ queryKey: ['merch-review-eligibility', orderId] });
    },
  });

  if (eligibility.isLoading) {
    return <Stack direction="row" gap={1} alignItems="center" role="status"><CircularProgress size={20} />Cargando…</Stack>;
  }
  if (eligibility.isError || !eligibility.data) {
    return (
      <Alert
        severity="error"
        action={<Button color="inherit" onClick={() => void eligibility.refetch()}>Reintentar</Button>}
      >
        No pudimos cargar las evaluaciones de esta orden o no tienes permiso para verla.
      </Alert>
    );
  }

  const data = eligibility.data;
  const cancelled = data.orderState === 'cancelled';
  return (
    <Box sx={{ maxWidth: 760, mx: 'auto', py: 3 }}>
      <Stack spacing={2}>
        <Box>
          <Typography component="h1" variant="h4" fontWeight={900}>Evalúa tu compra</Typography>
          <Typography color="text.secondary">
            Disponible durante 30 días desde la entrega, retiro o resolución de cancelación.
          </Typography>
        </Box>
        {sent && <Alert severity="success">Evaluación enviada. Puedes editarla mientras el periodo siga abierto.</Alert>}
        {data.storeReview.eligible && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={1}>
                <Typography component="h2" variant="h6" fontWeight={800}>Experiencia con la tienda</Typography>
                <Typography variant="body2" color="text.secondary">
                  {cancelled
                    ? 'Esta orden se canceló: solo puedes evaluar comunicación y resolución.'
                    : 'Evalúa atención y logística; esto no cambia la valoración de los productos.'}
                </Typography>
                <Button
                  variant={selection?.kind === 'store' ? 'contained' : 'outlined'}
                  onClick={() => setSelection({ kind: 'store', revision: data.storeReview.currentRevision })}
                >
                  {data.storeReview.state === 'edit_available' ? 'Editar evaluación de tienda' : 'Evaluar tienda'}
                </Button>
              </Stack>
            </CardContent>
          </Card>
        )}
        {data.productLines.map((line) => (
          <Card variant="outlined" key={line.lineId}>
            <CardContent>
              <Stack spacing={1}>
                <Typography component="h2" variant="h6" fontWeight={800}>{line.productName}</Typography>
                <Typography variant="body2" color="text.secondary">
                  {line.eligible
                    ? 'Valora el artículo recibido, separado de la atención y del envío.'
                    : 'Este producto no es evaluable porque la línea no consta como recibida.'}
                </Typography>
                {line.eligible && (
                  <Button
                    variant={selection?.kind === 'product' && selection.lineId === line.lineId ? 'contained' : 'outlined'}
                    onClick={() => setSelection({
                      kind: 'product',
                      lineId: line.lineId,
                      productName: line.productName,
                      revision: line.currentRevision,
                    })}
                  >
                    {line.state === 'edit_available' ? 'Editar valoración' : 'Evaluar producto'}
                  </Button>
                )}
              </Stack>
            </CardContent>
          </Card>
        ))}
        {selection && (
          <Card variant="outlined">
            <CardContent>
              <Typography component="h2" variant="h6" fontWeight={800} gutterBottom>
                {selection.kind === 'store' ? 'Experiencia comercial' : selection.productName}
              </Typography>
              <MerchReviewForm
                kind={selection.kind}
                cancelled={cancelled}
                expectedRevision={selection.revision}
                submitting={mutation.isPending}
                error={mutation.isError ? 'No se guardó. Revisa los datos y vuelve a intentarlo.' : null}
                onSubmit={(review) => mutation.mutate(review)}
              />
            </CardContent>
          </Card>
        )}
        {!data.storeReview.eligible && data.productLines.every((line) => !line.eligible) && (
          <Alert severity="info">El periodo de evaluación expiró o esta orden aún no alcanzó el estado permitido.</Alert>
        )}
      </Stack>
    </Box>
  );
}
