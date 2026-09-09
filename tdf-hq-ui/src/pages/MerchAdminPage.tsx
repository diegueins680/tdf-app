import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Chip, CircularProgress, Paper, Stack, TextField, Typography } from '@mui/material';
import { useTranslation } from 'react-i18next';
import { Merch, type MerchIssueTriageRequest } from '../api/merch';
import { merchLanguage, merchStatusLabel } from '../utils/merch';

const optionalTrimmed = (value?: string) => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  return trimmed;
};

export default function MerchAdminPage() {
  const { i18n } = useTranslation();
  const language = merchLanguage(i18n.resolvedLanguage);
  const client = useQueryClient();
  const [notes, setNotes] = useState<Record<string, string>>({});
  const [issueResponses, setIssueResponses] = useState<Record<string, string>>({});
  const [issueNotes, setIssueNotes] = useState<Record<string, string>>({});
  const stores = useQuery({ queryKey: ['merch-admin-stores'], queryFn: () => Merch.adminStores(), retry: false });
  const products = useQuery({ queryKey: ['merch-admin-products'], queryFn: () => Merch.adminProducts('pending_review'), retry: false });
  const issues = useQuery({ queryKey: ['merch-admin-issues'], queryFn: () => Merch.adminIssues(), retry: false });
  const storeReview = useMutation({
    mutationFn: ({ id, decision }: { id: string; decision: 'approve' | 'reject' | 'suspend' | 'reactivate' }) => Merch.reviewStore(id, {
      decision,
      reviewerNotes: notes[id]?.trim() ? notes[id].trim() : (decision === 'approve' ? 'Aprobado para piloto cerrado.' : 'Revisión administrativa.'),
      commissionBps: decision === 'approve' ? 0 : null,
      commissionReason: decision === 'approve' ? 'Banda piloto: comisión temporal de 0%, sujeta a revisión.' : null,
    }),
    onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-admin-stores'] }),
  });
  const productReview = useMutation({
    mutationFn: ({ id, status }: { id: string; status: 'published' | 'rejected' }) => Merch.reviewProduct(id, status, status === 'rejected' ? notes[id] : undefined),
    onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-admin-products'] }),
  });
  const issueReview = useMutation({
    mutationFn: ({ id, status }: { id: string; status: MerchIssueTriageRequest['status'] }) => Merch.updateAdminIssue(id, {
      status,
      publicResponse: optionalTrimmed(issueResponses[id]),
      internalNotes: optionalTrimmed(issueNotes[id]),
    }),
    onSuccess: () => void client.invalidateQueries({ queryKey: ['merch-admin-issues'] }),
  });

  if (stores.isLoading || products.isLoading || issues.isLoading) {
    return <Box py={8} textAlign="center"><CircularProgress aria-label={language === 'en' ? 'Loading merch review' : 'Cargando revisión de merch'} /></Box>;
  }
  if (stores.isError || products.isError || issues.isError) {
    return <Box py={4}><Alert severity="error">{language === 'en' ? 'The moderation queues could not be loaded, or you lack administrator permission.' : 'No se pudieron cargar las colas de moderación o no tienes permiso administrativo.'}</Alert></Box>;
  }

  return <Box component="main" py={4}>
    <Stack spacing={4}>
      <Box>
        <Typography component="h1" variant="h3" fontWeight={900}>{language === 'en' ? 'Merch pilot review' : 'Revisión del piloto de merch'}</Typography>
        <Typography color="text.secondary">{language === 'en' ? 'Approval never enables payment providers or production flags.' : 'Aprobar no habilita proveedores de pago ni flags de producción.'}</Typography>
      </Box>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Seller applications' : 'Solicitudes de vendedores'}</Typography>
        <Stack spacing={2}>{stores.data?.map((store) => <Paper key={store.id} variant="outlined" sx={{ p: 2 }}>
          <Stack spacing={1.5}>
            <Stack direction="row" spacing={1} alignItems="center"><Typography fontWeight={800}>{store.displayName}</Typography><Chip size="small" label={merchStatusLabel(store.applicationStatus ?? '', language)} /><Chip size="small" label={merchStatusLabel(store.operationalStatus ?? '', language)} /></Stack>
            <Typography variant="body2">@{store.slug}</Typography>
            <TextField multiline minRows={2} label={language === 'en' ? 'Audit note' : 'Nota de auditoría'} value={notes[store.id] ?? ''} onChange={(event) => setNotes({ ...notes, [store.id]: event.target.value })} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              {store.applicationStatus === 'requested' && <><Button variant="contained" onClick={() => storeReview.mutate({ id: store.id, decision: 'approve' })}>{language === 'en' ? 'Approve pilot at 0%' : 'Aprobar piloto al 0%'}</Button><Button color="error" onClick={() => storeReview.mutate({ id: store.id, decision: 'reject' })}>{language === 'en' ? 'Reject' : 'Rechazar'}</Button></>}
              {store.operationalStatus === 'active' && <Button color="error" onClick={() => storeReview.mutate({ id: store.id, decision: 'suspend' })}>{language === 'en' ? 'Suspend safely' : 'Suspender de forma segura'}</Button>}
              {store.operationalStatus === 'suspended' && <Button onClick={() => storeReview.mutate({ id: store.id, decision: 'reactivate' })}>{language === 'en' ? 'Reactivate' : 'Reactivar'}</Button>}
            </Stack>
          </Stack>
        </Paper>)}{stores.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No applications.' : 'No hay solicitudes.'}</Typography>}</Stack>
      </Paper>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={2}>{language === 'en' ? 'Products awaiting review' : 'Productos pendientes de revisión'}</Typography>
        <Stack spacing={2}>{products.data?.map((product) => <Paper key={product.id} variant="outlined" sx={{ p: 2 }}>
          <Stack spacing={1.5}>
            <Typography fontWeight={800}>{product.name}</Typography>
            <Typography variant="body2">{product.storeName} · {product.category}</Typography>
            <TextField multiline minRows={2} label={language === 'en' ? 'Rejection reason (required to reject)' : 'Motivo (obligatorio para rechazar)'} value={notes[product.id] ?? ''} onChange={(event) => setNotes({ ...notes, [product.id]: event.target.value })} />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}><Button variant="contained" onClick={() => productReview.mutate({ id: product.id, status: 'published' })}>{language === 'en' ? 'Publish' : 'Publicar'}</Button><Button color="error" disabled={(notes[product.id]?.trim().length ?? 0) < 5} onClick={() => productReview.mutate({ id: product.id, status: 'rejected' })}>{language === 'en' ? 'Reject' : 'Rechazar'}</Button></Stack>
          </Stack>
        </Paper>)}{products.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No products awaiting review.' : 'No hay productos pendientes.'}</Typography>}</Stack>
      </Paper>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
        <Typography component="h2" variant="h5" fontWeight={800} mb={1}>{language === 'en' ? 'Support and financial review' : 'Soporte y revisión financiera'}</Typography>
        <Alert severity="warning" sx={{ mb: 2 }}>{language === 'en' ? 'Closing a case records a resolution only. It does not execute or imply a refund, dispute outcome, or seller settlement.' : 'Cerrar un caso solo registra una resolución. No ejecuta ni implica un reembolso, resultado de disputa o liquidación al vendedor.'}</Alert>
        <Stack spacing={2}>{issues.data?.map((item) => {
          const response = issueResponses[item.id] ?? '';
          const terminal = ['resolved', 'rejected', 'cancelled'].includes(item.status);
          return <Paper key={item.id} variant="outlined" sx={{ p: 2 }}>
            <Stack spacing={1.5}>
              <Box><Typography fontWeight={800}>{item.storeName} · {item.orderNumber}</Typography><Typography variant="body2">{merchStatusLabel(item.issueType, language)} · {merchStatusLabel(item.status, language)}</Typography></Box>
              <Typography>{item.message}</Typography>
              {item.resolution && <Alert severity="success">{item.resolution}</Alert>}
              {!terminal && <>
                <TextField multiline minRows={2} inputProps={{ maxLength: 5000 }} label={language === 'en' ? 'Public response to buyer' : 'Respuesta pública para el comprador'} value={response} onChange={(event) => setIssueResponses({ ...issueResponses, [item.id]: event.target.value })} />
                <TextField multiline minRows={2} inputProps={{ maxLength: 5000 }} label={language === 'en' ? 'Internal note (never shown to buyer)' : 'Nota interna (nunca visible al comprador)'} value={issueNotes[item.id] ?? ''} onChange={(event) => setIssueNotes({ ...issueNotes, [item.id]: event.target.value })} />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  {item.status !== 'staff_review' && <Button onClick={() => issueReview.mutate({ id: item.id, status: 'staff_review' })}>{language === 'en' ? 'Start staff review' : 'Iniciar revisión TDF'}</Button>}
                  <Button disabled={response.trim().length < 1} onClick={() => issueReview.mutate({ id: item.id, status: 'awaiting_buyer' })}>{language === 'en' ? 'Ask buyer' : 'Consultar al comprador'}</Button>
                  <Button disabled={response.trim().length < 10} onClick={() => issueReview.mutate({ id: item.id, status: 'resolved' })}>{language === 'en' ? 'Record resolved' : 'Registrar resolución'}</Button>
                  <Button color="error" disabled={response.trim().length < 10} onClick={() => issueReview.mutate({ id: item.id, status: 'rejected' })}>{language === 'en' ? 'Reject request' : 'Rechazar solicitud'}</Button>
                </Stack>
              </>}
            </Stack>
          </Paper>;
        })}{issues.data?.length === 0 && <Typography color="text.secondary">{language === 'en' ? 'No support cases.' : 'No hay casos de soporte.'}</Typography>}</Stack>
      </Paper>

      {(storeReview.isError || productReview.isError || issueReview.isError) && <Alert severity="error">{language === 'en' ? 'The review could not be saved. Check state, separation of duties, and permissions.' : 'No se pudo guardar la revisión. Comprueba el estado, la separación de funciones y los permisos.'}</Alert>}
    </Stack>
  </Box>;
}
