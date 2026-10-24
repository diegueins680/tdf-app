import { Alert, Box, Button, Dialog, DialogActions, DialogContent, DialogTitle, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useEffect, useState } from 'react';

import { SocialEventsAPI, type TicketManualPaymentDTO } from '../../api/socialEvents';

export default function TicketPaymentReceiptDialog({ eventId, payment, onClose }: {
  eventId: string;
  payment: TicketManualPaymentDTO;
  onClose: () => void;
}) {
  const receipt = useQuery({
    queryKey: ['event-manual-payment-receipt', eventId, payment.tmpOrderId],
    queryFn: () => SocialEventsAPI.getManualPaymentReceipt(eventId, payment.tmpOrderId),
    enabled: !!payment.tmpHasReceipt,
    gcTime: 0,
    retry: false,
  });
  const [preview, setPreview] = useState<{ url: string; mime: string }>();
  const [previewError, setPreviewError] = useState(false);

  useEffect(() => {
    if (!receipt.data) return;
    let url: string | undefined;
    try {
      if (!['image/jpeg', 'image/png', 'application/pdf'].includes(receipt.data.contentType)) throw new Error('Unsupported receipt');
      const bytes = Uint8Array.from(atob(receipt.data.base64), (character) => character.charCodeAt(0));
      url = URL.createObjectURL(new Blob([bytes], { type: receipt.data.contentType }));
      setPreview({ url, mime: receipt.data.contentType });
      setPreviewError(false);
    } catch {
      setPreviewError(true);
    }
    return () => { if (url) URL.revokeObjectURL(url); };
  }, [receipt.data]);

  return (
    <Dialog open onClose={onClose} fullWidth maxWidth="md" aria-labelledby="payment-receipt-title">
      <DialogTitle id="payment-receipt-title">Comprobante · {payment.tmpPaymentReference}</DialogTitle>
      <DialogContent>
        <Stack spacing={2}>
          {payment.tmpCustomerReference && <Typography>Número de comprobante: {payment.tmpCustomerReference}</Typography>}
          {!payment.tmpHasReceipt && <Alert severity="info">El comprador solo reportó el número de comprobante. No adjuntó una imagen ni un PDF.</Alert>}
          {payment.tmpHasReceipt && receipt.isPending && <Typography role="status">Cargando comprobante...</Typography>}
          {(receipt.isError || previewError) && <Alert severity="error">No se pudo abrir el comprobante. Cierra esta ventana e inténtalo de nuevo.</Alert>}
          {preview && (preview.mime === 'application/pdf'
            ? <Box component="iframe" title="Comprobante de transferencia" src={preview.url} sandbox="allow-same-origin" sx={{ width: '100%', height: '65vh', border: 0 }} />
            : <Box component="img" src={preview.url} alt={`Comprobante de transferencia ${payment.tmpCustomerReference ?? payment.tmpPaymentReference}`} sx={{ width: '100%', maxHeight: '70vh', objectFit: 'contain' }} />)}
          {preview && <Button component="a" href={preview.url} download={`comprobante-${payment.tmpPaymentReference}.${preview.mime === 'application/pdf' ? 'pdf' : preview.mime === 'image/png' ? 'png' : 'jpg'}`}>Descargar comprobante</Button>}
        </Stack>
      </DialogContent>
      <DialogActions><Button onClick={onClose}>Cerrar</Button></DialogActions>
    </Dialog>
  );
}
