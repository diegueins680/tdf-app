import { useEffect, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, Divider, Stack, TextField, Typography } from '@mui/material';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import QrCode2Icon from '@mui/icons-material/QrCode2';
import * as QRCode from 'qrcode';

const CARDANO_ADDRESS =
  'addr1qx2mdr6n8d0v2y5s99tmdluzvcq6lvpvez0mx55vvpfy6ee4fzjjxl454z8d2f5gd2yualhds75ycvsl3wuar908v0csqksrwy';

export default function DonationPage() {
  const [qrDataUrl, setQrDataUrl] = useState<string | null>(null);
  const [copyMsg, setCopyMsg] = useState<string | null>(null);
  const [qrError, setQrError] = useState<string | null>(null);

  useEffect(() => {
    const buildQr = async () => {
      try {
        const url = await QRCode.toDataURL(CARDANO_ADDRESS, {
          errorCorrectionLevel: 'M',
          width: 320,
          margin: 1,
          color: { dark: '#0f172a', light: '#f8fafc' },
        });
        setQrDataUrl(url);
      } catch (err) {
        const message = err instanceof Error ? err.message : 'No pudimos generar el QR.';
        setQrError(message);
      }
    };
    void buildQr();
  }, []);

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(CARDANO_ADDRESS);
      setCopyMsg('Dirección copiada');
    } catch {
      setCopyMsg('No se pudo copiar, copia manualmente.');
    } finally {
      setTimeout(() => setCopyMsg(null), 2000);
    }
  };

  return (
    <Box sx={{ minHeight: '80vh', display: 'flex', alignItems: 'center', justifyContent: 'center', py: 4 }}>
      <Card sx={{ maxWidth: 720, width: '100%', borderRadius: 3, boxShadow: '0 16px 64px rgba(15,17,24,0.25)' }}>
        <CardContent sx={{ p: { xs: 3, md: 4 } }}>
          <Stack spacing={2.5}>
            <Stack spacing={0.5}>
              <Typography variant="overline" color="text.secondary">
                Donaciones cripto
              </Typography>
              <Typography variant="h4" fontWeight={800}>
                Apoya con Cardano (ADA)
              </Typography>
              <Typography variant="body1" color="text.secondary">
                Usamos una dirección de Cardano para contribuciones anónimas. Envía ADA a la dirección única indicada abajo. Si necesitas un comprobante, compártenos el hash de la transacción.
              </Typography>
            </Stack>

            <Divider />

            <Stack direction={{ xs: 'column', md: 'row' }} spacing={3} alignItems="stretch">
              <Stack spacing={1} flex={1}>
                <Typography variant="subtitle2" color="text.secondary">
                  Dirección
                </Typography>
                <TextField value={CARDANO_ADDRESS} multiline minRows={2} fullWidth InputProps={{ readOnly: true }} />
                <Stack direction="row" spacing={1}>
                  <Button
                    variant="contained"
                    startIcon={<ContentCopyIcon />}
                    onClick={() => {
                      void handleCopy();
                    }}
                  >
                    Copiar dirección
                  </Button>
                  {copyMsg && (
                    <Typography variant="body2" color="text.secondary" sx={{ alignSelf: 'center' }}>
                      {copyMsg}
                    </Typography>
                  )}
                </Stack>
                <Alert severity="info" variant="outlined">
                  Usa red Cardano. No envíes otros activos o monedas. Las donaciones son no reembolsables.
                </Alert>
              </Stack>

              <Stack
                spacing={1}
                alignItems="center"
                justifyContent="center"
                sx={{
                  flexBasis: 320,
                  flexShrink: 0,
                  bgcolor: 'rgba(148,163,184,0.12)',
                  borderRadius: 2,
                  p: 2,
                  border: '1px solid rgba(148,163,184,0.35)',
                }}
              >
                <Typography variant="subtitle2" color="text.secondary">
                  Escanea el QR
                </Typography>
                {qrError && <Alert severity="warning">{qrError}</Alert>}
                {qrDataUrl ? (
                  <Box
                    component="img"
                    src={qrDataUrl}
                    alt="QR de donación Cardano"
                    sx={{ width: '100%', maxWidth: 280, borderRadius: 2, border: '1px solid rgba(15,17,24,0.12)' }}
                  />
                ) : (
                  <Box
                    sx={{
                      width: 180,
                      height: 180,
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center',
                      bgcolor: 'rgba(255,255,255,0.5)',
                      borderRadius: 2,
                      border: '1px dashed rgba(148,163,184,0.7)',
                    }}
                  >
                    <QrCode2Icon fontSize="large" color="disabled" />
                  </Box>
                )}
              </Stack>
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
