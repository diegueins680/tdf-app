import { Alert, Box, Button, Stack, Typography } from '@mui/material';
import { useEffect, useRef, useState } from 'react';
import QRCode from 'qrcode';

/** The credential comes from authenticated issuance; a rendered QR is not proof of validity. */
export default function TicketCredentialQR({ code, english = false }: { code: string; english?: boolean }) {
  const canvas = useRef<HTMLCanvasElement>(null);
  const [renderedCode, setRenderedCode] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);
  useEffect(() => {
    let active = true;
    setRenderedCode(null);
    setFailed(false);
    const element = canvas.current;
    if (!element) return;
    void QRCode.toCanvas(element, code, {
      width: 280, margin: 4, errorCorrectionLevel: 'M',
      color: { dark: '#000000', light: '#ffffff' },
    }).then(() => { if (active) setRenderedCode(code); })
      .catch(() => { if (active) setFailed(true); });
    return () => { active = false; };
  }, [code]);
  const ready = renderedCode === code;
  return <Stack spacing={1} alignItems="center">
    <Box component="canvas" ref={canvas} role="img"
      aria-label={english ? 'Private admission QR code' : 'Código QR privado de acceso'}
      sx={{ maxWidth: '100%', height: 'auto', display: ready ? 'block' : 'none' }} />
    {failed && <Alert severity="warning">{english
      ? 'The QR could not be rendered. Staff can enter the code below.'
      : 'No se pudo generar el QR. El personal puede ingresar el código de abajo.'}</Alert>}
    <Typography component="code" sx={{ overflowWrap: 'anywhere' }}>{code}</Typography>
    <Button disabled={!ready} onClick={() => {
      if (!ready || !canvas.current) return;
      const link = document.createElement('a');
      link.download = 'tdf-ticket.png';
      link.href = canvas.current.toDataURL('image/png');
      link.click();
    }}>{english ? 'Save QR for entry' : 'Guardar QR para el acceso'}</Button>
    <Typography variant="caption" color="text.secondary">{english
      ? 'Keep this QR private. Entry is confirmed by event staff; each ticket can be used once.'
      : 'Mantén este QR privado. El personal del evento confirma el acceso; cada entrada se utiliza una sola vez.'}</Typography>
  </Stack>;
}
