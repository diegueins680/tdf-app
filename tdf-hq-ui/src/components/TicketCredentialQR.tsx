import { Alert, Box, Button, Stack, Typography } from '@mui/material';
import { useEffect, useRef, useState } from 'react';
import QRCode from 'qrcode';
import type { PublicEventTicket, PublicEventTicketQuote, PublicEventTicketStorefront } from '../api/eventTickets';
import { renderTicketPass, ticketLogo, type TicketPass } from '../utils/ticketPass';

interface Props {
  ticket: PublicEventTicket;
  orderId: number;
  eventId: number;
  event?: PublicEventTicketStorefront;
  date?: string;
  price: string;
  quote: PublicEventTicketQuote;
  english: boolean;
  eventLoading: boolean;
}

/** The credential comes from authenticated issuance. Keep it available while optional artwork loads. */
export default function TicketCredentialQR({ ticket, orderId, eventId, event, date, price, quote, english, eventLoading }: Props) {
  const canvas = useRef<HTMLCanvasElement>(null);
  const [renderedCode, setRenderedCode] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);
  const [saving, setSaving] = useState(false);
  const [saveFailed, setSaveFailed] = useState(false);
  const [failedArt, setFailedArt] = useState<string>();
  const code = ticket.ticketCode;
  useEffect(() => {
    let active = true;
    setRenderedCode(null);
    setFailed(false);
    if (canvas.current) {
      void QRCode.toCanvas(canvas.current, code, {
        scale: 12, margin: 4, errorCorrectionLevel: 'M',
        color: { dark: '#000000', light: '#ffffff' },
      }).then(() => { if (active) setRenderedCode(code); })
        .catch(() => { if (active) setFailed(true); });
    }
    return () => { active = false; };
  }, [code]);

  // Older APIs only expose policy copy on the live storefront.
  const policy = event?.policy;
  const acceptedPolicy = policy?.policyVersion === quote.policyVersion && policy.termsVersion === quote.termsVersion
    ? policy : undefined;
  const version = (id: string) => english ? 'Accepted version ' + id : 'Versión aceptada ' + id;
  const pass: TicketPass = {
    title: event?.title ?? (english ? 'Event #' : 'Evento #') + eventId,
    date,
    venue: [event?.venueName, event?.venueAddress].filter(Boolean).join(' · '),
    artwork: event?.imageUrl,
    serial: (english ? 'Ticket #' : 'Entrada #') + ticket.ticketId,
    details: [
      ticket.holderName, quote.tierName, (english ? 'Face value ' : 'Valor nominal ') + price,
      (english ? 'Order #' : 'Orden #') + orderId,
    ].filter((value): value is string => Boolean(value)),
    code,
    instruction: english ? 'Present this QR at the door. Keep it private. Valid for one admission.'
      : 'Presenta este QR al ingresar. Mantenlo privado. Válido para un solo acceso.',
    policies: [
      { label: english ? 'Ticket terms' : 'Términos de la entrada',
        text: quote.termsSummary ?? acceptedPolicy?.termsSummary ?? version(quote.termsVersion) },
      { label: english ? 'Refund policy' : 'Política de reembolso',
        text: quote.refundPolicy ?? acceptedPolicy?.refundPolicy ?? version(quote.policyVersion) },
    ],
  };

  const ready = renderedCode === code;
  async function save() {
    const source = canvas.current;
    if (!ready || !source || saving) return;
    setSaving(true);
    setSaveFailed(false);
    try {
      const url = await renderTicketPass(pass, source);
      if (canvas.current !== source) return;
      const link = document.createElement('a');
      link.download = 'tdf-ticket-' + ticket.ticketId + '.png';
      link.href = url;
      link.click();
    } catch {
      setSaveFailed(true);
    } finally {
      setSaving(false);
    }
  }

  return <Stack spacing={2} alignItems="center" sx={{ minWidth: 0 }}>
    <Box component="article" aria-label={pass.serial} sx={{ width: '100%', maxWidth: 540, borderRadius: 2, overflow: 'hidden',
      bgcolor: '#fbf8ed', color: '#172620', overflowWrap: 'anywhere' }}>
      <Stack direction="row" alignItems="center" justifyContent="space-between" spacing={1}
        sx={{ px: 2, py: 1.5, bgcolor: '#172620', color: '#fbf8ed' }}>
        <Stack direction="row" alignItems="center" spacing={1}>
          <Box component="img" src={ticketLogo} alt="" sx={{ width: 27, height: 27 }} />
          <Typography variant="subtitle2" fontWeight={800}>TDF RECORDS</Typography>
        </Stack>
        <Typography variant="caption" fontFamily="monospace" sx={{ color: '#c9eb85' }}>{pass.serial}</Typography>
      </Stack>
      {pass.artwork && failedArt !== pass.artwork && <Box component="img" src={pass.artwork}
        alt={english ? pass.title + ' artwork' : 'Arte de ' + pass.title} referrerPolicy="no-referrer"
        onError={() => setFailedArt(pass.artwork ?? undefined)}
        sx={{ display: 'block', width: '100%', height: 230, bgcolor: '#172620', objectFit: 'contain' }} />}
      <Box sx={{ borderTop: '4px solid #c9eb85', p: 2.5 }}>
        <Typography component="h3" variant="h4" sx={{ fontFamily: 'Georgia, serif', fontWeight: 700, mb: 1 }}>{pass.title}</Typography>
        {date && <Typography fontWeight={700}>{date}</Typography>}
        {pass.venue && <Typography>{pass.venue}</Typography>}
      </Box>
      <Box sx={{ p: 2, bgcolor: '#fff', borderTop: '1px dashed #8a9d90' }}>
        <Stack direction={{ xs: 'column', sm: 'row' }} alignItems="center" justifyContent="space-between" spacing={1}>
          <Box sx={{ minWidth: 0 }}>
            {pass.details.map((detail, i) => <Typography key={i} fontWeight={i === 0 ? 700 : 400}>{detail}</Typography>)}
            <Typography variant="body2" sx={{ mt: 1 }}>{pass.instruction}</Typography>
          </Box>
          <Box component="canvas" ref={canvas} role="img"
            aria-label={english ? 'Private admission QR code' : 'Código QR privado de acceso'}
            sx={{ width: 202, maxWidth: '100%', height: 'auto', flexShrink: 0, display: ready ? 'block' : 'none' }} />
        </Stack>
        {failed && <Alert severity="warning">{english ? 'Staff can enter the code below.' : 'El personal puede ingresar el código de abajo.'}</Alert>}
        <Typography component="code" variant="body2" sx={{ display: 'block', fontFamily: 'monospace', mt: 1 }}>{code}</Typography>
      </Box>
      <Stack sx={{ p: 2.5 }} spacing={1.5}>
        {pass.policies.map(({ label, text }) => <Box key={label}>
          <Typography component="h4" variant="subtitle2" color="#405e4c" fontWeight={700}>{label}</Typography>
          <Typography variant="body2" sx={{ whiteSpace: 'pre-line' }}>{text}</Typography>
        </Box>)}
      </Stack>
    </Box>
    <Button disabled={!ready || saving || eventLoading} onClick={() => void save()} variant="contained">
      {english ? 'Save ticket' : 'Guardar entrada'}
    </Button>
    {saveFailed && <Alert severity="warning">{english ? 'Could not save your ticket. Try again.' : 'No se pudo guardar la entrada. Inténtalo de nuevo.'}</Alert>}
  </Stack>;
}
