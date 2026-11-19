import { useRef, useState, type FormEvent } from 'react';
import { Alert, Box, Button, Container, Stack, TextField, Typography } from '@mui/material';
import { useLocation } from 'react-router-dom';
import { get, post } from '../api/client';
import { useQuery } from '@tanstack/react-query';

export default function EventDemoPage() {
  const { search } = useLocation();
  const availability = useQuery({
    queryKey: ['event-demo-intake-availability'],
    queryFn: ({signal}) => get<{available?: boolean}>('/public/event-demo-requests', {signal}),
    retry: false,
  });
  const enabled = availability.data?.available === true && !availability.isError;
  const lastSubmission = useRef<{ body: string; key: string } | null>(null);
  const pending = useRef(false);
  const [state, setState] = useState<'idle' | 'sending' | 'received' | 'unconfirmed'>('idle');
  async function submit(e: FormEvent<HTMLFormElement>) {
    e.preventDefault();
    if (!enabled || pending.current || state === 'received') return;
    const data = new FormData(e.currentTarget);
    const read = (key: string) => {
      const value = data.get(key);
      return typeof value === 'string' ? value.trim() : '';
    };
    const q = new URLSearchParams(search);
    const source = q.get('utm_source');
    const piece = q.get('utm_content');
    const validAttribution = ['instagram', 'tiktok'].includes(source ?? '')
      && ['organizador', 'publico'].includes(piece ?? '')
      && q.get('utm_campaign') === 'tdf_demo_eventos_quito'
      && q.get('utm_medium') === 'organic_social';
    const body = {
      name: read('name'), email: read('email'),
      message: [
        `Evento: ${read('event')}`,
        read('timing') && `Fecha aproximada: ${read('timing')}`,
        read('capacity') && `Aforo: ${read('capacity')}`,
      ].filter(Boolean).join('\n'),
      ...(validAttribution ? {channel: `${source}-${piece}`} : {}),
    };
    const serialized = JSON.stringify(body);
    if (lastSubmission.current?.body !== serialized) {
      lastSubmission.current = { body: serialized, key: crypto.randomUUID() };
    }
    pending.current = true;
    setState('sending');
    try {
      const result = await post<{status?: string}>('/public/event-demo-requests', body, {
        headers: { 'Idempotency-Key': lastSubmission.current.key },
      });
      setState(result?.status === 'requested' ? 'received' : 'unconfirmed');
    } catch {
      setState('unconfirmed');
    } finally {
      pending.current = false;
    }
  }
  if (!enabled) return (
    <Container maxWidth="sm" sx={{py: 6}}>
      <Typography component="h1" variant="h4">Demos de eventos TDF</Typography>
      <Typography>{availability.isPending ? 'Consultando disponibilidad…' : 'Las solicitudes de demos no están disponibles por ahora.'}</Typography>
    </Container>
  );
  return (
    <Container maxWidth="sm" sx={{ py: { xs: 5, md: 9 } }}>
      <Stack spacing={3}>
        <Typography variant="h3" component="h1">Tu próximo evento, desde la publicación hasta la puerta</Typography>
        <Typography>Si organizas eventos en Quito, recorre con TDF cómo se publica la ficha y se valida una entrada digital. Cuéntanos qué estás preparando.</Typography>
        {state === 'received' ? (
          <Alert severity="success">Solicitud recibida. Diego te escribirá para coordinar la hora. Aún no hay una cita reservada.</Alert>
        ) : (
          <Box component="form" onSubmit={(e: FormEvent<HTMLFormElement>) => { void submit(e); }}>
            <Stack spacing={2}>
              <TextField name="name" label="Nombre" required slotProps={{ htmlInput: { maxLength: 120 } }} />
              <TextField name="email" label="Correo" type="email" required slotProps={{ htmlInput: { maxLength: 254 } }} />
              <TextField name="event" label="¿Qué evento estás preparando?" required slotProps={{ htmlInput: { maxLength: 160 } }} />
              <TextField name="timing" label="Fecha aproximada (opcional)" slotProps={{ htmlInput: { maxLength: 120 } }} />
              <TextField name="capacity" label="Aforo estimado (opcional)" type="number" slotProps={{ htmlInput: { min: 1, max: 99999 } }} />
              {state === 'unconfirmed' && <Alert severity="error">No pudimos confirmar el envío. Intenta nuevamente.</Alert>}
              <Typography variant="body2">Usaremos tus datos para responder a esta solicitud. Enviar el formulario no reserva una hora.</Typography>
              <Button type="submit" variant="contained" disabled={state === 'sending'}>{state === 'sending' ? 'Enviando…' : 'Agenda una demo con TDF'}</Button>
            </Stack>
          </Box>
        )}
      </Stack>
    </Container>
  );
}
