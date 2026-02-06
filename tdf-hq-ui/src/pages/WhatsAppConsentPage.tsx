import { useState } from 'react';
import {
  Alert,
  Button,
  Checkbox,
  FormControlLabel,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import {
  WhatsAppConsentAPI,
  type WhatsAppConsentResponse,
  type WhatsAppConsentStatus,
} from '../api/whatsappConsent';

const formatTimestamp = (value?: string | null) => {
  if (!value) return '—';
  try {
    return new Date(value).toLocaleString();
  } catch {
    return value;
  }
};

export default function WhatsAppConsentPage() {
  const [phone, setPhone] = useState('');
  const [name, setName] = useState('');
  const [consentChecked, setConsentChecked] = useState(false);
  const [sendMessage, setSendMessage] = useState(true);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [response, setResponse] = useState<WhatsAppConsentResponse | null>(null);
  const [status, setStatus] = useState<WhatsAppConsentStatus | null>(null);
  const statusName = status?.displayName?.trim();

  const resetFeedback = () => {
    setError(null);
    setResponse(null);
    setStatus(null);
  };

  const handleConsent = async () => {
    resetFeedback();
    if (!consentChecked) {
      setError('Debes marcar el consentimiento para enviar el mensaje.');
      return;
    }
    if (!phone.trim()) {
      setError('Ingresa un número en formato E.164 (ej. +593999999999).');
      return;
    }
    setLoading(true);
    try {
      const trimmedName = name.trim();
      const res = await WhatsAppConsentAPI.createConsent({
        phone: phone.trim(),
        name: trimmedName === '' ? null : trimmedName,
        consent: true,
        source: 'tdf-hq-ui',
        sendMessage,
      });
      setResponse(res);
      setStatus(res.status);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Error inesperado');
    } finally {
      setLoading(false);
    }
  };

  const handleOptOut = async () => {
    resetFeedback();
    if (!phone.trim()) {
      setError('Ingresa el número a dar de baja.');
      return;
    }
    setLoading(true);
    try {
      const res = await WhatsAppConsentAPI.optOut({
        phone: phone.trim(),
        reason: 'Solicitud desde panel',
        sendMessage,
      });
      setResponse(res);
      setStatus(res.status);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Error inesperado');
    } finally {
      setLoading(false);
    }
  };

  const handleLookup = async () => {
    resetFeedback();
    if (!phone.trim()) {
      setError('Ingresa un número para consultar estado.');
      return;
    }
    setLoading(true);
    try {
      const res = await WhatsAppConsentAPI.fetchStatus(phone.trim());
      setStatus(res);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Error inesperado');
    } finally {
      setLoading(false);
    }
  };

  return (
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="h4" fontWeight={800}>
          Consentimiento WhatsApp
        </Typography>
        <Typography variant="body2" color="text.secondary">
          Registra el consentimiento explícito y envía un mensaje de prueba para la revisión de Meta.
        </Typography>
      </Stack>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 2 }}>
        <Stack spacing={2}>
          <TextField
            label="Número WhatsApp (E.164)"
            value={phone}
            onChange={(e) => setPhone(e.target.value)}
            placeholder="+593999999999"
            fullWidth
          />
          <TextField
            label="Nombre (opcional)"
            value={name}
            onChange={(e) => setName(e.target.value)}
            fullWidth
          />
          <FormControlLabel
            control={
              <Checkbox
                checked={consentChecked}
                onChange={(e) => setConsentChecked(e.target.checked)}
              />
            }
            label="Confirmo que el usuario dio su consentimiento explícito para recibir mensajes por WhatsApp."
          />
          <FormControlLabel
            control={
              <Checkbox checked={sendMessage} onChange={(e) => setSendMessage(e.target.checked)} />
            }
            label="Enviar mensaje de confirmación por WhatsApp"
          />
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button variant="contained" onClick={() => void handleConsent()} disabled={loading}>
              Registrar consentimiento
            </Button>
            <Button variant="outlined" onClick={() => void handleOptOut()} disabled={loading}>
              Dar de baja (opt-out)
            </Button>
            <Button variant="text" onClick={() => void handleLookup()} disabled={loading}>
              Consultar estado
            </Button>
          </Stack>
        </Stack>
      </Paper>

      {error && (
        <Alert severity="error">{error}</Alert>
      )}

      {response && (
        <Alert severity={response.messageSent ? 'success' : 'warning'}>
          {response.messageSent
            ? 'Mensaje enviado correctamente.'
            : 'Consentimiento guardado. El mensaje no fue enviado.'}
          {response.message && (
            <Typography variant="body2" sx={{ mt: 1 }}>
              {response.message}
            </Typography>
          )}
        </Alert>
      )}

      {status && (
        <Paper variant="outlined" sx={{ p: 3, borderRadius: 2 }}>
          <Stack spacing={1}>
            <Typography variant="h6">Estado actual</Typography>
            <Typography variant="body2">Teléfono: {status.phone}</Typography>
            <Typography variant="body2">
              Consentimiento: {status.consent ? 'Activo' : 'Inactivo'}
            </Typography>
            <Typography variant="body2">Nombre: {statusName && statusName.length > 0 ? statusName : '—'}</Typography>
            <Typography variant="body2">Consentido en: {formatTimestamp(status.consentedAt)}</Typography>
            <Typography variant="body2">Revocado en: {formatTimestamp(status.revokedAt)}</Typography>
          </Stack>
        </Paper>
      )}
    </Stack>
  );
}
