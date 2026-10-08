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
import { useNavigate } from 'react-router-dom';
import { WhatsAppConsentPublicAPI } from '../api/whatsappConsentPublic';

// Public consent is a double opt-in request: the number itself confirms by
// replying SI. Responses intentionally never reveal a number's consent state.
export default function PublicWhatsAppConsentPage() {
  const navigate = useNavigate();
  const [phone, setPhone] = useState('');
  const [name, setName] = useState('');
  const [consentChecked, setConsentChecked] = useState(false);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [notice, setNotice] = useState<string | null>(null);

  const resetFeedback = () => {
    setError(null);
    setNotice(null);
  };

  const handleConsent = async () => {
    resetFeedback();
    if (!consentChecked) {
      setError('Debes aceptar el consentimiento para continuar.');
      return;
    }
    if (!phone.trim()) {
      setError('Ingresa un número internacional en formato E.164 (ej. +14155552671).');
      return;
    }
    setLoading(true);
    try {
      const trimmedName = name.trim();
      await WhatsAppConsentPublicAPI.createConsent({
        phone: phone.trim(),
        name: trimmedName === '' ? null : trimmedName,
        consent: true,
        source: 'public-review',
      });
      navigate('/whatsapp/ok');
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
      const res = await WhatsAppConsentPublicAPI.optOut({
        phone: phone.trim(),
        reason: 'Solicitud desde página pública',
      });
      setNotice(res.message ?? 'Solicitud de baja registrada.');
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
          Consentimiento de WhatsApp
        </Typography>
        <Typography variant="body2" color="text.secondary">
          Solicita recibir mensajes de TDF Records por WhatsApp. Te enviaremos un mensaje y tu suscripción
          se activa solo cuando respondes SI desde ese número.
        </Typography>
      </Stack>

      <Paper variant="outlined" sx={{ p: 3, borderRadius: 2 }}>
        <Stack spacing={2}>
          <TextField
            type="tel"
            label="Número WhatsApp (E.164)"
            value={phone}
            onChange={(e) => setPhone(e.target.value)}
            placeholder="+14155552671"
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
            label="Confirmo que deseo recibir mensajes por WhatsApp de TDF Records."
          />
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button variant="contained" onClick={() => void handleConsent()} disabled={loading}>
              Solicitar suscripción
            </Button>
            <Button variant="outlined" onClick={() => void handleOptOut()} disabled={loading}>
              Dar de baja
            </Button>
          </Stack>
          <Typography variant="caption" color="text.secondary">
            Puedes escribir STOP en cualquier momento para dejar de recibir mensajes.
          </Typography>
        </Stack>
      </Paper>

      {error && <Alert severity="error">{error}</Alert>}
      {notice && <Alert severity="info">{notice}</Alert>}
    </Stack>
  );
}
