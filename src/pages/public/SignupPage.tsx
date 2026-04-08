import { useState } from 'react';
import { Box, Button, Checkbox, Container, FormControlLabel, Stack, TextField, Typography, Alert } from '@mui/material';
import { Link } from 'react-router-dom';
import { AuthApi } from '../../api/auth';

type SignupDraft = {
  firstName: string;
  lastName: string;
  email: string;
  phone: string;
  password: string;
  marketing: boolean;
};

const INITIAL_FORM: SignupDraft = {
  firstName: '',
  lastName: '',
  email: '',
  phone: '',
  password: '',
  marketing: false,
};

export default function SignupPage() {
  const [form, setForm] = useState(INITIAL_FORM);
  const [ok, setOk] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const update = (key: keyof SignupDraft, value: SignupDraft[keyof SignupDraft]) => {
    setForm((prev) => ({ ...prev, [key]: value }));
  };

  const onSubmit = async () => {
    setLoading(true);
    setError(null);
    try {
      await AuthApi.signup({
        firstName: form.firstName.trim(),
        lastName: form.lastName.trim(),
        email: form.email.trim(),
        phone: form.phone.trim() || undefined,
        password: form.password || undefined,
        marketingOptIn: form.marketing,
      });
      setOk(true);
      setForm(INITIAL_FORM);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No pudimos crear la cuenta.';
      setError(message);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Container maxWidth="sm" sx={{ py: 6 }}>
      <Typography variant="h4" gutterBottom>Crear cuenta</Typography>
      {ok && <Alert severity="success">¡Listo! Revisa tu email para verificar y completar tu registro.</Alert>}
      {error && <Alert severity="error" sx={{ mt: ok ? 2 : 0 }}>{error}</Alert>}
      <Stack gap={2} mt={3}>
        <Stack direction={{ xs: 'column', sm: 'row' }} gap={2}>
          <TextField
            label="Nombre"
            fullWidth
            value={form.firstName}
            onChange={(event) => update('firstName', event.target.value)}
          />
          <TextField
            label="Apellido"
            fullWidth
            value={form.lastName}
            onChange={(event) => update('lastName', event.target.value)}
          />
        </Stack>
        <TextField
          label="Email"
          type="email"
          fullWidth
          value={form.email}
          onChange={(event) => update('email', event.target.value)}
        />
        <TextField
          label="WhatsApp / Teléfono"
          fullWidth
          value={form.phone}
          onChange={(event) => update('phone', event.target.value)}
        />
        <TextField
          label="Contraseña"
          type="password"
          fullWidth
          value={form.password}
          onChange={(event) => update('password', event.target.value)}
          helperText="Opcional: también puedes completar tu registro con un enlace de verificación."
        />
        <FormControlLabel
          control={(
            <Checkbox
              checked={form.marketing}
              onChange={(event) => update('marketing', event.target.checked)}
            />
          )}
          label="Quiero recibir novedades por WhatsApp/email"
        />
        <Button onClick={onSubmit} variant="contained" disabled={loading} size="large">
          {loading ? 'Enviando…' : 'Crear cuenta'}
        </Button>
        <Box textAlign="center">
          <Typography variant="body2" color="text.secondary">
            ¿Prefieres probar una clase antes?{' '}
            <Typography component={Link} to="/trial" variant="body2" color="primary" fontWeight={600} sx={{ textDecoration: 'none' }}>
              Solicita tu trial
            </Typography>
          </Typography>
        </Box>
      </Stack>
    </Container>
  );
}
