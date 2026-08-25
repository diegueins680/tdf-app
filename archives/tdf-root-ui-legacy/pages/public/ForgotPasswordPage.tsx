import { useState } from 'react';
import { Alert, Box, Button, Container, Stack, TextField, Typography } from '@mui/material';
import { AuthApi } from '../../api/auth';

export default function ForgotPasswordPage() {
  const [email, setEmail] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState(false);
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError(null);
    setSuccess(false);
    setLoading(true);
    try {
      await AuthApi.requestPasswordReset({ email: email.trim() });
      setSuccess(true);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No pudimos enviar el correo de recuperación.';
      setError(message);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Container maxWidth="sm" sx={{ py: 6 }}>
      <Typography variant="h4" gutterBottom>Recupera tu acceso</Typography>
      <Typography variant="body1" color="text.secondary" gutterBottom>
        Ingresa tu correo electrónico y te enviaremos un enlace para restablecer tu contraseña.
      </Typography>
      <Box component="form" onSubmit={handleSubmit} noValidate>
        <Stack gap={2} mt={3}>
          {success && (
            <Alert severity="success">
              ¡Listo! Si el correo está registrado recibirás un enlace para restablecer tu contraseña en los próximos minutos.
            </Alert>
          )}
          {error && <Alert severity="error">{error}</Alert>}
          <TextField
            label="Correo electrónico"
            type="email"
            required
            value={email}
            onChange={(event) => setEmail(event.target.value)}
            disabled={loading}
            autoComplete="email"
            autoFocus
          />
          <Button type="submit" variant="contained" disabled={loading} size="large">
            {loading ? 'Enviando…' : 'Enviar enlace'}
          </Button>
        </Stack>
      </Box>
    </Container>
  );
}
