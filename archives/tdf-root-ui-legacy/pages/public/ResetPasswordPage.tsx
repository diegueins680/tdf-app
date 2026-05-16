import { useMemo, useState } from 'react';
import { Alert, Box, Button, Container, Stack, TextField, Typography } from '@mui/material';
import { useSearchParams } from 'react-router-dom';
import { AuthApi } from '../../api/auth';

export default function ResetPasswordPage() {
  const [searchParams] = useSearchParams();
  const presetToken = useMemo(() => searchParams.get('token') ?? '', [searchParams]);
  const [token, setToken] = useState(presetToken);
  const [password, setPassword] = useState('');
  const [confirm, setConfirm] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState(false);
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError(null);
    if (!token.trim()) {
      setError('El enlace de recuperación no es válido. Copia el token del correo e intenta nuevamente.');
      return;
    }
    if (!password || password !== confirm) {
      setError('Las contraseñas no coinciden.');
      return;
    }
    setLoading(true);
    try {
      await AuthApi.resetPassword({ token: token.trim(), password });
      setSuccess(true);
      setPassword('');
      setConfirm('');
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No pudimos restablecer tu contraseña.';
      setError(message);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Container maxWidth="sm" sx={{ py: 6 }}>
      <Typography variant="h4" gutterBottom>Restablecer contraseña</Typography>
      <Typography variant="body1" color="text.secondary" gutterBottom>
        El token se completa automáticamente si abriste el enlace desde tu correo. Si no, cópialo y pégalo a continuación.
      </Typography>
      <Box component="form" onSubmit={handleSubmit} noValidate>
        <Stack gap={2} mt={3}>
          {success && (
            <Alert severity="success">
              Tu contraseña se actualizó correctamente. Ya puedes iniciar sesión con tu nueva contraseña.
            </Alert>
          )}
          {error && <Alert severity="error">{error}</Alert>}
          <TextField
            label="Token de verificación"
            value={token}
            onChange={(event) => setToken(event.target.value)}
            fullWidth
            required
            disabled={loading && !!token}
            helperText="Lo encontrarás en el enlace recibido por correo."
          />
          <TextField
            label="Nueva contraseña"
            type="password"
            value={password}
            onChange={(event) => setPassword(event.target.value)}
            fullWidth
            required
            disabled={loading}
          />
          <TextField
            label="Confirmar contraseña"
            type="password"
            value={confirm}
            onChange={(event) => setConfirm(event.target.value)}
            fullWidth
            required
            disabled={loading}
          />
          <Button type="submit" variant="contained" disabled={loading} size="large">
            {loading ? 'Actualizando…' : 'Restablecer contraseña'}
          </Button>
        </Stack>
      </Box>
    </Container>
  );
}
