import { useState } from 'react';
import { Alert, Box, Button, Container, Stack, TextField, Typography } from '@mui/material';
import { AuthApi } from '../../api/auth';
import { useAuth } from '../../auth/AuthProvider';

export default function ChangePasswordPage() {
  const [currentPassword, setCurrentPassword] = useState('');
  const [newPassword, setNewPassword] = useState('');
  const [confirmPassword, setConfirmPassword] = useState('');
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState(false);
  const { user } = useAuth();

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError(null);
    setSuccess(false);

    if (!newPassword || newPassword !== confirmPassword) {
      setError('Las contraseñas nuevas no coinciden.');
      return;
    }

    if (!user?.username) {
      setError('No pudimos identificar tu usuario. Vuelve a iniciar sesión e inténtalo nuevamente.');
      return;
    }

    setLoading(true);
    try {
      await AuthApi.changePassword({ username: user.username, currentPassword, newPassword });
      setSuccess(true);
      setCurrentPassword('');
      setNewPassword('');
      setConfirmPassword('');
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No pudimos actualizar tu contraseña.';
      setError(message);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Container maxWidth="sm" sx={{ py: 6 }}>
      <Typography variant="h4" gutterBottom>Cambiar contraseña</Typography>
      <Typography variant="body1" color="text.secondary" gutterBottom>
        Usa esta sección para actualizar la contraseña de tu cuenta.
      </Typography>
      <Box component="form" onSubmit={handleSubmit} noValidate>
        <Stack gap={2} mt={3}>
          {success && <Alert severity="success">Tu contraseña se actualizó correctamente.</Alert>}
          {error && <Alert severity="error">{error}</Alert>}
          <TextField
            label="Contraseña actual"
            type="password"
            value={currentPassword}
            onChange={(event) => setCurrentPassword(event.target.value)}
            required
            fullWidth
            disabled={loading}
            autoComplete="current-password"
          />
          <TextField
            label="Nueva contraseña"
            type="password"
            value={newPassword}
            onChange={(event) => setNewPassword(event.target.value)}
            required
            fullWidth
            disabled={loading}
            autoComplete="new-password"
          />
          <TextField
            label="Confirmar nueva contraseña"
            type="password"
            value={confirmPassword}
            onChange={(event) => setConfirmPassword(event.target.value)}
            required
            fullWidth
            disabled={loading}
            autoComplete="new-password"
          />
          <Button type="submit" variant="contained" disabled={loading} size="large">
            {loading ? 'Guardando…' : 'Actualizar contraseña'}
          </Button>
        </Stack>
      </Box>
    </Container>
  );
}
