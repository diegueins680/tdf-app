import React, { useState } from 'react';
import {
  Box,
  Button,
  Checkbox,
  Container,
  FormControlLabel,
  Link,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { Navigate, useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { loginRequest } from '../api/auth';

export default function LoginPage() {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const { session, login } = useSession();
  const navigate = useNavigate();

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setSubmitting(true);
    setError(null);
    try {
      const data = await loginRequest({ username, password });
      login({
        username,
        displayName: username || 'Usuario',
        roles: data.roles ?? [],
        partyId: data.partyId,
        modules: data.modules ?? [],
      });
      navigate('/crm/contactos', { replace: true });
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No pudimos iniciar sesión');
    } finally {
      setSubmitting(false);
    }
  };

  if (session) {
    return <Navigate to="/crm/contactos" replace />;
  }

  return (
    <Container component="main" maxWidth="xs" sx={{ display: 'flex', alignItems: 'center', minHeight: '100vh' }}>
      <Paper elevation={3} sx={{ p: 4, width: '100%' }}>
        <Stack spacing={3} component="form" onSubmit={handleSubmit}>
          <Box>
            <Typography component="h1" variant="h5" fontWeight={600} gutterBottom>
              Inicia sesion en TDF Records
            </Typography>
            <Typography variant="body2" color="text.secondary">
              Ingresa tus credenciales para continuar.
            </Typography>
          </Box>

          <TextField
            label="Nombre de usuario"
            type="text"
            value={username}
            onChange={(event) => setUsername(event.target.value)}
            required
            fullWidth
            autoComplete="username"
          />

          <TextField
            label="Contrasena"
            type="password"
            value={password}
            onChange={(event) => setPassword(event.target.value)}
            required
            fullWidth
            autoComplete="current-password"
          />

          <Stack direction="row" alignItems="center" justifyContent="space-between">
            <FormControlLabel control={<Checkbox />} label="Recordarme" />
            <Link href="#" variant="body2" underline="hover">
              Olvido su contrasena?
            </Link>
          </Stack>

          <Button variant="contained" type="submit" size="large">
            {submitting ? 'Ingresando...' : 'Iniciar sesion'}
          </Button>

          {error && (
            <Typography variant="body2" color="error" textAlign="center">
              {error}
            </Typography>
          )}

          <Typography variant="caption" color="text.disabled" textAlign="center">
            Administracion integral de estudios, label, eventos y escuela.
          </Typography>
        </Stack>
      </Paper>
    </Container>
  );
}
