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

function LoginPage() {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    // TODO: Replace with actual authentication call.
    console.info('Attempted login', { username, password });
  };

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
            Iniciar sesion
          </Button>

          <Typography variant="caption" color="text.disabled" textAlign="center">
            Administracion integral de estudios, label, eventos y escuela.
          </Typography>
        </Stack>
      </Paper>
    </Container>
  );
}

export default LoginPage;
