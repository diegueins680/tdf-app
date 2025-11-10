import { useEffect, useState } from 'react';
import { Dialog, DialogActions, DialogContent, DialogTitle, Button, TextField, Stack, Typography } from '@mui/material';
import { useSession, DEFAULT_DEMO_TOKEN } from '../session/SessionContext';

type ApiTokenDialogProps = {
  open: boolean;
  onClose: () => void;
};

export default function ApiTokenDialog({ open, onClose }: ApiTokenDialogProps) {
  const { session, setApiToken } = useSession();
  const [token, setToken] = useState(session?.apiToken ?? '');

  useEffect(() => {
    if (open) {
      setToken(session?.apiToken ?? '');
    }
  }, [open, session]);

  const handleSave = () => {
    setApiToken(token.trim() || null);
    onClose();
  };

  const handleUseDemo = () => {
    setToken(DEFAULT_DEMO_TOKEN);
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Configurar token API</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField
            label="Token Bearer"
            value={token}
            onChange={(e) => setToken(e.target.value)}
            placeholder="tdf_xxxx-xxxx"
            fullWidth
          />
          <Typography variant="body2" color="text.secondary">
            Este token se envía en el encabezado Authorization para todas las llamadas al backend. Pega aquí tu token activo.
          </Typography>
          {DEFAULT_DEMO_TOKEN && (
            <Button variant="text" onClick={handleUseDemo} sx={{ alignSelf: 'flex-start' }}>
              Usar token de demo
            </Button>
          )}
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={handleSave}>
          Guardar
        </Button>
      </DialogActions>
    </Dialog>
  );
}
