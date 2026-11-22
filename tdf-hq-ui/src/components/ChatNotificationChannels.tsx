import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  Stack,
  Switch,
  Typography,
} from '@mui/material';
import NotificationsActiveOutlinedIcon from '@mui/icons-material/NotificationsActiveOutlined';
import { ChatAPI, type ChatNotificationChannels as Channels } from '../api/chat';

const QUERY_KEY = ['chat-notification-channels'] as const;

/** Lets the signed-in user turn chat email and phone reminders on or off. */
export default function ChatNotificationChannels() {
  const [open, setOpen] = useState(false);
  const qc = useQueryClient();
  const channelsQuery = useQuery({
    queryKey: QUERY_KEY,
    queryFn: () => ChatAPI.getNotificationChannels(),
    enabled: open,
    retry: false,
  });
  const saveMutation = useMutation({
    mutationFn: (next: Channels) => ChatAPI.updateNotificationChannels(next),
    onSuccess: (saved) => qc.setQueryData(QUERY_KEY, saved),
  });
  const channels = saveMutation.isPending ? saveMutation.variables : channelsQuery.data;
  const toggle = (key: keyof Channels) => (_: unknown, checked: boolean) => {
    if (!channels) return;
    saveMutation.mutate({ ...channels, [key]: checked });
  };

  return (
    <>
      <Button
        variant="text"
        startIcon={<NotificationsActiveOutlinedIcon />}
        onClick={() => setOpen(true)}
      >
        Avisos
      </Button>
      <Dialog
        open={open}
        onClose={() => setOpen(false)}
        fullWidth
        maxWidth="xs"
        aria-labelledby="chat-notification-channels-title"
      >
        <DialogTitle id="chat-notification-channels-title">Avisos de mensajes nuevos</DialogTitle>
        <DialogContent>
          <Stack spacing={2}>
            <Typography variant="body2" color="text.secondary">
              Siempre verás los mensajes nuevos en la campana. Además puedes recibir un aviso fuera de TDF.
              Los avisos no incluyen el texto del mensaje.
            </Typography>
            {channelsQuery.isPending && <Typography role="status">Cargando…</Typography>}
            {channelsQuery.isError && (
              <Alert
                severity="error"
                action={<Button onClick={() => void channelsQuery.refetch()}>Reintentar</Button>}
              >
                No se pudieron cargar tus preferencias.
              </Alert>
            )}
            {channels && (
              <>
                <FormControlLabel
                  control={<Switch checked={channels.email} onChange={toggle('email')} disabled={saveMutation.isPending} />}
                  label="Correo si no lo lees en 10 minutos (máximo uno por conversación cada 6 horas)"
                />
                <FormControlLabel
                  control={<Switch checked={channels.push} onChange={toggle('push')} disabled={saveMutation.isPending} />}
                  label="Notificación en el celular (app TDF con notificaciones permitidas)"
                />
              </>
            )}
            {saveMutation.isError && (
              <Alert severity="error">No se pudo guardar el cambio. Inténtalo de nuevo.</Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setOpen(false)}>Cerrar</Button>
        </DialogActions>
      </Dialog>
    </>
  );
}
