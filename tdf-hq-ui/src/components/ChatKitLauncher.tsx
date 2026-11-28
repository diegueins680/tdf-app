import { useState } from 'react';
import { Button, Drawer, Fab, Tooltip } from '@mui/material';
import ChatBubbleOutlineIcon from '@mui/icons-material/ChatBubbleOutline';
import VeniceAssistant from './VeniceAssistant';
import { useSession } from '../session/SessionContext';
import { canAccessPath } from '../utils/accessControl';

export default function ChatKitLauncher() {
  const [open, setOpen] = useState(false);
  const { session } = useSession();
  if (!canAccessPath('/herramientas/chatkit', session?.roles, session?.modules)) return null;

  return (
    <>
      <Tooltip title="Abrir asistente">
        <Fab color="primary" size="medium" aria-label="Abrir asistente" onClick={() => setOpen(true)}>
          <ChatBubbleOutlineIcon />
        </Fab>
      </Tooltip>
      <Drawer anchor="right" open={open} onClose={() => setOpen(false)} ModalProps={{ keepMounted: true }}
        PaperProps={{ sx: { width: { xs: '100%', sm: 420 } } }}>
        <Button onClick={() => setOpen(false)}>Cerrar</Button>
        <VeniceAssistant />
      </Drawer>
    </>
  );
}
