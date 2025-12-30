import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Drawer,
  Fab,
  IconButton,
  Stack,
  Tooltip,
  Typography,
} from '@mui/material';
import ChatBubbleOutlineIcon from '@mui/icons-material/ChatBubbleOutline';
import CloseIcon from '@mui/icons-material/Close';
import { ChatKit, useChatKit } from '@openai/chatkit-react';
import { env, reportMissingEnv } from '../utils/env';
import { createChatKitClientSecretFetcher } from '../utils/chatkit';

export default function ChatKitLauncher() {
  const [open, setOpen] = useState(false);
  const workflowId = env.read('VITE_CHATKIT_WORKFLOW_ID') ?? '';

  useEffect(() => {
    reportMissingEnv(['VITE_CHATKIT_WORKFLOW_ID']);
  }, []);

  const getClientSecret = useMemo(
    () => createChatKitClientSecretFetcher(workflowId),
    [workflowId],
  );

  const chatkit = useChatKit({
    api: { getClientSecret },
  });

  return (
    <>
      <Tooltip title="Abrir ChatKit" placement="left">
        <Fab
          color="primary"
          size="medium"
          onClick={() => setOpen(true)}
          aria-label="Abrir ChatKit"
          sx={{ boxShadow: '0 12px 28px rgba(15,23,42,0.25)' }}
        >
          <ChatBubbleOutlineIcon />
        </Fab>
      </Tooltip>

      <Drawer
        anchor="right"
        open={open}
        onClose={() => setOpen(false)}
        ModalProps={{ keepMounted: true }}
        PaperProps={{
          sx: {
            width: { xs: '100%', sm: 420 },
            maxWidth: '100%',
            display: 'flex',
            flexDirection: 'column',
          },
        }}
      >
        <Stack
          direction="row"
          alignItems="center"
          justifyContent="space-between"
          sx={{ px: 2, py: 1.5, borderBottom: '1px solid', borderColor: 'divider' }}
        >
          <Stack spacing={0}>
            <Typography fontWeight={800}>ChatKit</Typography>
            <Typography variant="caption" color="text.secondary">
              Asistente IA
            </Typography>
          </Stack>
          <IconButton onClick={() => setOpen(false)} aria-label="Cerrar ChatKit">
            <CloseIcon />
          </IconButton>
        </Stack>
        <Box sx={{ flex: 1, minHeight: 0, p: 2 }}>
          {workflowId ? (
            <Box sx={{ height: '100%' }}>
              <ChatKit control={chatkit.control} style={{ height: '100%', width: '100%' }} />
            </Box>
          ) : (
            <Alert severity="warning">
              Define `VITE_CHATKIT_WORKFLOW_ID` para activar el chat.
            </Alert>
          )}
        </Box>
      </Drawer>
    </>
  );
}
