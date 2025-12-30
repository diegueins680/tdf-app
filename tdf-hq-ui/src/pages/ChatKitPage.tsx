import { useEffect, useMemo } from 'react';
import { Alert, Box, Card, CardContent, Stack, Typography } from '@mui/material';
import { ChatKit, useChatKit } from '@openai/chatkit-react';
import { env, reportMissingEnv } from '../utils/env';
import { createChatKitClientSecretFetcher } from '../utils/chatkit';

export default function ChatKitPage() {
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
    <Box sx={{ maxWidth: 1200, mx: 'auto' }}>
      <Stack spacing={2}>
        <Stack spacing={0.5}>
          <Typography variant="overline" color="text.secondary">
            Herramientas · IA
          </Typography>
          <Typography variant="h4" fontWeight={800}>
            ChatKit
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Asistente conversacional conectado a tus workflows de OpenAI.
          </Typography>
        </Stack>

        {!workflowId && (
          <Alert severity="warning">
            Define `VITE_CHATKIT_WORKFLOW_ID` en el frontend para iniciar la sesión de ChatKit.
          </Alert>
        )}

        <Card variant="outlined">
          <CardContent
            sx={{
              p: 0,
              height: { xs: 520, md: 720 },
              display: 'flex',
              flexDirection: 'column',
            }}
          >
            {workflowId ? (
              <Box sx={{ flex: 1, minHeight: 0 }}>
                <ChatKit control={chatkit.control} style={{ height: '100%', width: '100%' }} />
              </Box>
            ) : (
              <Box sx={{ p: 3 }}>
                <Typography color="text.secondary">
                  Configura el workflow de ChatKit y vuelve a cargar esta vista.
                </Typography>
              </Box>
            )}
          </CardContent>
        </Card>

        <Alert severity="info" variant="outlined">
          Env vars: `VITE_CHATKIT_WORKFLOW_ID` en el frontend y `OPENAI_API_KEY` en el backend.
          Opcional: `CHATKIT_WORKFLOW_ID` (fallback backend) y `CHATKIT_API_BASE`.
        </Alert>
      </Stack>
    </Box>
  );
}
