import { useRef, useState } from 'react';
import { Alert, Button, Stack, TextField, Typography } from '@mui/material';
import { useSession } from '../session/SessionContext';
import { post } from '../api/client';

export default function VeniceAssistant() {
  const { session } = useSession();
  return session ? <AssistantConversation key={session.partyId ?? session.username} /> : null;
}

function AssistantConversation() {
  const [turns, setTurns] = useState<string[]>([]);
  const [draft, setDraft] = useState('');
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState('');
  const pending = useRef(false);

  async function send() {
    const message = draft.trim();
    if (!message || pending.current) return;
    const history = [...turns.slice(-20), message];
    while (history.length > 1 && history.join('').length > 32000) history.splice(0, 2);
    pending.current = true;
    setBusy(true);
    setError('');
    try {
      const reply = await post<string>('/assistant/chat', history);
      setTurns([...turns, message, reply]);
      setDraft('');
    } catch {
      setError('No se pudo obtener una respuesta. Puedes volver a intentarlo.');
    } finally {
      pending.current = false;
      setBusy(false);
    }
  }

  return (
    <Stack spacing={2} sx={{ maxWidth: 800, mx: 'auto', p: 2 }}>
      <Typography variant="h5" component="h1">Asistente de TDF</Typography>
      <Stack spacing={2} role="log" aria-live="polite">
        {turns.map((text, index) => (
          <Stack key={index}>
            <Typography variant="caption">{index % 2 === 0 ? 'Tú' : 'Venice'}</Typography>
            <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{text}</Typography>
          </Stack>
        ))}
      </Stack>
      {error && <Alert severity="error">{error}</Alert>}
      <TextField multiline label="Mensaje" value={draft} disabled={busy}
        inputProps={{ maxLength: 8000 }} onChange={event => setDraft(event.target.value)} />
      <Button variant="contained" disabled={busy || !draft.trim()} onClick={() => void send()}>
        {busy ? 'Enviando…' : 'Enviar'}
      </Button>
    </Stack>
  );
}
