import { useState } from 'react';
import { createRoot } from 'react-dom/client';
import { BrowserRouter } from 'react-router-dom';
import { TextField } from '@mui/material';
import '../src/i18n';
import { SessionProvider, useSession } from '../src/session/SessionContext';
import GoogleDriveUploadWidget from '../src/components/GoogleDriveUploadWidget';
import GoogleDriveCallbackPage from '../src/pages/GoogleDriveCallbackPage';
import { useOwnedTabDraft } from '../src/session/useOwnedTabDraft';

function Harness() {
  const { session, loading } = useSession();
  const draft = useOwnedTabDraft({ bio: '' }, { ownerId: loading ? null : session?.partyId ?? null, scope: 'synthetic-artist',
    parse: (value) => value && typeof value === 'object' && 'bio' in value && typeof value.bio === 'string' ? { bio: value.bio } : undefined });
  const [bio, setBio] = draft.field('bio');
  const [done, setDone] = useState(false);
  if (new URLSearchParams(window.location.search).has('code')) return <GoogleDriveCallbackPage />;
  if (loading) return <p role="status">Cargando sesión</p>;
  return <main style={{ maxWidth: 640, margin: 'auto', padding: 16 }}>
    <h1>Perfil de prueba</h1>
    <TextField label="Biografía" value={bio} onChange={(event) => setBio(event.target.value)} multiline fullWidth />
    <GoogleDriveUploadWidget recoveryScope="synthetic-artist" formRecoveryAvailable={draft.persistenceAvailable} label="Subir portada" visibility="public" onComplete={() => setDone(true)} />
    {done && <p role="status">Archivo listo</p>}
  </main>;
}
createRoot(document.getElementById('root')!).render(<BrowserRouter><SessionProvider><Harness /></SessionProvider></BrowserRouter>);
