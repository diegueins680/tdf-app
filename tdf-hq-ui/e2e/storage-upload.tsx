import { useState } from 'react';
import { createRoot } from 'react-dom/client';
import { BrowserRouter } from 'react-router-dom';
import { TextField } from '@mui/material';
import '../src/i18n';
import { SessionProvider } from '../src/session/SessionContext';
import GoogleDriveUploadWidget from '../src/components/GoogleDriveUploadWidget';
import GoogleDriveCallbackPage from '../src/pages/GoogleDriveCallbackPage';

function Harness() {
  const [bio, setBio] = useState('');
  const [done, setDone] = useState(false);
  if (new URLSearchParams(window.location.search).has('code')) return <GoogleDriveCallbackPage />;
  return <main style={{ maxWidth: 640, margin: 'auto', padding: 16 }}>
    <h1>Perfil de prueba</h1>
    <TextField label="Biografía" value={bio} onChange={(event) => setBio(event.target.value)} multiline fullWidth />
    <GoogleDriveUploadWidget label="Subir portada" visibility="public" onComplete={() => setDone(true)} />
    {done && <p role="status">Archivo listo</p>}
  </main>;
}
createRoot(document.getElementById('root')!).render(<BrowserRouter><SessionProvider><Harness /></SessionProvider></BrowserRouter>);
