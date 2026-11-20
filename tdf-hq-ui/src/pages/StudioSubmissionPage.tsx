import { useEffect, useRef, useState } from 'react';
import {
  Alert,
  Button,
  Container,
  LinearProgress,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { intakeRequest, sendPhoto, type Submission } from '../api/studioSubmissions';

interface Item {
  file: File;
  url: string;
  state: string;
  progress: number;
  done?: boolean;
  previewFailed?: boolean;
}
const accept = '.jpg,.jpeg,.png,.webp,.heic,.heif';
const states = {
  pending: 'Pendiente de revisión',
  accepted: 'Aprobada para el CMS',
  rejected: 'Rechazada',
  expired: 'Caducada',
};

export default function StudioSubmissionPage() {
  const fragment = new URLSearchParams(window.location.hash.slice(1));
  const [token] = useState(() => fragment.get('enlace') ?? '');
  const [receipt, setReceipt] = useState(() => fragment.get('lote') ?? crypto.randomUUID());
  const [items, setItems] = useState<Item[]>([]);
  const [credit, setCredit] = useState('');
  const [message, setMessage] = useState('');
  const [history, setHistory] = useState<Submission[]>([]);
  const [busy, setBusy] = useState(false);
  const [started, setStarted] = useState(new URLSearchParams(location.hash.slice(1)).has('lote'));
  const urls = useRef<string[]>([]);
  useEffect(() => () => urls.current.forEach((url) => URL.revokeObjectURL(url)), []);
  useEffect(() => {
    const reopen = () => window.location.reload();
    window.addEventListener('hashchange', reopen);
    return () => window.removeEventListener('hashchange', reopen);
  }, []);
  const refresh = async () => setHistory(await intakeRequest<Submission[]>('status', { receipt }));
  useEffect(() => {
    if (new URLSearchParams(location.hash.slice(1)).has('lote')) {
      void intakeRequest<Submission[]>('status', { receipt })
        .then(setHistory)
        .catch(() => setMessage('No se pudo consultar el lote.'));
    }
  }, [receipt]);
  const add = (files: File[]) => {
    if (started || busy) return;
    const next = [...items];
    const errors: string[] = [];
    for (const file of files) {
      const supported =
        /\.(jpe?g|png|webp|heic|heif)$/i.test(file.name) ||
        ['image/jpeg', 'image/png', 'image/webp', 'image/heic', 'image/heif'].includes(file.type);
      if (next.length >= 30 || !supported || !file.size || file.size > 25 * 1024 ** 2) {
        errors.push(file.name);
        continue;
      }
      if (
        next.some(
          (item) =>
            item.file.name === file.name &&
            item.file.size === file.size &&
            item.file.lastModified === file.lastModified,
        )
      )
        continue;
      const url = URL.createObjectURL(file);
      urls.current.push(url);
      next.push({ file, url, state: 'Lista para enviar', progress: 0 });
    }
    setItems(next);
    setMessage(errors.length ? 'No se añadieron: ' + errors.join(', ') : '');
  };
  const move = (index: number, offset: number) => {
    const next = [...items];
    next.splice(index + offset, 0, ...next.splice(index, 1));
    setItems(next);
  };
  const patch = (index: number, value: Partial<Item>) =>
    setItems((current) => current.map((item, i) => (i === index ? { ...item, ...value } : item)));
  const submit = async () => {
    setBusy(true);
    setMessage('');
    try {
      await intakeRequest('batch', { token, receipt, credit });
      setStarted(true);
      const url = new URL(location.href);
      url.hash = new URLSearchParams({ enlace: token, lote: receipt }).toString();
      window.history.replaceState(null, '', url);
      for (const [index, item] of items.entries()) {
        if (item.done) continue;
        patch(index, { state: 'Enviando…', progress: 0 });
        try {
          const result = await sendPhoto(item.file, index, token, receipt, (progress) =>
            patch(index, { progress }),
          );
          patch(index, {
            done: true,
            progress: 100,
            state: result.duplicate ? 'Duplicada en este lote' : 'Recibida para revisión',
          });
        } catch (error) {
          patch(index, { state: error instanceof Error ? error.message : 'Error al enviar' });
        }
      }
      await refresh();
    } catch (error) {
      setMessage(error instanceof Error ? error.message : 'No se pudo enviar.');
    } finally {
      setBusy(false);
    }
  };
  return (
    <Container component="main" id="main-content" maxWidth="sm" sx={{ py: 5 }}>
      <Stack spacing={2}>
        <Typography component="h1" variant="h4">
          Enviar fotos del estudio
        </Typography>
        <Typography>
          El equipo de TDF revisará las fotos antes de publicarlas. Hasta 30 por lote, de 25 MB cada una.
          JPG, PNG, WebP, HEIC o HEIF.
        </Typography>
        {!token && (
          <Alert severity="warning">Abre el enlace que te compartió TDF para enviar fotos.</Alert>
        )}
        {!started && token && (
          <Paper
            variant="outlined"
            sx={{ p: 3 }}
            onDragOver={(e) => e.preventDefault()}
            onDrop={(e) => {
              e.preventDefault();
              add(Array.from(e.dataTransfer.files));
            }}
          >
            <Stack spacing={2}>
              <Typography>Arrastra tus fotos aquí o selecciónalas en tu dispositivo.</Typography>
              <input
                aria-label="Seleccionar fotos"
                type="file"
                multiple
                accept={accept}
                disabled={busy}
                onChange={(e) => {
                  add(Array.from(e.target.files ?? []));
                  e.target.value = '';
                }}
              />
              <TextField
                label="Crédito del fotógrafo (opcional)"
                value={credit}
                disabled={busy}
                inputProps={{ maxLength: 250 }}
                onChange={(e) => setCredit(e.target.value)}
              />
            </Stack>
          </Paper>
        )}
        {items.map((item, index) => (
          <Paper variant="outlined" sx={{ p: 2 }} key={item.url}>
            <Stack spacing={1}>
              {item.previewFailed ? (
                <Typography variant="caption">
                  Este dispositivo no puede mostrar la vista previa.
                </Typography>
              ) : (
                <img
                  src={item.url}
                  alt=""
                  style={{ maxWidth: 140, maxHeight: 100 }}
                  onError={() => patch(index, { previewFailed: true })}
                />
              )}
              <Typography sx={{ overflowWrap: 'anywhere' }}>
                {index + 1}. {item.file.name}
              </Typography>
              <Typography role="status">{item.state}</Typography>
              {busy && !item.done && (
                <LinearProgress
                  aria-label={item.file.name}
                  variant="determinate"
                  value={item.progress}
                />
              )}
              {!started && !busy && (
                <Stack direction="row">
                  <Button disabled={!index} onClick={() => move(index, -1)}>
                    Subir
                  </Button>
                  <Button disabled={index === items.length - 1} onClick={() => move(index, 1)}>
                    Bajar
                  </Button>
                  <Button onClick={() => setItems(items.filter((_, i) => i !== index))}>Quitar</Button>
                </Stack>
              )}
            </Stack>
          </Paper>
        ))}
        {items.some((item) => !item.done) && (
          <Button variant="contained" disabled={busy} onClick={() => void submit()}>
            {busy ? 'Enviando…' : started ? 'Reintentar pendientes' : 'Enviar lote'}
          </Button>
        )}
        {message && <Alert severity="warning">{message}</Alert>}
        {started && (
          <>
            <Typography variant="h6">Estado de tu lote</Typography>
            <Typography>Guarda esta dirección para consultar la revisión de tus fotos.</Typography>
            {history.map((row) => (
              <Typography key={row.position}>
                {row.name} · {states[row.state]}
              </Typography>
            ))}
            <Button
              disabled={busy}
              onClick={() => void refresh().catch(() => setMessage('No se pudo consultar el lote.'))}
            >
              Actualizar estado
            </Button>
            {token && (
              <Button
                disabled={busy}
                onClick={() => {
                  window.history.replaceState(
                    null,
                    '',
                    location.pathname + '#enlace=' + encodeURIComponent(token),
                  );
                  setReceipt(crypto.randomUUID());
                  setItems([]);
                  setHistory([]);
                  setStarted(false);
                  setMessage('');
                }}
              >
                Empezar otro lote
              </Button>
            )}
          </>
        )}
      </Stack>
    </Container>
  );
}
