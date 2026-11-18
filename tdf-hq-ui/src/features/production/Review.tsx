import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Stack, TextField, Typography } from '@mui/material';
import { buildAuthorizationHeader } from '../../api/authHeader';
import { API_BASE_URL } from '../../api/client';
import { getAuthSessionEpoch } from '../../session/authEvents';
import { newUploadKey, normalizeUploadFailure, uploadFailureMessage } from '../../api/uploadFailure';
import { uploadToDrive } from '../../api/drive';
import type { Action, Project, Task, Version } from '../../api/productions';
import { Field, Form, SelectField } from './ProductionForms';

export type Save = (body: Action) => Promise<boolean>;

function ReviewVersion({ p, t, v, actor, save }: { p: Project; t: Task; v: Version; actor: number; save: Save }) {
  const [playing, setPlaying] = useState('');
  const [busy, setBusy] = useState(false);
  const [failure, setFailure] = useState('');
  const [note, setNote] = useState('');
  const pending = useRef<{ fingerprint: string; id: string } | null>(null);
  const controller = useRef<AbortController | null>(null);
  const source = useRef('');
  useEffect(() => () => {
    controller.current?.abort();
    if (source.current) URL.revokeObjectURL(source.current);
  }, []);
  async function listen() {
    setBusy(true); setFailure('');
    const epoch = getAuthSessionEpoch();
    const request = new AbortController();
    controller.current = request;
    try {
      const auth = buildAuthorizationHeader();
      const response = await fetch(`${API_BASE_URL}/productions/${p.id}/versions/${v.id}/audio`, {
        credentials: 'include', headers: auth ? { Authorization: auth } : {}, cache: 'no-store', signal: request.signal,
      });
      if (!response.ok) throw new Error('No se pudo abrir esta versión.');
      const bytes = await response.blob();
      if (epoch !== getAuthSessionEpoch() || request.signal.aborted) return;
      source.current = URL.createObjectURL(bytes.slice(0, bytes.size, v.media_type));
      setPlaying(source.current);
    } catch { if (!request.signal.aborted) setFailure('Audio no disponible. Vuelve a intentarlo.'); }
    finally { if (!request.signal.aborted) setBusy(false); }
  }
  async function decide(kind: string) {
    const fingerprint = JSON.stringify([v.id, kind, note]);
    if (pending.current?.fingerprint !== fingerprint) pending.current = { fingerprint, id: crypto.randomUUID() };
    if (await save({ op: 'feedback', version: v.id, kind, note, request: pending.current.id })) {
      pending.current = null; setNote('');
    }
  }
  return <Stack spacing={1} sx={{ borderLeft: '3px solid', borderColor: 'divider', pl: 2, my: 2 }}>
    <Typography variant="subtitle1">v{v.number}{v.current ? ' · Vigente' : ''}</Typography>
    {/* The submitted music mix has no authored caption track. */}
    {/* eslint-disable-next-line jsx-a11y/media-has-caption */}
    {playing ? <audio controls src={playing} aria-label={`${t.title} v${v.number}`} style={{ maxWidth: '100%' }} />
      : <Button disabled={busy} onClick={() => void listen()}>{busy ? 'Cargando audio…' : `Escuchar v${v.number}`}</Button>}
    {failure && <Alert severity="error">{failure}</Alert>}
    {v.feedback.map((f, index) => <Typography key={index} variant="body2">
      {f.actor} · {({ comment: 'Comentario', changes: 'Cambios solicitados', approve: 'Aprobada' })[f.kind]} · {new Date(f.at).toLocaleString('es-EC')} {f.note}
    </Typography>)}
    <TextField label={`Observación sobre v${v.number}`} size="small" value={note} onChange={e => setNote(e.target.value)} />
    <Stack direction="row" flexWrap="wrap">
      <Button disabled={!note.trim()} onClick={() => void decide('comment')}>Comentar</Button>
      {v.current && t.status === 'En revisión' && <>
        {(t.approver === actor || p.role === 'client') && <Button disabled={!note.trim()} onClick={() => void decide('changes')}>Solicitar cambios</Button>}
        {t.approver === actor && <Button onClick={() => void decide('approve')}>Aprobar v{v.number}</Button>}
      </>}
    </Stack>
  </Stack>;
}

export function Reviews({ p, t, actor, save, editable }: { p: Project; t: Task; actor: number; save: Save; editable: boolean }) {
  const [uploading, setUploading] = useState(false);
  const [error, setError] = useState('');
  const attempts = useRef(new WeakMap<File, string>());
  async function publish(d: FormData) {
    const file = d.get('file');
    if (!(file instanceof File) || !file.size) return;
    const epoch = getAuthSessionEpoch();
    setUploading(true); setError('');
    try {
      const key = attempts.current.get(file) ?? newUploadKey();
      attempts.current.set(file, key);
      const uploaded = await uploadToDrive(file, { destination: 'tdf', visibility: 'private', idempotencyKey: key });
      if (getAuthSessionEpoch() === epoch) await save({ op: 'publish', task: t.id, file: uploaded.id, number: Number(d.get('number')), recipients: d.getAll('recipients') });
    } catch (e) { setError(uploadFailureMessage(normalizeUploadFailure(e, 'server'))); }
    finally { setUploading(false); }
  }
  return <>
    {t.versions.map(v => <ReviewVersion key={v.id} {...{ p,t,v,actor,save }} />)}
    {editable && t.approver !== null && t.status !== 'Completada' && <details><summary>Entregar versión de audio</summary>
      <fieldset disabled={uploading}><Form key={t.versions[0]?.id ?? t.id} submit={uploading ? 'Subiendo audio…' : 'Compartir entrega'} action={d => void publish(d)}>
        <Field name="number" title="Número de versión" type="number" value={(t.versions[0]?.number ?? 0) + 1} />
        <label>Audio privado <input type="file" name="file" required accept="audio/*" /></label>
        <SelectField name="recipients" title="Otros clientes con acceso a esta versión" multiple>
          {p.people.filter(a => a.role === 'client' && a.id !== t.approver).map(a => <option key={a.id} value={a.id}>{a.name}</option>)}
        </SelectField></Form></fieldset>{error && <Alert severity="error">{error}</Alert>}
    </details>}
  </>;
}
