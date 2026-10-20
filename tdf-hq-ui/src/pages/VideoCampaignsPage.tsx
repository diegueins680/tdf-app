import { useRef, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Link as RouterLink, useSearchParams } from 'react-router-dom';
import { Alert, Box, Button, Card, CardContent, Checkbox, CircularProgress, FormControlLabel,
  MenuItem, Stack, Step, StepLabel, Stepper, TextField, Typography } from '@mui/material';
import SessionVideoPreview from '../components/SessionVideoPreview';
import { Fans } from '../api/fans';
import { uploadToDrive } from '../api/drive';
import { VideoCampaigns } from '../api/videoCampaigns';
import { getActiveSession, useSession, type SessionUser } from '../session/SessionContext';
import { getAuthSessionEpoch } from '../session/authEvents';
import { hasStrictAdminAccess } from '../utils/accessControl';
import { campaignLink, createCampaignPieces, emptyVideoCampaign, importedCampaignSchema, safeVideoUrl, validateVideoCampaign,
  type SocialChannel, type VideoCampaign, type VideoCampaignDocument, type VideoPiece } from '../features/campaigns/videoCampaign';

const steps = ['Video', 'Objetivo y redes', 'Piezas y calendario', 'Revisión'];

export default function VideoCampaignsPage() {
  const { session, loading } = useSession();
  if (loading) return <CircularProgress aria-label="Cargando sesión" />;
  if (!session) return <Stack spacing={2}><Typography variant="h4">Lanza tu video</Typography>
    <Button component={RouterLink} to="/login?redirect=%2Fmi-artista%2Fcampanas">Iniciar sesión</Button></Stack>;
  const allowed = session.roles.some((r) => ['artist', 'artista'].includes(r.toLowerCase()))
    || hasStrictAdminAccess(session.roles, session.modules);
  if (!allowed) return <Alert severity="info">Activa tu perfil de artista para preparar campañas.
    <Button component={RouterLink} to="/artista/crear">Crear perfil</Button></Alert>;
  return <CampaignWorkspace key={`${session.partyId}:${getAuthSessionEpoch()}`} session={session} />;
}

function CampaignWorkspace({ session }: { session: SessionUser }) {
  const [params] = useSearchParams();
  const admin = hasStrictAdminAccess(session.roles, session.modules);
  const supplied = Number(params.get('artistId'));
  const initialArtist = admin && Number.isSafeInteger(supplied) && supplied > 0 ? supplied : session.partyId ?? 0;
  const [artistId, setArtistId] = useState(initialArtist);
  const [active, setActive] = useState<VideoCampaign | null>(null);
  const [creating, setCreating] = useState(false);
  const [importError, setImportError] = useState('');
  const importPlan = async (file: File | undefined) => {
    if (!file || getActiveSession() !== session) return;
    try {
      if (file.size > 131072) throw new Error('El plan supera el tamaño permitido.');
      const parsed = importedCampaignSchema.parse(JSON.parse(await file.text()));
      if (getActiveSession() !== session) return;
      if (parsed.artistId !== artistId) throw new Error('Selecciona el artista al que pertenece este plan.');
      const { artistId: owner, campaignId, ...content } = parsed;
      const invalid = validateVideoCampaign(content); if (invalid) throw new Error(invalid);
      setActive({ id: campaignId, artistId: owner, revision: 0, content,
        createdAt: new Date().toISOString(), updatedAt: new Date().toISOString() });
      setImportError('');
    } catch (e) { setImportError(e instanceof Error ? e.message : 'El plan no es válido.'); }
  };
  const artists = useQuery({ queryKey: ['video-campaign-artists', session.partyId],
    queryFn: () => admin ? Fans.listArtists() : Fans.getMyArtistProfile().then((p) => [p]) });
  const campaigns = useQuery({ queryKey: ['video-campaigns', session.partyId, artistId],
    queryFn: () => VideoCampaigns.list(artistId), enabled: artistId > 0 });
  const metrics = useQuery({ queryKey: ['video-campaign-metrics', session.partyId, artistId, active?.id],
    queryFn: () => VideoCampaigns.metrics(artistId, active!.id), enabled: Boolean(active?.revision), retry: false });
  const artist = artists.data?.find((a) => a.apArtistId === artistId);
  return <Box sx={{ maxWidth: 1000, mx: 'auto', p: { xs: 1, sm: 3 } }}><Stack spacing={3}>
    <Typography variant="h4" component="h1">Lanza tu video</Typography>
    <Typography>Prepara tu sesión y una campaña de 14 días para conseguir registros y seguidores en TDF.</Typography>
    <Alert severity="info">Difusión orgánica · USD 0 de pauta. Guardar y exportar no publican contenido.</Alert>
    {importError && <Alert severity="error">{importError}</Alert>}
    {admin && <TextField select label="Artista" value={artistId} onChange={(e) => {
      setArtistId(Number(e.target.value)); setActive(null); setCreating(false);
    }}>{artists.data?.map((a) => <MenuItem key={a.apArtistId} value={a.apArtistId}>{a.apDisplayName}</MenuItem>)}</TextField>}
    {(artists.isError || campaigns.isError) && <Alert severity="error">No pudimos cargar tus campañas.
      <Button onClick={() => { void artists.refetch(); void campaigns.refetch(); }}>Reintentar</Button></Alert>}
    {(artists.isPending || campaigns.isPending) && <CircularProgress aria-label="Cargando campañas" />}
    {campaigns.isSuccess && artist && <>
      {!creating && !active && <><Button variant="contained" onClick={() => setCreating(true)}>Crear campaña</Button>
        <Button component="label">Importar plan<input hidden type="file" accept="application/json,.json" onChange={(e) => {
          const file = e.target.files?.[0]; e.target.value = ''; void importPlan(file);
        }} /></Button>
        {campaigns.data.length === 0 && <Typography>Aún no tienes campañas guardadas.</Typography>}
        {campaigns.data.map((c) => <Card key={c.id}><CardContent><Stack spacing={1}>
          <Typography variant="h6">{c.content.title}</Typography><Typography>{c.content.status === 'ready' ? 'Lista para revisar' : 'Borrador'}</Typography>
          <Button onClick={() => setActive(c)}>Continuar</Button>
          <Button onClick={() => { setActive({ ...c, id: crypto.randomUUID(), revision: 0,
            content: { ...c.content, status: 'draft', title: `${c.content.title.slice(0, 190)} (copia)`,
              pieces: c.content.pieces.map((p) => ({ ...p, status: 'draft', publishedUrl: '' })) } }); }}>Duplicar</Button>
        </Stack></CardContent></Card>)}</>}
      {active && <Alert severity={metrics.isError ? 'warning' : 'info'}>{metrics.data
        ? `Registros atribuidos: ${metrics.data.registrations} · Nuevos seguidores atribuidos: ${metrics.data.follows}`
        : metrics.isError ? 'No pudimos cargar las conversiones.' : 'Cargando conversiones…'}</Alert>}
      {(creating || active) && <CampaignEditor key={`${artistId}:${active?.id ?? 'new'}`} session={session}
        artistId={artistId} artistName={artist.apDisplayName} initial={active}
        onSaved={() => { void campaigns.refetch(); }} onClose={() => { setActive(null); setCreating(false); }} />}
    </>}
  </Stack></Box>;
}

function CampaignEditor({ session, artistId, artistName, initial, onSaved, onClose }: {
  session: SessionUser; artistId: number; artistName: string; initial: VideoCampaign | null;
  onSaved: () => void; onClose: () => void;
}) {
  const [id] = useState(() => initial?.id ?? crypto.randomUUID());
  const [revision, setRevision] = useState(initial?.revision ?? 0);
  const [doc, setDoc] = useState<VideoCampaignDocument>(initial?.content ?? emptyVideoCampaign());
  const [step, setStep] = useState(0);
  const [busy, setBusy] = useState(false);
  const [progress, setProgress] = useState<number | null>(null);
  const [notice, setNotice] = useState('');
  const [error, setError] = useState('');
  const flight = useRef(false);
  const current = () => getActiveSession() === session;
  const patch = (part: Partial<VideoCampaignDocument>) => setDoc((d) => ({ ...d, ...part, status: 'draft' }));
  const patchPiece = (id: string, part: Partial<VideoPiece>) => patch({ pieces: doc.pieces.map((p) => p.id === id ? { ...p, ...part } : p) });
  const save = async (ready = false) => {
    if (flight.current || !current()) return;
    const next = { ...doc, status: ready ? 'ready' as const : doc.status };
    const invalid = validateVideoCampaign(next);
    if (invalid) { setError(invalid); return; }
    flight.current = true; setBusy(true); setError(''); setNotice('');
    try {
      const result = await VideoCampaigns.save(artistId, id, revision, next);
      if (!current()) return;
      setRevision(result.revision); setDoc(result.content); onSaved();
      setNotice(ready ? 'Campaña guardada para revisión. No se ha publicado ni programado.' : 'Borrador guardado. Puedes continuar desde otro dispositivo.');
    } catch (e) {
      if (current()) setError(e instanceof Error ? e.message : 'No pudimos guardar. Reintenta con el mismo borrador.');
    } finally { flight.current = false; if (current()) setBusy(false); }
  };
  const upload = async (file: File | undefined) => {
    if (!file || flight.current || !current()) return;
    if (file.size > 50 * 1024 * 1024) { setError('La carga actual admite hasta 50 MiB. Sube el original a tu almacenamiento y pega su enlace HTTPS.'); return; }
    if (!file.type.startsWith('video/')) { setError('Selecciona un archivo de video.'); return; }
    flight.current = true; setBusy(true); setError(''); setProgress(0);
    try {
      const media = await uploadToDrive(file, { onProgress: (p) => { if (current()) setProgress(p); } });
      if (!current()) return;
      const url = media.webViewLink ?? media.publicUrl ?? media.webContentLink;
      if (!url) throw new Error('No se recibió un enlace al archivo.');
      patch({ videoUrl: url }); setNotice('Archivo cargado. Comprueba sus permisos de reproducción antes de lanzar.');
    } catch (e) { if (current()) setError(e instanceof Error ? e.message : 'Falló la carga. Puedes reintentar o pegar un enlace.'); }
    finally { flight.current = false; if (current()) { setBusy(false); setProgress(null); } }
  };
  const exportPlan = () => {
    const invalid = validateVideoCampaign(doc); if (invalid) { setError(invalid); return; }
    const plan = { artistId, campaignId: id, objective: 'Registros y seguidores del artista en TDF',
      timezone: 'America/Guayaquil', budgetCents: 0, ...doc,
      pieces: doc.pieces.map((p) => ({ ...p, trackingUrl: campaignLink(artistId, id, p) })) };
    const url = URL.createObjectURL(new Blob([JSON.stringify(plan, null, 2)], { type: 'application/json' }));
    const a = document.createElement('a'); a.href = url; a.download = `campana-${id}.json`; a.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    setNotice('Plan descargado. El estado de las piezas se conserva; exportar no significa publicar.');
  };
  const featureVideo = async () => {
    if (flight.current || !current() || doc.status !== 'ready' || revision < 1) return;
    flight.current = true; setBusy(true); setError(''); setNotice('');
    try {
      await VideoCampaigns.feature(artistId, id, revision);
      if (current()) setNotice('Video publicado en el perfil de TDF. No se ha publicado en Instagram ni TikTok.');
    } catch (e) { if (current()) setError(e instanceof Error ? e.message : 'No pudimos actualizar el video del perfil.'); }
    finally { flight.current = false; if (current()) setBusy(false); }
  };
  return <Stack spacing={2}>
    <Stepper activeStep={step} alternativeLabel>{steps.map((s) => <Step key={s}><StepLabel>{s}</StepLabel></Step>)}</Stepper>
    {error && <Alert severity="error">{error}</Alert>}{notice && <Alert severity="success">{notice}</Alert>}
    <Box component="fieldset" disabled={busy} sx={{ border: 0, p: 0, m: 0, minWidth: 0 }}><Stack spacing={2}>
    {step === 0 && <>
      <Typography variant="h6">{artistName}</Typography>
      <TextField label="Título de la sesión" value={doc.title} onChange={(e) => patch({ title: e.target.value })} />
      <TextField label="Enlace HTTPS del video completo" value={doc.videoUrl} onChange={(e) => patch({ videoUrl: e.target.value })}
        helperText="Puedes usar tu propio almacenamiento. No modificamos los permisos del enlace." />
      <Button component="label">Subir video (hasta 50 MiB)<input hidden type="file" accept="video/*"
        onChange={(e) => { const file = e.target.files?.[0]; e.target.value = ''; void upload(file); }} /></Button>
      {progress !== null && <Typography role="status">Cargando: {progress}%</Typography>}
      <TextField label="Duración en segundos" type="number" value={doc.durationSeconds} onChange={(e) => patch({ durationSeconds: Number(e.target.value) })} />
      <TextField label="Enlace HTTPS de portada" value={doc.coverUrl} onChange={(e) => patch({ coverUrl: e.target.value })} />
    </>}
    {step === 1 && <>
      <Typography variant="h6">Conseguir registros y seguidores en TDF</Typography>
      <Typography>Campaña orgánica. D0 es el día del estreno que elijas después de revisar las piezas.</Typography>
      {(['instagram', 'tiktok'] as SocialChannel[]).map((c) => <FormControlLabel key={c} label={c === 'instagram' ? 'Instagram' : 'TikTok'}
        control={<Checkbox checked={doc.channels.includes(c)} onChange={(_e, checked) => patch({
          channels: checked ? [...doc.channels, c] : doc.channels.filter((v) => v !== c),
          pieces: doc.pieces.filter((p) => checked || p.channel !== c),
        })} />}/>)}
      <Alert severity="info">Publicación manual disponible. Las conexiones de publicación automática aún no están habilitadas en este asistente.</Alert>
      <Button disabled={doc.pieces.length > 0 || !doc.channels.length} onClick={() => patch({ pieces: createCampaignPieces(artistName, doc.channels) })}>Preparar textos y calendario</Button>
    </>}
    {step === 2 && <>
      {!doc.pieces.length && <Alert severity="info">Selecciona las redes y prepara el calendario en el paso anterior.</Alert>}
      {doc.pieces.map((p) => <Card key={p.id}><CardContent><Stack spacing={2}>
        <Typography variant="h6">{p.channel === 'instagram' ? 'Instagram' : 'TikTok'} · {p.format} · D{p.day >= 0 ? '+' : ''}{p.day}</Typography>
        <TextField label="Día relativo al estreno" type="number" value={p.day} onChange={(e) => patchPiece(p.id, { day: Number(e.target.value) })} />
        <TextField multiline minRows={3} label="Texto de la publicación" value={p.caption} onChange={(e) => patchPiece(p.id, { caption: e.target.value })} />
        <TextField label="Enlace de la pieza audiovisual" value={p.assetUrl} onChange={(e) => patchPiece(p.id, { assetUrl: e.target.value })} />
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
          <TextField label="Inicio del corte (segundos)" type="number" value={p.sourceStart} onChange={(e) => patchPiece(p.id, { sourceStart: Number(e.target.value) })} />
          <TextField label="Fin del corte (segundos)" type="number" value={p.sourceEnd} onChange={(e) => patchPiece(p.id, { sourceEnd: Number(e.target.value) })} />
        </Stack>
        <TextField select label="Estado de la pieza" value={p.status} onChange={(e) => patchPiece(p.id, { status: e.target.value as VideoPiece['status'] })}>
          <MenuItem value="draft">Borrador</MenuItem><MenuItem value="prepared">Preparada</MenuItem>
          <MenuItem value="exported">Exportada</MenuItem><MenuItem value="published_manual">Publicada manualmente</MenuItem>
        </TextField>
        {p.status === 'published_manual' && <TextField label="URL de la publicación manual" value={p.publishedUrl} onChange={(e) => patchPiece(p.id, { publishedUrl: e.target.value })} />}
        <TextField label="Enlace de seguimiento para esta pieza" value={campaignLink(artistId, id, p)} slotProps={{ input: { readOnly: true } }} />
        <Button onClick={() => { void navigator.clipboard.writeText(`${p.caption}\n${campaignLink(artistId, id, p)}`)
          .then(() => setNotice('Texto y enlace copiados.'))
          .catch(() => setError('No pudimos copiar. Puedes seleccionar el texto o descargar el plan.')); }}>Copiar texto y enlace</Button>
        {p.assetUrl && safeVideoUrl(p.assetUrl) && <Button component="a" href={p.assetUrl} target="_blank" rel="noopener noreferrer">Abrir archivo de la pieza</Button>}
      </Stack></CardContent></Card>)}
    </>}
    {step === 3 && <>
      <Typography variant="h6">{doc.title}</Typography>
      <Typography>{artistName} · {doc.pieces.length} piezas · 14 días · USD 0 de pauta</Typography>
      <Typography sx={{ overflowWrap: 'anywhere' }}>{doc.videoUrl || 'Falta el enlace del video completo.'}</Typography>
      <SessionVideoPreview url={doc.videoUrl} title={doc.title} />
      <Typography>Revisa cortes, textos, permisos del video, créditos y enlaces antes de publicar desde cada red.</Typography>
      <Typography>Los enlaces incluyen atribución por pieza. Las conversiones verificadas se muestran al abrir una campaña guardada. Las métricas externas requieren una conexión autorizada.</Typography>
      <Button onClick={exportPlan}>Descargar plan y textos</Button>
      <Button variant="contained" onClick={() => void save(true)}>Guardar para revisión</Button>
      {doc.status === 'ready' && revision > 0 && <>
        <Typography>La siguiente acción hace visible el video en el perfil público del artista en TDF.</Typography>
        <Button color="secondary" variant="outlined" onClick={() => void featureVideo()}>Publicar video en el perfil de TDF</Button>
      </>}
    </>}
    </Stack></Box>
    <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
      <Button disabled={busy || step === 0} onClick={() => { setStep(step - 1); window.scrollTo(0, 0); }}>Anterior</Button>
      {step < 3 && <Button disabled={busy} onClick={() => { setStep(step + 1); window.scrollTo(0, 0); }}>Siguiente</Button>}
      <Button disabled={busy} onClick={() => void save()}>Guardar borrador</Button>
      <Button disabled={busy} onClick={onClose}>Volver a campañas</Button>
    </Stack>
  </Stack>;
}
