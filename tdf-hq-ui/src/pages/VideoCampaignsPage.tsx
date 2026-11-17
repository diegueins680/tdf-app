import ArtistProfileLink from '../components/ArtistProfileLink';
import { useRef, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Link as RouterLink, useSearchParams } from 'react-router-dom';
import { Alert, Box, Button, Card, CardContent, Checkbox, CircularProgress, FormControlLabel,
  MenuItem, Stack, Step, StepLabel, Stepper, TextField, Typography } from '@mui/material';
import SessionVideoPreview from '../components/SessionVideoPreview';
import { Fans } from '../api/fans';
import GoogleDriveUploadWidget from '../components/GoogleDriveUploadWidget';
import { VideoCampaigns } from '../api/videoCampaigns';
import { ApiError } from '../api/client';
import { getActiveSession, useSession, type SessionUser } from '../session/SessionContext';
import { getAuthSessionEpoch } from '../session/authEvents';
import { hasStrictAdminAccess } from '../utils/accessControl';
import { campaignLink, prepareMissingCampaignPieces, emptyVideoCampaign, importedCampaignSchema, safeVideoUrl, validateVideoCampaign,
  type SocialChannel, type VideoCampaign, type VideoCampaignDocument, type VideoPiece } from '../features/campaigns/videoCampaign';

const steps = ['Video', 'Objetivo y redes', 'Piezas y calendario', 'Revisión'];

export default function VideoCampaignsPage() {
  const { session, loading } = useSession();
  const [params] = useSearchParams();
  const target = Number(params.get('artistId'));
  const returnPath = Number.isSafeInteger(target) && target > 0 ? `/mi-artista/campanas?artistId=${target}` : '/mi-artista/campanas';
  if (loading) return <CircularProgress aria-label="Cargando sesión" />;
  if (!session) return <Stack spacing={2}><Typography variant="h4">Lanza tu video</Typography>
    <Button component={RouterLink} to={`/login?${new URLSearchParams({ redirect: returnPath })}`}>Iniciar sesión</Button></Stack>;
  return <CampaignWorkspace key={`${session.partyId}:${getAuthSessionEpoch()}`} session={session} />;
}

function CampaignWorkspace({ session }: { session: SessionUser }) {
  const [params] = useSearchParams();
  const admin = hasStrictAdminAccess(session.roles, session.modules);
  const supplied = Number(params.get('artistId'));
  const initialArtist = Number.isSafeInteger(supplied) && supplied > 0 ? supplied : session.partyId ?? 0;
  const [artistId, setArtistId] = useState(initialArtist);
  const [active, setActive] = useState<VideoCampaign | null>(null);
  const [creating, setCreating] = useState(false);
  const [importError, setImportError] = useState('');
  const [importConflict, setImportConflict] = useState<{ plan: VideoCampaign; saved: VideoCampaign } | null>(null);
  const [importing, setImporting] = useState(false);
  const importEpoch = useRef(0);
  const selectionEpoch = useRef(0);
  const selectArtist = (value: number) => {
    selectionEpoch.current += 1;
    setArtistId(value); setActive(null); setCreating(false); setImportConflict(null); setImportError(''); setImporting(false);
  };
  const freshCopy = (campaign: VideoCampaign): VideoCampaign => ({ ...campaign, id: crypto.randomUUID(), revision: 0,
    content: { ...campaign.content, status: 'draft', rightsConfirmed: false, technicalReviewed: false,
      proposedLaunchDate: '', title: `${campaign.content.title.slice(0, 190)} (copia)`,
      pieces: campaign.content.pieces.map((p) => ({ ...p, status: 'draft', publishedUrl: '' })) } });
  const importPlan = async (file: File | undefined, copy = false) => {
    if (!file || getActiveSession() !== session) return;
    const epoch = selectionEpoch.current;
    const ticket = ++importEpoch.current;
    const current = () => getActiveSession() === session && selectionEpoch.current === epoch && importEpoch.current === ticket;
    setImporting(true);
    try {
      if (file.size > 131072) throw new Error('El plan supera el tamaño permitido.');
      const parsed = importedCampaignSchema.parse(JSON.parse(await file.text()));
      if (!current()) return;
      if (parsed.artistId !== artistId) throw new Error('Selecciona el artista al que pertenece este plan.');
      const { artistId: owner, campaignId, ...content } = parsed;
      const invalid = validateVideoCampaign(content); if (invalid) throw new Error(invalid);
      const saved = copy ? undefined : (await VideoCampaigns.list(artistId)).find((row) => row.id === campaignId);
      if (!current()) return;
      const plan: VideoCampaign = { id: campaignId, artistId: owner, revision: 0,
        content: { ...content, status: 'draft', rightsConfirmed: false, technicalReviewed: false },
        createdAt: new Date().toISOString(), updatedAt: new Date().toISOString() };
      if (copy) { setImportConflict(null); setActive(freshCopy(plan)); }
      else if (saved) setImportConflict({ plan, saved });
      else { setImportConflict(null); setActive(plan); }
      setImportError('');
    } catch (e) { if (current()) setImportError(e instanceof Error ? e.message : 'El plan no es válido.'); }
    finally { if (current()) setImporting(false); }
  };
  const artists = useQuery({ queryKey: ['video-campaign-artists', session.partyId, artistId],
    queryFn: () => admin ? Fans.listArtists() : artistId !== session.partyId
      ? Fans.getPublicArtist(String(artistId)).then((p) => [p]) : Fans.getMyArtistProfile().then((p) => [p]), retry: false });
  const campaigns = useQuery({ queryKey: ['video-campaigns', session.partyId, artistId],
    queryFn: () => VideoCampaigns.list(artistId), enabled: Number.isSafeInteger(artistId) && artistId > 0 });
  const accessDenied = campaigns.error instanceof ApiError
    && (campaigns.error.status === 401 || campaigns.error.status === 403 || campaigns.error.status === 404);
  const savedCampaigns = campaigns.data;
  const canEdit = savedCampaigns !== undefined && !accessDenied;
  const metrics = useQuery({ queryKey: ['video-campaign-metrics', session.partyId, artistId, active?.id],
    queryFn: () => VideoCampaigns.metrics(artistId, active!.id), enabled: Boolean(active?.revision), retry: false });
  const artist = artists.data?.find((a) => a.apArtistId === artistId);
  return <Box sx={{ maxWidth: 1000, mx: 'auto', p: { xs: 1, sm: 3 } }}><Stack spacing={3}>
    <Typography variant="h4" component="h1">Lanza tu video</Typography>
    <Typography>Prepara tu sesión y una campaña de 14 días para conseguir registros y seguidores en TDF.</Typography>
    <Alert severity="info">Difusión orgánica · USD 0 de pauta. Guardar y exportar no publican contenido.</Alert>
    {importError && <Alert severity="error">{importError}</Alert>}
    {admin && <>
      <TextField select label="Artista" value={artistId} onChange={(e) => selectArtist(Number(e.target.value))}>
        {!artists.data?.some((a) => a.apArtistId === artistId) && <MenuItem value={artistId}>Artista #{artistId}</MenuItem>}
        {artists.data?.map((a) => <MenuItem key={a.apArtistId} value={a.apArtistId}>{a.apDisplayName}</MenuItem>)}
      </TextField>
      <TextField label="ID del artista" type="number" value={artistId} onChange={(e) => selectArtist(Number(e.target.value))}
        helperText="También permite seleccionar un artista que todavía no tiene perfil público. El servidor verifica tu acceso." />
    </>}
    {campaigns.isError && <Alert severity={canEdit ? 'warning' : 'error'}>{canEdit
      ? 'No pudimos actualizar tus campañas. Tus cambios en el editor se conservan; reintenta la actualización.'
      : 'No pudimos cargar tus campañas.'}
      <Button onClick={() => { void artists.refetch(); void campaigns.refetch(); }}>Reintentar</Button></Alert>}
    {(!Number.isSafeInteger(artistId) || artistId <= 0) && <Alert severity="error">Escribe un ID de artista válido.</Alert>}
    {Number.isSafeInteger(artistId) && artistId > 0 && campaigns.isPending && <CircularProgress aria-label="Cargando campañas" />}
    {importing && <CircularProgress aria-label="Leyendo plan" />}
    {canEdit && <>
      {!artist && <Alert severity="info">Tienes acceso al artista #{artistId}. Sus datos públicos no están disponibles.
        Puedes preparar sus campañas; guardar un borrador no modifica la visibilidad del perfil.</Alert>}
      {importConflict && <Alert severity="warning">
        Ya existe esta campaña: {importConflict.saved.content.title} · revisión {importConflict.saved.revision}.
        Puedes revisar el archivo como un borrador que reemplazará ese contenido al guardar, o crear una copia con enlaces nuevos.
        Ninguna opción guarda ni publica automáticamente.
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <Button onClick={() => {
            setActive({ ...importConflict.saved, content: importConflict.plan.content }); setImportConflict(null);
          }}>Revisar cambios sobre esta campaña</Button>
          <Button onClick={() => { setActive(freshCopy(importConflict.plan)); setImportConflict(null); }}>Importar como copia nueva</Button>
          <Button onClick={() => setImportConflict(null)}>Cancelar importación</Button>
        </Stack>
      </Alert>}
      {!creating && !active && !importConflict && <><Button variant="contained" disabled={importing} onClick={() => setCreating(true)}>Crear campaña</Button>
        <Button component="label" disabled={importing}>Importar plan<input aria-label="Archivo del plan" hidden disabled={importing} type="file" accept="application/json,.json" onChange={(e) => {
          const file = e.target.files?.[0]; e.target.value = ''; void importPlan(file);
        }} /></Button>
        <Button component="label" disabled={importing}>Importar plan como copia nueva<input aria-label="Archivo del plan para copia nueva" hidden disabled={importing} type="file" accept="application/json,.json" onChange={(e) => {
          const file = e.target.files?.[0]; e.target.value = ''; void importPlan(file, true);
        }} /></Button>
        {savedCampaigns.length === 0 && <Typography>Aún no tienes campañas guardadas.</Typography>}
        {savedCampaigns.map((c) => <Card key={c.id}><CardContent><Stack spacing={1}>
          <Typography variant="h6">{c.content.title}</Typography><Typography>{c.content.status === 'ready' ? 'Lista para revisar' : 'Borrador'}</Typography>
          <Button disabled={importing} onClick={() => setActive(c)}>Continuar</Button>
          <Button disabled={importing} onClick={() => setActive(freshCopy(c))}>Duplicar</Button>
        </Stack></CardContent></Card>)}</>}
      {active && active.revision > 0 && <Alert severity={metrics.isError ? 'warning' : 'info'}>{metrics.data
        ? `Registros atribuidos: ${metrics.data.registrations} · Nuevos seguidores atribuidos: ${metrics.data.follows}`
        : metrics.isError ? 'No pudimos cargar las conversiones.' : 'Cargando conversiones…'}</Alert>}
      {metrics.data?.byPiece?.map((p) => <Typography key={`${p.source}:${p.piece}`}>
        {p.source} · {p.piece}: {p.registrations} registros · {p.follows} seguidores nuevos en TDF
      </Typography>)}
      {(creating || active) && <CampaignEditor key={`${artistId}:${active?.id ?? 'new'}`} session={session}
        artistId={artistId} artistProfileId={artist?.apArtistId} artistName={artist?.apDisplayName ?? `Artista #${artistId}`} initial={active}
        onSaved={() => { void campaigns.refetch(); }} onClose={() => { setActive(null); setCreating(false); }} />}
    </>}
  </Stack></Box>;
}

function CampaignEditor({ session, artistId, artistProfileId, artistName, initial, onSaved, onClose }: {
  session: SessionUser; artistId: number; artistProfileId?: number; artistName: string; initial: VideoCampaign | null;
  onSaved: () => void; onClose: () => void;
}) {
  const [id] = useState(() => initial?.id ?? crypto.randomUUID());
  const [revision, setRevision] = useState(initial?.revision ?? 0);
  const [doc, setDoc] = useState<VideoCampaignDocument>(initial?.content ?? emptyVideoCampaign());
  const [step, setStep] = useState(0);
  const [busy, setBusy] = useState(false);
  const [notice, setNotice] = useState('');
  const [error, setError] = useState('');
  const flight = useRef(false);
  const current = () => getActiveSession() === session;
  const patch = (part: Partial<VideoCampaignDocument>) => setDoc((d) => ({ ...d, rightsConfirmed: false, technicalReviewed: false, ...part, status: 'draft' }));
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
    if (flight.current || !current() || doc.status !== 'ready' || !doc.rightsConfirmed || !doc.technicalReviewed || revision < 1) return;
    flight.current = true; setBusy(true); setError(''); setNotice('');
    try {
      await VideoCampaigns.feature(artistId, id, revision);
      if (current()) setNotice('Video destacado actualizado en TDF. La visibilidad del perfil no cambia. No se ha publicado en Instagram ni TikTok.');
    } catch (e) { if (current()) setError(e instanceof Error ? e.message : 'No pudimos actualizar el video del perfil.'); }
    finally { flight.current = false; if (current()) setBusy(false); }
  };
  return <Stack spacing={2}>
    <Stepper activeStep={step} alternativeLabel>{steps.map((s) => <Step key={s}><StepLabel>{s}</StepLabel></Step>)}</Stepper>
    {error && <Alert severity="error">{error}</Alert>}{notice && <Alert severity="success">{notice}</Alert>}
    <Box component="fieldset" disabled={busy} sx={{ border: 0, p: 0, m: 0, minWidth: 0 }}><Stack spacing={2}>
    {step === 0 && <>
      <Typography variant="h6"><ArtistProfileLink partyId={artistProfileId}>{artistName}</ArtistProfileLink></Typography>
      <TextField label="Título de la sesión" value={doc.title} onChange={(e) => patch({ title: e.target.value })} />
      <TextField label="Enlace HTTPS del video completo" value={doc.videoUrl} onChange={(e) => patch({ videoUrl: e.target.value })}
        helperText="Puedes usar tu propio almacenamiento. No modificamos los permisos del enlace." />
      {revision > 0 ? <GoogleDriveUploadWidget label="Subir video original" accept="video/*" multiple={false}
        visibility="private" recoveryScope={`video-campaign:${artistId}:${id}`} formRecoveryAvailable={false}
        helperText="Se conserva privado. Reanuda seleccionando el mismo original; la cuota y el destino los controla tu cuenta. Para una sesión pública, revisa después el enlace y sus permisos."
        onComplete={(files) => {
          if (!current()) return;
          const media = files[0]; const url = media?.publicUrl ?? media?.webContentLink ?? media?.webViewLink;
          if (url && safeVideoUrl(url)) { patch({ videoUrl: url }); setNotice('Original cargado. Revisa la reproducción y los permisos antes de publicar.'); }
          else setError('El original está guardado, pero falta un enlace reproducible. Revisa el archivo en tu almacenamiento y conserva este borrador.');
        }} /> : <Alert severity="info">Guarda primero el borrador para vincular una carga recuperable a esta campaña.</Alert>}
      <TextField label="Duración en segundos" type="number" value={doc.durationSeconds} onChange={(e) => patch({ durationSeconds: Number(e.target.value) })} />
      <TextField label="Enlace HTTPS de portada" value={doc.coverUrl} onChange={(e) => patch({ coverUrl: e.target.value })} />
    </>}
    {step === 1 && <>
      <Typography variant="h6">Conseguir registros y seguidores en TDF</Typography>
      <TextField label="Objetivo principal" value={doc.objective ?? 'Registros y seguidores del artista en TDF'}
        onChange={(e) => patch({ objective: e.target.value })}
        helperText="Puedes priorizar registros o seguidores. El panel mide estas dos acciones dentro de TDF." />
      <Typography>Campaña orgánica. D0 es el día del estreno que elijas después de revisar las piezas.</Typography>
      {(['instagram', 'tiktok'] as SocialChannel[]).map((c) => <FormControlLabel key={c} label={c === 'instagram' ? 'Instagram' : 'TikTok'}
        control={<Checkbox checked={doc.channels.includes(c)} onChange={(_e, checked) => patch({
          channels: checked ? [...doc.channels, c] : doc.channels.filter((v) => v !== c),
          pieces: doc.pieces.filter((p) => checked || p.channel !== c),
        })} />}/>)}
      <TextField label="Fecha propuesta (opcional)" type="date" value={doc.proposedLaunchDate ?? ''}
        slotProps={{ inputLabel: { shrink: true } }} helperText="Solo referencia del borrador. No programa publicaciones."
        onChange={(e) => patch({ proposedLaunchDate: e.target.value })} />
      <Alert severity="info">Publicación manual disponible. Las conexiones de publicación automática aún no están habilitadas en este asistente.</Alert>
      <Button disabled={!doc.channels.some((c) => !doc.pieces.some((p) => p.channel === c))}
        onClick={() => {
          try {
            const pieces = prepareMissingCampaignPieces(artistName, doc.channels, doc.pieces);
            patch({ pieces }); setError('');
          } catch (e) {
            setError(e instanceof Error ? e.message : 'No pudimos preparar el calendario. Las piezas existentes se conservan.');
          }
        }}>Preparar textos y calendario</Button>
    </>}
    {step === 2 && <>
      {!doc.pieces.length && <Alert severity="info">Selecciona las redes y prepara el calendario en el paso anterior.</Alert>}
      {doc.pieces.map((p) => <Card key={p.id}><CardContent><Stack spacing={2}>
        <Typography variant="h6">{p.channel === 'instagram' ? 'Instagram' : 'TikTok'} · {p.format} · D{p.day >= 0 ? '+' : ''}{p.day}</Typography>
        <TextField label="Día relativo al estreno" type="number" value={p.day} onChange={(e) => patchPiece(p.id, { day: Number(e.target.value) })} />
        <TextField multiline minRows={3} label="Texto de la publicación" value={p.caption} onChange={(e) => patchPiece(p.id, { caption: e.target.value })} />
        <TextField label="Enlace de la pieza audiovisual" value={p.assetUrl} onChange={(e) => patchPiece(p.id, { assetUrl: e.target.value })} />
        <Box component="details">
        <Box component="summary" sx={{ cursor: 'pointer' }}>Opciones avanzadas: cortes y registro manual</Box>
        <Stack spacing={2} sx={{ pt: 2 }}>
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
        </Stack>
        </Box>
        <Button onClick={() => { void navigator.clipboard.writeText(`${p.caption}\n${campaignLink(artistId, id, p)}`)
          .then(() => setNotice('Texto y enlace copiados.'))
          .catch(() => setError('No pudimos copiar. Puedes seleccionar el texto o descargar el plan.')); }}>Copiar texto y enlace</Button>
        {p.assetUrl && safeVideoUrl(p.assetUrl) && <Button component="a" href={p.assetUrl} target="_blank" rel="noopener noreferrer">Abrir archivo de la pieza</Button>}
      </Stack></CardContent></Card>)}
    </>}
    {step === 3 && <>
      <Typography variant="h6">{doc.title}</Typography>
      <Typography>Objetivo: {doc.objective ?? 'Registros y seguidores del artista en TDF'}</Typography>
      <Typography><ArtistProfileLink partyId={artistProfileId}>{artistName}</ArtistProfileLink> · {doc.pieces.length} piezas · 14 días · USD 0 de pauta</Typography>
      <Typography sx={{ overflowWrap: 'anywhere' }}>{doc.videoUrl || 'Falta el enlace del video completo.'}</Typography>
      <SessionVideoPreview url={doc.videoUrl} title={doc.title} />
      <TextField multiline minRows={3} label="Créditos confirmados" value={doc.credits ?? ''}
        helperText="Deja pendientes los cargos o nombres que no hayas verificado."
        onChange={(e) => patch({ credits: e.target.value, rightsConfirmed: false, technicalReviewed: false })} />
      <FormControlLabel label="Confirmo los permisos de música, imagen y video para publicar en TDF"
        control={<Checkbox checked={doc.rightsConfirmed ?? false} onChange={(_e, checked) => patch({ rightsConfirmed: checked, technicalReviewed: doc.technicalReviewed })} />} />
      <FormControlLabel label="Revisé el video completo, el audio, los cortes y los créditos"
        control={<Checkbox checked={doc.technicalReviewed ?? false} onChange={(_e, checked) => patch({ technicalReviewed: checked, rightsConfirmed: doc.rightsConfirmed })} />} />
      <Typography>Revisa cortes, textos, permisos del video, créditos y enlaces antes de publicar desde cada red.</Typography>
      <Typography>Los enlaces incluyen atribución por pieza. Las conversiones verificadas se muestran al abrir una campaña guardada. Las métricas externas requieren una conexión autorizada.</Typography>
      <Button onClick={exportPlan}>Descargar plan y textos</Button>
      <Button variant="contained" onClick={() => void save(true)}>Guardar para revisión</Button>
      {doc.status === 'ready' && doc.rightsConfirmed && doc.technicalReviewed && revision > 0 && <>
        <Typography>La siguiente acción actualiza el video destacado del perfil de TDF. Si el perfil es público, se verá allí; un perfil privado conserva su visibilidad.</Typography>
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
