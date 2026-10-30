import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Stack, TextField, Typography } from '@mui/material';
import { getActiveSession, useSession } from '../session/SessionContext';
import { musicReleases, type ArtistFeaturedTrack } from '../api/musicReleases';
import type { PlayerTrack } from './types';

export function featuredSelectionKey(selection: Pick<ArtistFeaturedTrack, 'recordingId' | 'legacyReleaseId'>): string {
  return selection.recordingId ? `recording:${selection.recordingId}`
    : selection.legacyReleaseId ? `legacy-release-${selection.legacyReleaseId}` : '';
}

export default function ArtistFeaturedTrackEditor({ artistId, tracks }: { artistId: number; tracks: PlayerTrack[] }) {
  const { session, loading } = useSession();
  const [baseline, setBaseline] = useState<ArtistFeaturedTrack | null>(null);
  const [draft, setDraft] = useState('');
  const [failure, setFailure] = useState(false);
  const [saving, setSaving] = useState(false);
  const [saved, setSaved] = useState(false);
  const [attempt, setAttempt] = useState(0);
  const generation = useRef(0);
  const write = useRef<AbortController | null>(null);
  const owner = !loading && session?.partyId === artistId;
  useEffect(() => {
    const current = ++generation.current;
    const controller = new AbortController();
    setBaseline(null); setFailure(false); setSaved(false); setSaving(false);
    if (owner) void musicReleases.getMyFeaturedTrack(controller.signal).then((selection) => {
      if (controller.signal.aborted || current !== generation.current || getActiveSession() !== session) return;
      if (selection.artistPartyId !== artistId) { setFailure(true); return; }
      setBaseline(selection); setDraft(featuredSelectionKey(selection));
    }).catch(() => { if (!controller.signal.aborted && current === generation.current) setFailure(true); });
    return () => { controller.abort(); write.current?.abort(); generation.current += 1; };
  }, [artistId, attempt, owner, session]);
  if (!owner) return null;
  const choices = tracks.map((track) => ({ key: track.recordingId ? `recording:${track.recordingId}` : track.id, track }));
  const missingSelection = baseline && featuredSelectionKey(baseline) && !choices.some((choice) => choice.key === featuredSelectionKey(baseline));
  const save = async () => {
    if (!baseline || saving || getActiveSession() !== session) return;
    const choice = choices.find((item) => item.key === draft);
    if (draft && !choice) return;
    const controller = new AbortController();
    write.current?.abort(); write.current = controller;
    const current = generation.current;
    setSaving(true); setFailure(false); setSaved(false);
    try {
      const selection = await musicReleases.setMyFeaturedTrack({ artistPartyId: artistId,
        recordingId: choice?.track.recordingId ?? null,
        legacyReleaseId: choice && !choice.track.recordingId ? Number(choice.track.id.slice('legacy-release-'.length)) : null,
      }, controller.signal);
      if (controller.signal.aborted || current !== generation.current || getActiveSession() !== session) return;
      if (selection.artistPartyId !== artistId) { setFailure(true); return; }
      setBaseline(selection); setDraft(featuredSelectionKey(selection)); setSaved(true);
    } catch { if (!controller.signal.aborted && current === generation.current && getActiveSession() === session) setFailure(true); }
    finally { if (!controller.signal.aborted && current === generation.current) setSaving(false); }
  };
  return <Stack spacing={1} sx={{ py: 2 }}>
    <Typography variant="subtitle2">Música al abrir tu perfil</Typography>
    {failure && <Alert severity="warning" action={!baseline && <Button onClick={() => setAttempt((value) => value + 1)}>Reintentar</Button>}>
      No pudimos confirmar tu tema destacado. Vuelve a intentarlo.
    </Alert>}
    {missingSelection && <Alert severity="info">El tema guardado ya no está disponible en este catálogo. Puedes cambiarlo o volver al orden normal.</Alert>}
    <TextField label="Tema destacado" select SelectProps={{ native: true }} value={baseline ? draft : ''}
      disabled={!baseline || saving} onChange={(event) => { setDraft(event.target.value); setSaved(false); }}>
      <option value="">Orden normal del catálogo</option>
      {missingSelection && <option value={featuredSelectionKey(baseline)} disabled>Selección anterior no disponible</option>}
      {choices.map(({ key, track }) => <option key={key} value={key}>{track.title}</option>)}
    </TextField>
    <Button onClick={() => { void save(); }} disabled={!baseline || saving || draft === featuredSelectionKey(baseline)}>
      {saving ? 'Guardando…' : 'Guardar tema destacado'}
    </Button>
    {saved && <Typography role="status" variant="body2">Tema destacado guardado. Se usará al abrir nuevamente tu perfil.</Typography>}
  </Stack>;
}
