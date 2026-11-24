// MEDIA-MUSIC-LOUDNESS-001: advisory loudness result of one track's newest
// master. It never disables an action; a missing result renders nothing.
import { Alert, Typography } from '@mui/material';

import type { MusicLoudnessCheck, MusicLoudnessTrack } from '../../api/musicLoudness';

type Limits = MusicLoudnessCheck['limits'];

interface MasterCandidate { id: string; recordingId: string | null; role: string; processingState: string; createdAt: string }

// The same choice the server makes: the newest master of the recording that
// was not deleted. A result for any other file is not shown for the track.
export function newestMasterAssetId(assets: readonly MasterCandidate[], recordingId: string | null | undefined): string | null {
  let newest: MasterCandidate | null = null;
  for (const asset of assets) {
    if (asset.role !== 'master_audio' || asset.recordingId !== recordingId || asset.processingState === 'deleted') continue;
    const order = newest === null ? 1 : Date.parse(asset.createdAt) - Date.parse(newest.createdAt);
    if (newest === null || order > 0 || (order === 0 && asset.id > newest.id)) newest = asset;
  }
  return newest?.id ?? null;
}

export const formatTenths = (tenths: number): string => {
  const magnitude = Math.abs(tenths);
  return `${tenths < 0 ? '−' : ''}${Math.floor(magnitude / 10)}.${magnitude % 10}`;
};

export function loudnessMessages(track: MusicLoudnessTrack, limits: Limits): string[] {
  const target = formatTenths(limits.integratedLufsTenths);
  const band = formatTenths(limits.toleranceTenths);
  return track.flags.map((flag) => {
    if (flag === 'too_loud') {
      return `Suena más fuerte que el objetivo de ${target} LUFS (±${band}); las plataformas le bajarán el volumen.`;
    }
    if (flag === 'too_quiet') {
      return `Suena más bajo que el objetivo de ${target} LUFS (±${band}).`;
    }
    return `El pico real supera ${formatTenths(limits.truePeakCeilingDbtpTenths)} dBTP y puede distorsionar al codificarse.`;
  });
}

export function loudnessFailureText(reason: string | null | undefined): string {
  if (reason === 'timeout') return 'la medición superó el tiempo límite';
  if (reason === 'silent') return 'el archivo no contiene señal audible';
  if (reason === 'decode_error') return 'el archivo no se pudo decodificar por completo';
  return 'la herramienta de medición no entregó un resultado';
}

export default function TrackLoudnessCheck({ track, limits, label, currentMasterAssetId }: {
  track: MusicLoudnessTrack | undefined;
  limits: Limits | undefined;
  label: string;
  currentMasterAssetId: string | null;
}) {
  // A result fetched before a re-upload describes another file: show nothing.
  if (!track || !limits || track.masterAssetId === null || track.masterAssetId !== currentMasterAssetId) return null;
  if (track.status === 'pending') {
    return <Typography variant="body2" color="text.secondary" role="status">Midiendo la sonoridad del máster de {label}…</Typography>;
  }
  if (track.status === 'failed') {
    return (
      <Alert severity="info" role="status">
        No se pudo medir la sonoridad de {label}: {loudnessFailureText(track.failureReason)}. Esto no bloquea la publicación.
      </Alert>
    );
  }
  if (!track.checked || track.integratedLufsTenths == null || track.truePeakDbtpTenths == null) return null;
  const measured = `Sonoridad de ${label}: ${formatTenths(track.integratedLufsTenths)} LUFS, pico real ${formatTenths(track.truePeakDbtpTenths)} dBTP.`;
  if (track.status === 'pass') {
    return <Typography variant="body2" color="text.secondary" role="status">{measured} Dentro del objetivo.</Typography>;
  }
  return (
    <Alert severity="warning" role="status">
      {measured} {loudnessMessages(track, limits).join(' ')} Es una advertencia: no bloquea la publicación.
    </Alert>
  );
}
