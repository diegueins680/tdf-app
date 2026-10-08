import AddIcon from '@mui/icons-material/Add';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import CloudUploadIcon from '@mui/icons-material/CloudUpload';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import SaveIcon from '@mui/icons-material/Save';
import SendIcon from '@mui/icons-material/Send';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  Divider,
  FormControlLabel,
  IconButton,
  LinearProgress,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { useCallback, useEffect, useRef, useState } from 'react';
import { Link as RouterLink, useNavigate, useParams, useSearchParams } from 'react-router-dom';

import {
  musicReleases,
  type MusicPartyDraftInput,
  type MusicInfringementReport,
  type MusicReleaseContentInput,
  type MusicReleaseKind,
  type MusicReleaseMetadataDraft,
  type MusicReleaseVersion,
  type MusicTrackDraftInput,
} from '../api/musicReleases';
import { Catalogs } from '../api/catalogs';
import { useDocumentTitle } from '../hooks/useDocumentTitle';
import { useSession } from '../session/SessionContext';
import { musicPreviewRangeError } from '../utils/musicPreviewRange';

type DraftContent = Omit<MusicReleaseContentInput, 'expectedUpdatedAt'>;
type DraftMetadata = Omit<MusicReleaseMetadataDraft, 'expectedUpdatedAt'>;
type AvailabilityDraft = MusicReleaseContentInput['availability'][number];
interface UploadState { progress: number; message: string; error?: string }
type Raw = Record<string, unknown>;

const asRaw = (value: unknown): Raw => value && typeof value === 'object' ? value as Raw : {};
const text = (value: unknown): string => typeof value === 'string' ? value : '';
const nullableText = (value: unknown): string | null => {
  const valueText = text(value).trim();
  return valueText || null;
};
const numberValue = (value: unknown, fallback = 0): number => typeof value === 'number' ? value : fallback;
const stringArray = (value: unknown): string[] => Array.isArray(value) ? value.filter((item): item is string => typeof item === 'string') : [];
const newRef = (prefix: string) => `${prefix}-${globalThis.crypto?.randomUUID?.() ?? Date.now().toString(36)}`;
const today = () => new Date().toISOString().slice(0, 10);
export const musicReleaseSlug = (value: string) => value.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase()
  .replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '').replace(/-{2,}/g, '').slice(0, 160);

const emptyMetadata = (displayName: string): DraftMetadata => ({
  title: '', subtitle: null, versionTitle: null, displayArtist: displayName,
  titleLanguage: 'es', titleScript: null, explicitContent: 'unknown', originalReleaseDate: null,
  primaryGenreId: null, secondaryGenreId: null,
  releaseAtUtc: null, releaseTimezone: null, embargoUntilUtc: null, labelName: null,
  catalogNumber: null, recordingCopyrightText: null, workCopyrightText: null,
});

export const defaultMusicReleaseContent = (displayName: string): DraftContent => {
  const trackRef = newRef('track');
  const partyRef = newRef('party');
  const party: MusicPartyDraftInput = {
    clientRef: partyRef, partyId: null, tdfPartyId: null, displayName,
    legalName: null, partyKind: 'person', identifiers: [],
  };
  const track: MusicTrackDraftInput = {
    clientRef: trackRef, recordingId: null, title: '', subtitle: null, versionTitle: null,
    titleLanguage: 'es', titleScript: null, explicitContent: 'unknown', discNumber: 1,
    trackNumber: 1, displayArtist: displayName, isPrimaryResource: true,
    previewStartMs: null, previewDurationMs: null,
  };
  return {
    tracks: [track], parties: [party],
    credits: [
      { partyRef, trackRef, role: 'main_artist', displayOrder: 0, notes: null },
      { partyRef, trackRef, role: 'composer', displayOrder: 1, notes: null },
    ],
    identifiers: [],
    rightsDeclarations: ['master', 'composition'].map((scope) => ({
      trackRef, scope: scope as 'master' | 'composition', authorityBasis: '', territories: ['Worldwide'],
      startsOn: today(), endsOn: null,
      splits: [{ partyRef, basisPoints: 10000, territories: ['Worldwide'], startsOn: today(), endsOn: null }],
    })),
    availability: [{
      trackRef: null, territoryMode: 'include', territories: ['Worldwide'], startsAt: null, endsAt: null,
      listeningPolicy: 'full', downloadPolicy: 'none', purchasable: false,
      priceMinor: null, currency: null, downloadableAssetId: null,
    }],
  };
};

export const musicReleaseDraftFromVersion = (version: MusicReleaseVersion): { metadata: DraftMetadata; content: DraftContent } => {
  const metadata: DraftMetadata = {
    title: version.title, subtitle: version.subtitle, versionTitle: version.versionTitle,
    displayArtist: version.displayArtist, titleLanguage: version.titleLanguage, titleScript: version.titleScript,
    primaryGenreId: version.primaryGenreId, secondaryGenreId: version.secondaryGenreId,
    explicitContent: version.explicitContent, originalReleaseDate: version.originalReleaseDate,
    releaseAtUtc: version.releaseAtUtc, releaseTimezone: version.releaseTimezone,
    embargoUntilUtc: version.embargoUntilUtc, labelName: version.labelName,
    catalogNumber: version.catalogNumber, recordingCopyrightText: version.recordingCopyrightText,
    workCopyrightText: version.workCopyrightText,
  };
  if (version.tracks.length === 0) return { metadata, content: defaultMusicReleaseContent(version.displayArtist) };

  const trackRefByRecording = new Map(version.tracks.map((track) => [track.recordingId, `track-${track.recordingId}`]));
  const trackRefByTrackId = new Map(version.tracks.map((track) => [track.trackId, `track-${track.recordingId}`]));
  const parties = version.parties.map((value) => {
    const party = asRaw(value);
    const identifiers = Array.isArray(party['identifiers']) ? party['identifiers'].map((entry) => {
      const identifier = asRaw(entry);
      return { type: text(identifier['identifier_type']) as MusicPartyDraftInput['identifiers'][number]['type'], value: text(identifier['identifier_value']) };
    }) : [];
    return {
      clientRef: `party-${text(party['id'])}`, partyId: nullableText(party['id']),
      tdfPartyId: !nullableText(party['id']) && typeof party['tdfPartyId'] === 'number' ? party['tdfPartyId'] : null,
      displayName: text(party['displayName']), legalName: nullableText(party['legalName']),
      partyKind: (text(party['partyKind']) || 'unknown') as MusicPartyDraftInput['partyKind'], identifiers,
    };
  });
  const partyRefById = new Map(parties.map((party) => [party.partyId, party.clientRef]));
  const tracks: MusicTrackDraftInput[] = version.tracks.map((track) => ({
    clientRef: trackRefByRecording.get(track.recordingId) ?? newRef('track'), recordingId: track.recordingId,
    title: track.title, subtitle: track.subtitle, versionTitle: track.versionTitle,
    titleLanguage: track.titleLanguage, titleScript: track.titleScript, explicitContent: track.explicitContent,
    discNumber: track.discNumber, trackNumber: track.trackNumber, displayArtist: track.displayArtist,
    isPrimaryResource: track.isPrimaryResource, previewStartMs: track.previewStartMs,
    previewDurationMs: track.previewDurationMs,
  }));
  const credits = version.credits.map((value) => {
    const credit = asRaw(value);
    return {
      partyRef: partyRefById.get(nullableText(credit['music_party_id'])) ?? '',
      trackRef: trackRefByRecording.get(text(credit['recording_id'])) ?? null,
      role: text(credit['credit_role']), displayOrder: numberValue(credit['display_order']), notes: nullableText(credit['notes']),
    };
  }).filter((credit) => credit.partyRef);
  const identifiers = version.identifiers.map((value) => {
    const identifier = asRaw(value);
    return {
      trackRef: trackRefByRecording.get(text(identifier['recording_id'])) ?? null,
      type: text(identifier['identifier_type']) as MusicReleaseContentInput['identifiers'][number]['type'],
      value: text(identifier['identifier_value']),
    };
  });
  const rightsDeclarations = version.rights.map((value) => {
    const rights = asRaw(value);
    const splits = Array.isArray(rights['splits']) ? rights['splits'].map((entry) => {
      const split = asRaw(entry);
      return {
        partyRef: partyRefById.get(nullableText(split['rights_holder_id'])) ?? '',
        basisPoints: numberValue(split['basis_points']), territories: stringArray(split['territories']),
        startsOn: text(split['starts_on']), endsOn: nullableText(split['ends_on']),
      };
    }).filter((split) => split.partyRef) : [];
    return {
      trackRef: trackRefByRecording.get(text(rights['recording_id'])) ?? null,
      scope: text(rights['rights_scope']) as 'master' | 'composition', authorityBasis: text(rights['authority_basis']),
      territories: stringArray(rights['territories']), startsOn: text(rights['starts_on']), endsOn: nullableText(rights['ends_on']), splits,
    };
  });
  const availability = version.availability.map((value) => {
    const rule = asRaw(value);
    return {
      trackRef: trackRefByTrackId.get(text(rule['release_track_id'])) ?? null,
      territoryMode: text(rule['territory_mode']) as 'include' | 'exclude', territories: stringArray(rule['territories']),
      startsAt: nullableText(rule['starts_at']), endsAt: nullableText(rule['ends_at']),
      listeningPolicy: text(rule['listening_policy']) as 'none' | 'preview' | 'full',
      downloadPolicy: text(rule['download_policy']) as 'none' | 'free' | 'purchase',
      purchasable: rule['purchasable'] === true, priceMinor: typeof rule['price_minor'] === 'number' ? rule['price_minor'] : null,
      currency: nullableText(rule['currency']), downloadableAssetId: nullableText(rule['downloadable_asset_id']),
    };
  });
  return {
    metadata,
    content: {
      tracks, parties: parties.length > 0 ? parties : defaultMusicReleaseContent(version.displayArtist).parties,
      credits, identifiers, rightsDeclarations, availability,
    },
  };
};

function UploadProgress({ state }: { state?: UploadState }) {
  if (!state) return null;
  return <>
    <LinearProgress variant="determinate" value={state.progress} />
    <Typography variant="caption" color={state.error ? 'error' : 'text.secondary'}>
      {state.error ?? state.message}
    </Typography>
  </>;
}

export default function MusicReleaseStudioPage() {
  useDocumentTitle('Estudio de lanzamientos');
  const { releaseId, versionId } = useParams();
  return releaseId && versionId
    ? <ReleaseEditor key={`${releaseId}:${versionId}`} releaseId={releaseId} versionId={versionId} />
    : <ReleaseIndex />;
}

function ReleaseIndex() {
  const navigate = useNavigate();
  const { session } = useSession();
  const [searchParams] = useSearchParams();
  const requestedArtistId = Number(searchParams.get('artistPartyId'));
  const initialArtist = Number.isSafeInteger(requestedArtistId) && requestedArtistId > 0
    ? requestedArtistId
    : session?.partyId ?? 0;
  const [artistPartyId, setArtistPartyId] = useState(initialArtist);
  const [kind, setKind] = useState<MusicReleaseKind>('single');
  const [title, setTitle] = useState('');
  const [displayArtist, setDisplayArtist] = useState(session?.displayName ?? '');
  const [slug, setSlug] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [creating, setCreating] = useState(false);
  const releases = useQuery({
    queryKey: ['music-releases', 'mine', artistPartyId],
    queryFn: () => musicReleases.listMine(artistPartyId || undefined),
    enabled: artistPartyId > 0,
    retry: false,
  });

  const create = async () => {
    setCreating(true); setError(null);
    try {
      const created = await musicReleases.create({
        artistPartyId, canonicalSlug: slug || musicReleaseSlug(title), releaseKind: kind,
        title: title.trim(), displayArtist: displayArtist.trim(), titleLanguage: 'es',
      }, newRef('music-create'));
      navigate(`/label/releases/${created.id}/versions/${created.versionId}?artistPartyId=${artistPartyId}`);
    } catch (reason) {
      setError(reason instanceof Error ? reason.message : 'No se pudo crear el lanzamiento.');
    } finally { setCreating(false); }
  };

  return <Stack spacing={3}>
    <Stack spacing={0.5}>
      <Typography variant="h4" fontWeight={700}>Estudio de lanzamientos</Typography>
      <Typography color="text.secondary">Catálogo canónico, derechos, assets privados y revisión editorial.</Typography>
    </Stack>
    {error && <Alert severity="error">{error}</Alert>}
    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h6">Crear lanzamiento canónico</Typography>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
        <TextField label="Party ID del artista verificado" type="number" value={artistPartyId || ''}
          onChange={(event) => setArtistPartyId(Number(event.target.value))} required />
        <TextField select label="Tipo" value={kind} onChange={(event) => setKind(event.target.value as MusicReleaseKind)}>
          <MenuItem value="single">Single</MenuItem><MenuItem value="ep">EP</MenuItem><MenuItem value="album">Álbum</MenuItem>
        </TextField>
        <TextField label="Artista visible" value={displayArtist} onChange={(event) => setDisplayArtist(event.target.value)} required fullWidth />
      </Stack>
      <TextField label="Título" value={title} onChange={(event) => { setTitle(event.target.value); setSlug(musicReleaseSlug(event.target.value)); }} required />
      <TextField label="Slug canónico" value={slug} onChange={(event) => setSlug(musicReleaseSlug(event.target.value))} helperText="La URL pública será estable." />
      <Box><Button variant="contained" startIcon={<AddIcon />} onClick={() => void create()}
        disabled={creating || artistPartyId <= 0 || !title.trim() || !displayArtist.trim() || !(slug || musicReleaseSlug(title))}>
        {creating ? 'Creando…' : 'Crear borrador'}
      </Button></Box>
    </Stack></CardContent></Card>
    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h6">Borradores y versiones</Typography>
      {releases.isLoading && <LinearProgress />}
      {releases.isError && <Alert severity="warning">El catálogo canónico no está habilitado o no tienes permiso para este artista.</Alert>}
      {releases.data?.map((release) => <Stack key={release.id} direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
        <Box flex={1}><Typography fontWeight={600}>{release.versions[0]?.title ?? release.slug}</Typography>
          <Typography variant="body2" color="text.secondary">{release.kind.toUpperCase()} · /musica/{release.slug}</Typography></Box>
        {release.versions.map((version) => <Button key={version.id} component={RouterLink}
          to={`/label/releases/${release.id}/versions/${version.id}?artistPartyId=${release.artistPartyId}`} variant="outlined" size="small">
          v{version.number} · {version.state}
        </Button>)}
      </Stack>)}
      {releases.data?.length === 0 && <Typography color="text.secondary">Aún no hay versiones canónicas para este artista.</Typography>}
    </Stack></CardContent></Card>
    <Button component={RouterLink} to="/label/releases" startIcon={<ArrowBackIcon />}>Volver al catálogo legado</Button>
  </Stack>;
}

function ReleaseEditor({ releaseId, versionId }: { releaseId: string; versionId: string }) {
  const { session } = useSession();
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const requestedArtistId = Number(searchParams.get('artistPartyId'));
  const artistPartyId = Number.isSafeInteger(requestedArtistId) && requestedArtistId > 0
    ? requestedArtistId
    : session?.partyId ?? 0;
  const version = useQuery({
    queryKey: ['music-release-version', releaseId, versionId], queryFn: () => musicReleases.getVersion(releaseId, versionId),
    retry: false, refetchOnWindowFocus: false,
  });
  const genres = useQuery({
    queryKey: ['catalog', 'genres', 'music-release-studio'],
    queryFn: () => Catalogs.listPublicItems('genres', { locale: 'es', page: 1, pageSize: 500 }),
    retry: false,
  });
  const [metadata, setMetadata] = useState<DraftMetadata>(() => emptyMetadata(session?.displayName ?? ''));
  const [content, setContent] = useState<DraftContent>(() => defaultMusicReleaseContent(session?.displayName ?? ''));
  const [dirty, setDirty] = useState(false);
  const [status, setStatus] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [acceptedTerms, setAcceptedTerms] = useState(false);
  const [uploads, setUploads] = useState<Record<string, UploadState>>({});
  const [scheduleLocal, setScheduleLocal] = useState('');
  const [takedownLocal, setTakedownLocal] = useState('');
  const [reviewField, setReviewField] = useState('');
  const [reviewBody, setReviewBody] = useState('');
  const [reviewStaffOnly, setReviewStaffOnly] = useState(false);
  const [ddexOperation, setDdexOperation] = useState<'new_release' | 'update' | 'takedown'>('new_release');
  const [ddexSenderId, setDdexSenderId] = useState('');
  const [ddexRecipientId, setDdexRecipientId] = useState('');
  const [ddexPartyName, setDdexPartyName] = useState('');
  const [ddexPartyDpid, setDdexPartyDpid] = useState('');
  const [ddexPartyRole, setDdexPartyRole] = useState<'sender' | 'recipient' | 'both'>('recipient');
  const [ddexPartyAuthority, setDdexPartyAuthority] = useState('');
  const [ddexPartyEvidence, setDdexPartyEvidence] = useState('');
  const [actionPending, setActionPending] = useState(false);
  const [analyticsFrom, setAnalyticsFrom] = useState('');
  const [analyticsTo, setAnalyticsTo] = useState('');
  const [infringementNotes, setInfringementNotes] = useState('');
  const [suspendOnAction, setSuspendOnAction] = useState(true);
  const initializedVersion = useRef<string | null>(null);
  const saving = useRef(false);
  const autosaveTimer = useRef<number | null>(null);
  const operationPending = useRef(false);
  const [isSaving, setIsSaving] = useState(false);
  const updatedAt = useRef('');
  const canEdit = version.data ? ['draft','uploading','processing','validation_failed','changes_requested'].includes(version.data.state) : false;
  const editable = canEdit && !isSaving && !actionPending;
  const isAdmin = session?.roles.some((role) => ['admin','superadmin','owner'].includes(role.toLowerCase())) ?? false;
  const analytics = useQuery({
    queryKey: ['music-release-analytics', releaseId, analyticsFrom, analyticsTo],
    queryFn: () => musicReleases.analytics(releaseId, analyticsFrom || undefined, analyticsTo || undefined),
    enabled: artistPartyId > 0, retry: false,
  });
  const ddexParties = useQuery({
    queryKey: ['music-ddex-parties'], queryFn: () => musicReleases.listDdexParties(),
    enabled: isAdmin, retry: false,
  });
  const ddexExports = useQuery({
    queryKey: ['music-ddex-exports', releaseId, versionId],
    queryFn: () => musicReleases.listDdexExports(releaseId, versionId), enabled: isAdmin, retry: false,
  });
  const infringementReports = useQuery({
    queryKey: ['music-infringement-reports', releaseId],
    queryFn: () => musicReleases.listInfringementReports(releaseId), enabled: isAdmin, retry: false,
  });

  useEffect(() => {
    if (!version.data || initializedVersion.current === version.data.id) return;
    const draft = musicReleaseDraftFromVersion(version.data);
    setMetadata(draft.metadata); setContent(draft.content); updatedAt.current = version.data.updatedAt;
    setAcceptedTerms(version.data.terms.some((entry) => text(asRaw(entry)['terms_kind']) === 'publication_authority'));
    setDirty(false); setStatus(''); setError(null);
    initializedVersion.current = version.data.id;
  }, [version.data]);

  const applyVersion = useCallback((next: MusicReleaseVersion) => {
    updatedAt.current = next.updatedAt;
    queryClient.setQueryData(['music-release-version', releaseId, versionId], next);
  }, [queryClient, releaseId, versionId]);

  const persistDraft = useCallback(async () => {
    if (autosaveTimer.current !== null) {
      window.clearTimeout(autosaveTimer.current);
      autosaveTimer.current = null;
    }
    // Fail closed: an operation must never proceed using a snapshot whose
    // persistence failed or is still in flight. Content assigns server IDs,
    // so freeze editing until both transactions have acknowledged this graph.
    if (saving.current) return false;
    if (!dirty) return true;
    if (!canEdit || !updatedAt.current) return false;
    saving.current = true; setIsSaving(true); setStatus('Guardando…'); setError(null);
    try {
      for (const [index, track] of content.tracks.entries()) {
        const rangeError = musicPreviewRangeError(track.previewStartMs, track.previewDurationMs);
        if (rangeError) throw new Error(`Pista ${index + 1}: ${rangeError}`);
      }
      const metadataResult = await musicReleases.saveMetadata(releaseId, versionId, { ...metadata, expectedUpdatedAt: updatedAt.current });
      // Metadata and content are separate transactions. Preserve the successful
      // revision even if progressive content validation rejects the next call.
      updatedAt.current = metadataResult.updatedAt;
      const contentResult = await musicReleases.saveContent(releaseId, versionId, { ...content, expectedUpdatedAt: metadataResult.updatedAt });
      applyVersion(contentResult); setContent(musicReleaseDraftFromVersion(contentResult).content);
      setDirty(false); setStatus('Borrador guardado automáticamente.');
      return true;
    } catch (reason) {
      setError(reason instanceof Error ? reason.message : 'No se pudo guardar el borrador.');
      setStatus('');
      return false;
    } finally { saving.current = false; setIsSaving(false); }
  }, [applyVersion, content, dirty, canEdit, metadata, releaseId, versionId]);

  useEffect(() => {
    if (!dirty || !canEdit) return undefined;
    const timer = window.setTimeout(() => { void persistDraft(); }, 1600);
    autosaveTimer.current = timer;
    return () => {
      window.clearTimeout(timer);
      if (autosaveTimer.current === timer) autosaveTimer.current = null;
    };
  }, [dirty, canEdit, persistDraft]);

  const changeMetadata = <K extends keyof DraftMetadata>(key: K, value: DraftMetadata[K]) => {
    setMetadata((current) => ({ ...current, [key]: value })); setDirty(true);
  };
  const changeTrack = <K extends keyof MusicTrackDraftInput>(index: number, key: K, value: MusicTrackDraftInput[K]) => {
    setContent((current) => ({ ...current, tracks: current.tracks.map((track, at) => at === index ? { ...track, [key]: value } : track) }));
    setDirty(true);
  };
  const changeParty = (index: number, patch: Partial<MusicPartyDraftInput>) => {
    setContent((current) => ({ ...current, parties: current.parties.map((party, at) => at === index ? { ...party, ...patch } : party) }));
    setDirty(true);
  };
  const changeAvailability = (index: number, patch: Partial<AvailabilityDraft>) => {
    setContent((current) => ({
      ...current,
      availability: current.availability.map((item, at) => at === index ? { ...item, ...patch } : item),
    }));
    setDirty(true);
  };

  const addTrack = () => {
    const trackRef = newRef('track');
    const firstParty = content.parties[0]?.clientRef;
    const trackNumber = content.tracks.length + 1;
    const template = defaultMusicReleaseContent(metadata.displayArtist).tracks[0];
    if (!template) return;
    setContent((current) => ({
      ...current,
      tracks: [...current.tracks, { ...template, clientRef: trackRef, trackNumber, displayArtist: metadata.displayArtist }],
      credits: firstParty ? [...current.credits,
        { partyRef: firstParty, trackRef, role: 'main_artist', displayOrder: 0, notes: null },
        { partyRef: firstParty, trackRef, role: 'composer', displayOrder: 1, notes: null }] : current.credits,
      rightsDeclarations: firstParty ? [...current.rightsDeclarations,
        ...(['master','composition'] as const).map((scope) => ({
          trackRef, scope, authorityBasis: '', territories: ['Worldwide'], startsOn: today(), endsOn: null,
          splits: [{ partyRef: firstParty, basisPoints: 10000, territories: ['Worldwide'], startsOn: today(), endsOn: null }],
        }))] : current.rightsDeclarations,
    })); setDirty(true);
  };

  const removeTrack = (trackRef: string) => {
    setContent((current) => ({
      ...current, tracks: current.tracks.filter((track) => track.clientRef !== trackRef),
      credits: current.credits.filter((credit) => credit.trackRef !== trackRef),
      identifiers: current.identifiers.filter((identifier) => identifier.trackRef !== trackRef),
      rightsDeclarations: current.rightsDeclarations.filter((rights) => rights.trackRef !== trackRef),
      availability: current.availability.filter((rule) => rule.trackRef !== trackRef),
    })); setDirty(true);
  };

  const addParty = () => {
    setContent((current) => ({ ...current, parties: [...current.parties, {
      clientRef: newRef('party'), partyId: null, tdfPartyId: null, displayName: '', legalName: null,
      partyKind: 'person', identifiers: [],
    }] })); setDirty(true);
  };

  const upload = async (key: string, file: File, role: 'master_audio' | 'cover_original', recordingId?: string | null) => {
    if (operationPending.current || saving.current) return;
    operationPending.current = true; setActionPending(true);
    try {
      if (!await persistDraft()) return;
      setUploads((current) => ({ ...current, [key]: { progress: 0, message: 'Calculando checksum…' } }));
      await musicReleases.uploadAsset({ releaseId, versionId, recordingId, assetRole: role, file,
        idempotencyKey: newRef('music-upload'),
        onProgress: (progress) => setUploads((current) => ({ ...current, [key]: {
          progress: progress.totalBytes ? Math.round(progress.completedBytes * 100 / progress.totalBytes) : 0,
          message: progress.phase === 'hashing' ? 'Calculando checksum…' : progress.phase === 'uploading' ? 'Subiendo directamente al almacenamiento…' : 'Confirmando…',
        } })),
      });
      const refreshed = await musicReleases.getVersion(releaseId, versionId); applyVersion(refreshed);
      setUploads((current) => ({ ...current, [key]: { progress: 100, message: 'Carga confirmada; procesamiento en cola.' } }));
    } catch (reason) {
      setUploads((current) => ({ ...current, [key]: { progress: 0, message: '', error: reason instanceof Error ? reason.message : 'Falló la carga.' } }));
    } finally { operationPending.current = false; setActionPending(false); }
  };

  const acceptAndValidate = async () => {
    if (operationPending.current || saving.current) return;
    if (!acceptedTerms) { setError('Debes aceptar la declaración de autoridad para publicar.'); return; }
    operationPending.current = true; setActionPending(true);
    try {
      if (!await persistDraft()) return;
      await musicReleases.acceptTerms(releaseId, versionId, 'publication-authority-2026-09-12');
      const result = await musicReleases.validate(releaseId, versionId);
      const refreshed = await musicReleases.getVersion(releaseId, versionId); applyVersion(refreshed);
      if (!result.valid) setError(result.errors.map((entry) => `${entry.fieldPath}: ${entry.message}`).join('\n'));
      else setStatus('Validación editorial completa. Ya puedes enviar a revisión.');
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo validar.'); }
    finally { operationPending.current = false; setActionPending(false); }
  };

  const transition = async (targetState: string, extra: Record<string, unknown> = {}) => {
    if (operationPending.current || saving.current) return;
    operationPending.current = true; setActionPending(true);
    try {
      if (!await persistDraft()) return;
      await musicReleases.transition(releaseId, versionId, { targetState, reason: null, ...extra }, newRef('music-transition'));
      const next = await musicReleases.getVersion(releaseId, versionId);
      applyVersion(next); setStatus(`Estado actualizado: ${next.state}.`); setError(null);
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'La transición fue rechazada.'); }
    finally { operationPending.current = false; setActionPending(false); }
  };

  const refreshVersion = async () => {
    const next = await musicReleases.getVersion(releaseId, versionId);
    applyVersion(next);
    return next;
  };

  const requestChanges = async () => {
    if (!reviewBody.trim()) { setError('Describe la corrección solicitada.'); return; }
    setActionPending(true); setError(null);
    try {
      await musicReleases.addComment(releaseId, versionId, {
        parentId: null, fieldPath: reviewField.trim() || null, body: reviewBody.trim(),
        staffOnly: reviewStaffOnly, requestChanges: true,
      });
      const next = await refreshVersion();
      setReviewField(''); setReviewBody(''); setStatus(`Solicitud registrada; estado actualizado: ${next.state}.`);
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo solicitar el cambio.'); }
    finally { setActionPending(false); }
  };

  const resolveComment = async (commentId: string) => {
    setActionPending(true); setError(null);
    try {
      await musicReleases.resolveComment(releaseId, versionId, commentId);
      await refreshVersion(); setStatus('Solicitud marcada como resuelta; el historial se conserva.');
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo resolver la solicitud.'); }
    finally { setActionPending(false); }
  };

  const createCorrection = async () => {
    setActionPending(true); setError(null);
    try {
      const correction = await musicReleases.createCorrection(releaseId, versionId, newRef('music-correction'));
      await queryClient.invalidateQueries({ queryKey: ['music-releases', 'mine'] });
      navigate(`/label/releases/${releaseId}/versions/${correction.id}?artistPartyId=${artistPartyId}`);
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo crear la corrección versionada.'); }
    finally { setActionPending(false); }
  };

  const registerDdexParty = async () => {
    if (![ddexPartyName, ddexPartyDpid, ddexPartyAuthority, ddexPartyEvidence].every((value) => value.trim())) {
      setError('Completa nombre, DPID, autoridad y evidencia verificable de la parte DDEX.'); return;
    }
    setActionPending(true); setError(null);
    try {
      await musicReleases.registerDdexParty({
        name: ddexPartyName.trim(), dpid: ddexPartyDpid.trim(), role: ddexPartyRole,
        verificationAuthority: ddexPartyAuthority.trim(),
        verificationEvidence: { reference: ddexPartyEvidence.trim(), recordedFrom: 'music_release_studio' },
      });
      await ddexParties.refetch();
      setDdexPartyName(''); setDdexPartyDpid(''); setDdexPartyAuthority(''); setDdexPartyEvidence('');
      setStatus('Parte DDEX registrada con evidencia auditable.');
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo registrar la parte DDEX.'); }
    finally { setActionPending(false); }
  };

  const createDdexExport = async () => {
    if (!ddexSenderId || !ddexRecipientId) { setError('Selecciona emisor y destinatario DDEX verificados.'); return; }
    setActionPending(true); setError(null);
    try {
      await musicReleases.createDdexExport(releaseId, versionId, {
        operation: ddexOperation, senderRegistryId: ddexSenderId, recipientRegistryId: ddexRecipientId,
      }, newRef('music-ddex-export'));
      await ddexExports.refetch(); setStatus('Exportación DDEX encolada; actualiza el estado después de que el worker la procese.');
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo crear la exportación DDEX.'); }
    finally { setActionPending(false); }
  };

  const downloadDdexExport = async (exportId: string) => {
    setActionPending(true); setError(null);
    try {
      const access = await musicReleases.downloadDdexExport(exportId);
      const anchor = document.createElement('a');
      anchor.href = access.url; anchor.target = '_blank'; anchor.rel = 'noopener noreferrer';
      anchor.click();
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo autorizar la descarga DDEX.'); }
    finally { setActionPending(false); }
  };

  const actionInfringement = async (report: MusicInfringementReport, nextStatus: 'triage' | 'investigating' | 'actioned' | 'dismissed') => {
    if (!infringementNotes.trim()) { setError('Registra notas verificables para la decisión de infracción.'); return; }
    const suspensionVersionId = report.publishedVersionId ?? null;
    setActionPending(true); setError(null);
    try {
      await musicReleases.actionInfringementReport(report.id, {
        status: nextStatus, notes: infringementNotes.trim(),
        suspendVersionId: nextStatus === 'actioned' && suspendOnAction ? suspensionVersionId : null,
      });
      await infringementReports.refetch();
      if (nextStatus === 'actioned' && suspendOnAction && suspensionVersionId === versionId) await refreshVersion();
      setInfringementNotes(''); setStatus(`Reporte actualizado: ${nextStatus}.`);
    } catch (reason) { setError(reason instanceof Error ? reason.message : 'No se pudo actualizar el reporte.'); }
    finally { setActionPending(false); }
  };

  if (version.isLoading) return <LinearProgress />;
  if (version.isError || !version.data) return <Alert severity="error">No se pudo abrir esta versión o no tienes permiso.</Alert>;
  const currentVersion = version.data;

  return <Stack spacing={3}>
    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
      <Button component={RouterLink} to={`/label/releases/nuevo?artistPartyId=${artistPartyId}`} startIcon={<ArrowBackIcon />}>Versiones</Button>
      <Box flex={1}><Typography variant="h4" fontWeight={700}>{currentVersion.title}</Typography>
        <Typography color="text.secondary">Versión {currentVersion.versionNumber} · {currentVersion.state}</Typography></Box>
      <Chip role="status" aria-live="polite" label={isSaving ? 'Guardando…' : dirty ? 'Cambios sin guardar' : status || 'Guardado'} color={dirty ? 'warning' : 'success'} />
    </Stack>
    {error && <Alert severity="error" sx={{ whiteSpace: 'pre-line' }} onClose={() => setError(null)}>{error}</Alert>}
    {!canEdit && <Alert severity="info" action={
      ['approved','scheduled','published','suspended','replacement_pending','takedown_scheduled','withdrawn'].includes(currentVersion.state)
        ? <Button color="inherit" size="small" disabled={actionPending} onClick={() => void createCorrection()}>Crear corrección</Button>
        : undefined
    }>Esta versión es inmutable. Las correcciones crean un nuevo grafo editable y conservan esta versión intacta.</Alert>}

    <Box component="fieldset" disabled={isSaving || actionPending} sx={{ border: 0, p: 0, m: 0, minWidth: 0 }}>
    <Stack spacing={3}>
    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h6">1. Metadatos del release</Typography>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
        <TextField label="Título" value={metadata.title} onChange={(event) => changeMetadata('title', event.target.value)} fullWidth disabled={!editable} />
        <TextField label="Display artist" value={metadata.displayArtist} onChange={(event) => changeMetadata('displayArtist', event.target.value)} fullWidth disabled={!editable} />
      </Stack>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
        <TextField select label="Género principal" value={metadata.primaryGenreId ?? ''} onChange={(event) => changeMetadata('primaryGenreId', event.target.value || null)} fullWidth required disabled={!editable || genres.isLoading}>
          <MenuItem value="">Selecciona un género</MenuItem>
          {genres.data?.items.map((genre) => <MenuItem key={genre.id} value={genre.id}>{genre.name}</MenuItem>)}
        </TextField>
        <TextField select label="Subgénero / género secundario" value={metadata.secondaryGenreId ?? ''} onChange={(event) => changeMetadata('secondaryGenreId', event.target.value || null)} fullWidth disabled={!editable || genres.isLoading}>
          <MenuItem value="">Sin género secundario</MenuItem>
          {genres.data?.items.map((genre) => <MenuItem key={genre.id} value={genre.id}>{genre.name}</MenuItem>)}
        </TextField>
      </Stack>
      {genres.isError && <Alert severity="warning">No se pudo cargar el catálogo canónico de géneros. El envío quedará bloqueado hasta seleccionar una referencia vigente.</Alert>}
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
        <TextField select label="Contenido explícito" value={metadata.explicitContent} onChange={(event) => changeMetadata('explicitContent', event.target.value as DraftMetadata['explicitContent'])} fullWidth disabled={!editable}>
          <MenuItem value="unknown">Pendiente</MenuItem><MenuItem value="not_explicit">No explícito</MenuItem><MenuItem value="explicit">Explícito</MenuItem><MenuItem value="cleaned">Versión clean</MenuItem>
        </TextField>
        <TextField label="Fecha original" type="date" InputLabelProps={{ shrink: true }} value={metadata.originalReleaseDate ?? ''} onChange={(event) => changeMetadata('originalReleaseDate', event.target.value || null)} fullWidth disabled={!editable} />
        <TextField label="Sello" value={metadata.labelName ?? ''} onChange={(event) => changeMetadata('labelName', event.target.value || null)} fullWidth disabled={!editable} />
        <TextField label="N.º catálogo" value={metadata.catalogNumber ?? ''} onChange={(event) => changeMetadata('catalogNumber', event.target.value || null)} fullWidth disabled={!editable} />
      </Stack>
      <TextField label="Copyright de la grabación (℗)" value={metadata.recordingCopyrightText ?? ''} onChange={(event) => changeMetadata('recordingCopyrightText', event.target.value || null)} disabled={!editable} />
      <TextField label="Copyright de la obra (©)" value={metadata.workCopyrightText ?? ''} onChange={(event) => changeMetadata('workCopyrightText', event.target.value || null)} disabled={!editable} />
    </Stack></CardContent></Card>

    <Card><CardContent><Stack spacing={2}>
      <Stack direction="row" justifyContent="space-between"><Typography variant="h6">2. Pistas y másteres</Typography>
        <Button startIcon={<AddIcon />} onClick={addTrack} disabled={!editable}>Añadir pista</Button></Stack>
      {content.tracks.map((track, index) => <Stack key={track.clientRef} spacing={1.5} sx={{ p: 2, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
        <Stack direction="row" alignItems="center"><Typography fontWeight={700} flex={1}>Pista {index + 1}</Typography>
          <IconButton aria-label={`Eliminar pista ${index + 1}`} onClick={() => removeTrack(track.clientRef)} disabled={!editable || content.tracks.length === 1}><DeleteOutlineIcon /></IconButton></Stack>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
          <TextField label="Título" value={track.title} onChange={(event) => changeTrack(index, 'title', event.target.value)} fullWidth disabled={!editable} />
          <TextField label="Display artist" value={track.displayArtist} onChange={(event) => changeTrack(index, 'displayArtist', event.target.value)} fullWidth disabled={!editable} />
          <TextField select label="Explícito" value={track.explicitContent} onChange={(event) => changeTrack(index, 'explicitContent', event.target.value as MusicTrackDraftInput['explicitContent'])} disabled={!editable}>
            <MenuItem value="unknown">Pendiente</MenuItem><MenuItem value="not_explicit">No</MenuItem><MenuItem value="explicit">Sí</MenuItem><MenuItem value="cleaned">Clean</MenuItem>
          </TextField>
        </Stack>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
          <TextField label={`Inicio del preview de pista ${index + 1} (ms)`} type="number"
            inputProps={{ min: 0, step: 1 }} value={track.previewStartMs ?? ''}
            onChange={(event) => changeTrack(index, 'previewStartMs', event.target.value === '' ? null : Number(event.target.value))}
            disabled={!editable} helperText="Vacío: inicio automático, o 0 si indicas duración." />
          <TextField label={`Duración del preview de pista ${index + 1} (ms)`} type="number"
            inputProps={{ min: 1, step: 1 }} value={track.previewDurationMs ?? ''}
            onChange={(event) => changeTrack(index, 'previewDurationMs', event.target.value === '' ? null : Number(event.target.value))}
            disabled={!editable} helperText="Ambos vacíos: hasta 30 s; cambios requieren reprocesamiento." />
        </Stack>
        {musicPreviewRangeError(track.previewStartMs, track.previewDurationMs, currentVersion.tracks.find((item) => item.recordingId === track.recordingId)?.durationMs) &&
          <Alert severity="warning">{musicPreviewRangeError(track.previewStartMs, track.previewDurationMs, currentVersion.tracks.find((item) => item.recordingId === track.recordingId)?.durationMs)}</Alert>}
        <TextField label="ISRC aportado (opcional)" value={content.identifiers.find((identifier) => identifier.trackRef === track.clientRef && identifier.type === 'isrc')?.value ?? ''}
          onChange={(event) => { const value = event.target.value; setContent((current) => ({ ...current, identifiers: [...current.identifiers.filter((identifier) => !(identifier.trackRef === track.clientRef && identifier.type === 'isrc')), ...(value.trim() ? [{ trackRef: track.clientRef, type: 'isrc' as const, value }] : [])] })); setDirty(true); }} disabled={!editable} />
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ sm: 'center' }}>
          <Button component="label" variant="outlined" startIcon={<CloudUploadIcon />} disabled={!editable || !track.recordingId}>
            Cargar máster WAV/FLAC/AIFF<input hidden type="file" accept="audio/wav,audio/x-wav,audio/flac,audio/aiff,audio/x-aiff" onChange={(event) => { const file = event.target.files?.[0]; if (file) void upload(track.clientRef, file, 'master_audio', track.recordingId); }} />
          </Button>
          {!track.recordingId && <Typography variant="body2" color="text.secondary">Guarda primero para asignar el recordingId.</Typography>}
          <Typography variant="body2">Duración técnica: {currentVersion.tracks.find((item) => item.recordingId === track.recordingId)?.durationMs ?? 'pendiente'}</Typography>
        </Stack>
        <UploadProgress state={uploads[track.clientRef]} />
      </Stack>)}
      <Button component="label" variant="outlined" startIcon={<CloudUploadIcon />} disabled={!editable}>
        Cargar portada original<input hidden type="file" accept="image/jpeg,image/png,image/tiff" onChange={(event) => { const file = event.target.files?.[0]; if (file) void upload('cover', file, 'cover_original'); }} />
      </Button>
      <UploadProgress state={uploads['cover']} />
      <Stack direction="row" spacing={1} flexWrap="wrap">{currentVersion.assets.map((asset) => <Chip key={asset.id} label={`${asset.role}: ${asset.processingState}`} size="small" />)}</Stack>
    </Stack></CardContent></Card>

    <Card><CardContent><Stack spacing={2}>
      <Stack direction="row" justifyContent="space-between"><Typography variant="h6">3. Partes, créditos y derechos</Typography>
        <Button startIcon={<AddIcon />} onClick={addParty} disabled={!editable}>Añadir colaborador</Button></Stack>
      <Typography variant="body2" color="text.secondary">Una parte puede ser externa y no necesita cuenta TDF. Los puntos básicos de cada declaración deben sumar 10000.</Typography>
      {currentVersion.parties.some((party) => asRaw(party)['detailsSource'] === 'legacy_observed') &&
        <Alert severity="warning">Estos colaboradores provienen de datos legados. Revisa sus nombres e identificadores antes de guardar y enviar; no son una reconstrucción de la aprobación histórica.</Alert>}
      {content.parties.map((party, index) => <Stack key={party.clientRef} direction={{ xs: 'column', md: 'row' }} spacing={2}>
        <TextField label="Nombre visible" value={party.displayName} onChange={(event) => changeParty(index, { displayName: event.target.value })} fullWidth disabled={!editable} />
        <TextField label="Nombre legal" value={party.legalName ?? ''} onChange={(event) => changeParty(index, { legalName: event.target.value || null })} fullWidth disabled={!editable} />
        <TextField label="IPI (opcional)" value={party.identifiers.find((identifier) => identifier.type === 'ipi')?.value ?? ''}
          onChange={(event) => changeParty(index, { identifiers: [...party.identifiers.filter((identifier) => identifier.type !== 'ipi'), ...(event.target.value ? [{ type: 'ipi' as const, value: event.target.value }] : [])] })} disabled={!editable} />
      </Stack>)}
      <Divider />
      {content.rightsDeclarations.map((rights, rightsIndex) => <Stack key={`${rights.trackRef}-${rights.scope}-${rightsIndex}`} spacing={1} sx={{ p: 2, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
        <Typography fontWeight={700}>{rights.scope === 'master' ? 'Derechos de máster' : 'Derechos de composición'} · {content.tracks.find((track) => track.clientRef === rights.trackRef)?.title ?? 'release'}</Typography>
        <TextField label="Base de autoridad" value={rights.authorityBasis} onChange={(event) => { setContent((current) => ({ ...current, rightsDeclarations: current.rightsDeclarations.map((item, at) => at === rightsIndex ? { ...item, authorityBasis: event.target.value } : item) })); setDirty(true); }} disabled={!editable} helperText="Ej.: titularidad propia o licencia identificada; no adjuntes datos falsos." />
        {rights.splits.map((split, splitIndex) => <Stack key={`${split.partyRef}-${splitIndex}`} direction={{ xs: 'column', sm: 'row' }} spacing={2}>
          <TextField select label="Titular" value={split.partyRef} onChange={(event) => { setContent((current) => ({ ...current, rightsDeclarations: current.rightsDeclarations.map((item, at) => at === rightsIndex ? { ...item, splits: item.splits.map((part, splitAt) => splitAt === splitIndex ? { ...part, partyRef: event.target.value } : part) } : item) })); setDirty(true); }} fullWidth disabled={!editable}>
            {content.parties.map((candidate) => <MenuItem key={candidate.clientRef} value={candidate.clientRef}>{candidate.displayName.trim() ? candidate.displayName : 'Sin nombre'}</MenuItem>)}
          </TextField>
          <TextField label="Puntos básicos" type="number" value={split.basisPoints} onChange={(event) => { const basisPoints = Number(event.target.value); setContent((current) => ({ ...current, rightsDeclarations: current.rightsDeclarations.map((item, at) => at === rightsIndex ? { ...item, splits: item.splits.map((part, splitAt) => splitAt === splitIndex ? { ...part, basisPoints } : part) } : item) })); setDirty(true); }} disabled={!editable} />
          <IconButton aria-label="Eliminar split" disabled={!editable || rights.splits.length === 1} onClick={() => { setContent((current) => ({ ...current, rightsDeclarations: current.rightsDeclarations.map((item, at) => at === rightsIndex ? { ...item, splits: item.splits.filter((_, splitAt) => splitAt !== splitIndex) } : item) })); setDirty(true); }}><DeleteOutlineIcon /></IconButton>
        </Stack>)}
        <Button size="small" onClick={() => { const partyRef = content.parties[0]?.clientRef; if (!partyRef) return; setContent((current) => ({ ...current, rightsDeclarations: current.rightsDeclarations.map((item, at) => at === rightsIndex ? { ...item, splits: [...item.splits, { partyRef, basisPoints: 0, territories: item.territories, startsOn: item.startsOn, endsOn: item.endsOn }] } : item) })); setDirty(true); }} disabled={!editable}>Añadir split</Button>
        <Typography variant="caption" color={rights.splits.reduce((sum, split) => sum + split.basisPoints, 0) === 10000 ? 'success.main' : 'error'}>Total: {rights.splits.reduce((sum, split) => sum + split.basisPoints, 0)} / 10000</Typography>
      </Stack>)}
    </Stack></CardContent></Card>

    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h6">4. Acceso, declaración y revisión</Typography>
      {content.availability.map((rule, index) => <Stack key={index} spacing={2} sx={{ p: 2, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
          <TextField select label="Escucha" value={rule.listeningPolicy} onChange={(event) => changeAvailability(index, { listeningPolicy: event.target.value as AvailabilityDraft['listeningPolicy'] })} disabled={!editable}>
            <MenuItem value="none">Sin escucha</MenuItem><MenuItem value="preview">Preview</MenuItem><MenuItem value="full">Completa</MenuItem>
          </TextField>
          <TextField select label="Modo territorial" value={rule.territoryMode} onChange={(event) => changeAvailability(index, { territoryMode: event.target.value as AvailabilityDraft['territoryMode'] })} disabled={!editable}>
            <MenuItem value="include">Solo incluir</MenuItem><MenuItem value="exclude">Excluir</MenuItem>
          </TextField>
          <TextField label="Territorios" value={rule.territories.join(', ')} onChange={(event) => changeAvailability(index, { territories: event.target.value.split(',').map((item) => item.trim()).filter(Boolean) })} helperText="Worldwide o códigos EC, CO, MX…" fullWidth disabled={!editable} />
        </Stack>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
          <TextField type="datetime-local" label="Disponible desde" InputLabelProps={{ shrink: true }} value={rule.startsAt?.slice(0, 16) ?? ''} onChange={(event) => changeAvailability(index, { startsAt: event.target.value ? new Date(event.target.value).toISOString() : null })} fullWidth disabled={!editable} />
          <TextField type="datetime-local" label="Disponible hasta" InputLabelProps={{ shrink: true }} value={rule.endsAt?.slice(0, 16) ?? ''} onChange={(event) => changeAvailability(index, { endsAt: event.target.value ? new Date(event.target.value).toISOString() : null })} fullWidth disabled={!editable} />
        </Stack>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
          <TextField select label="Descarga" value={rule.downloadPolicy} onChange={(event) => {
            const policy = event.target.value as AvailabilityDraft['downloadPolicy'];
            changeAvailability(index, policy === 'purchase'
              ? { downloadPolicy: policy, purchasable: true, priceMinor: rule.priceMinor ?? 100, currency: rule.currency ?? 'USD' }
              : policy === 'free'
                ? { downloadPolicy: policy, purchasable: false, priceMinor: null, currency: null }
                : { downloadPolicy: policy, purchasable: false, priceMinor: null, currency: null, downloadableAssetId: null });
          }} disabled={!editable}>
            <MenuItem value="none">No descargable</MenuItem><MenuItem value="free">Descarga gratuita</MenuItem><MenuItem value="purchase">Incluida tras compra</MenuItem>
          </TextField>
          {rule.downloadPolicy !== 'none' && <TextField select label="Activo descargable" value={rule.downloadableAssetId ?? ''} onChange={(event) => changeAvailability(index, { downloadableAssetId: event.target.value === '' ? null : event.target.value })} fullWidth disabled={!editable} helperText="Selecciona exactamente el archivo que se entregará.">
            {currentVersion.assets.filter((asset) => ['master_audio','stream_audio'].includes(asset.role) && ['valid','ready'].includes(asset.processingState)).map((asset) => <MenuItem key={asset.id} value={asset.id}>{asset.originalFilename ?? asset.role} · {asset.sha256.slice(0, 12)}</MenuItem>)}
          </TextField>}
          {rule.downloadPolicy === 'purchase' && <>
            <TextField label="Precio (unidad menor)" type="number" value={rule.priceMinor ?? ''} onChange={(event) => changeAvailability(index, { priceMinor: event.target.value === '' ? null : Number(event.target.value) })} helperText="Ej.: USD 4,99 = 499. Sin flotantes." disabled={!editable} />
            <TextField label="Moneda ISO" value={rule.currency ?? ''} onChange={(event) => { const currency = event.target.value.toUpperCase().slice(0, 3); changeAvailability(index, { currency: currency === '' ? null : currency }); }} disabled={!editable} />
          </>}
        </Stack>
      </Stack>)}
      <FormControlLabel control={<Checkbox checked={acceptedTerms} onChange={(event) => setAcceptedTerms(event.target.checked)} disabled={!editable} />} label="Declaro que tengo autoridad para publicar y que los titulares, créditos, splits, territorios y restricciones son correctos." />
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} flexWrap="wrap">
        <Button variant="outlined" startIcon={<SaveIcon />} onClick={() => void persistDraft()} disabled={!editable || !dirty}>Guardar ahora</Button>
        <Button variant="outlined" onClick={() => void acceptAndValidate()} disabled={!editable}>Aceptar y validar</Button>
        <Button variant="contained" startIcon={<SendIcon />} onClick={() => void transition('ready_for_review')} disabled={!editable}>Enviar a revisión</Button>
      </Stack>
      <Stack spacing={0.5}>{currentVersion.validation.errors.map((entry) => <Alert key={`${entry.fieldPath}-${entry.code}`} severity="warning"><strong>{entry.fieldPath}</strong>: {entry.message}</Alert>)}</Stack>
      {currentVersion.comments.map((comment) => <Alert key={comment.id}
        severity={comment.resolutionState === 'open' ? 'info' : 'success'}
        action={comment.resolutionState === 'open' ? <Button color="inherit" size="small" disabled={actionPending} onClick={() => void resolveComment(comment.id)}>Resolver</Button> : undefined}>
        {comment.fieldPath ? `${comment.fieldPath}: ` : ''}{comment.body}
      </Alert>)}
      {isAdmin && <><Divider /><Typography variant="subtitle1" fontWeight={700}>Revisión TDF</Typography>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
          <TextField label="Campo afectado" placeholder="tracks[1].isrc" value={reviewField} onChange={(event) => setReviewField(event.target.value)} fullWidth />
          <TextField label="Corrección solicitada" value={reviewBody} onChange={(event) => setReviewBody(event.target.value)} fullWidth required multiline minRows={2} />
          <FormControlLabel control={<Checkbox checked={reviewStaffOnly} onChange={(event) => setReviewStaffOnly(event.target.checked)} />} label="Solo personal" />
          <Button color="warning" disabled={actionPending || currentVersion.state !== 'in_review' || !reviewBody.trim()} onClick={() => void requestChanges()}>Solicitar cambios</Button>
        </Stack>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <Button disabled={currentVersion.state !== 'ready_for_review'} onClick={() => void transition('in_review')}>Iniciar revisión</Button>
          <Button disabled={currentVersion.state !== 'in_review'} onClick={() => void transition('approved')}>Aprobar versión inmutable</Button>
          <TextField type="datetime-local" label="Publicar" InputLabelProps={{ shrink: true }} value={scheduleLocal} onChange={(event) => setScheduleLocal(event.target.value)} />
          <Button variant="contained" disabled={!scheduleLocal || currentVersion.state !== 'approved'} onClick={() => void transition('scheduled', { releaseAtUtc: new Date(scheduleLocal).toISOString(), releaseTimezone: Intl.DateTimeFormat().resolvedOptions().timeZone, embargoUntilUtc: new Date(scheduleLocal).toISOString() })}>Programar</Button>
        </Stack>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <Button color="error" disabled={!['in_review','approved','scheduled','published'].includes(currentVersion.state)} onClick={() => void transition('suspended', { reason: 'Suspensión editorial desde Studio' })}>Suspender</Button>
          <TextField type="datetime-local" label="Retirar" InputLabelProps={{ shrink: true }} value={takedownLocal} onChange={(event) => setTakedownLocal(event.target.value)} />
          <Button color="error" disabled={!takedownLocal || !['published','suspended','replacement_pending'].includes(currentVersion.state)} onClick={() => void transition('takedown_scheduled', { reason: 'Retiro programado desde Studio', takedownAtUtc: new Date(takedownLocal).toISOString(), takedownTimezone: Intl.DateTimeFormat().resolvedOptions().timeZone })}>Programar retiro</Button>
          <Button color="error" disabled={!editable} onClick={() => void transition('cancelled', { reason: 'Cancelación editorial desde Studio' })}>Cancelar borrador</Button>
        </Stack></>}
    </Stack></CardContent></Card>

    <Card><CardContent><Stack spacing={2}>
      <Typography variant="h6">5. Analítica operativa</Typography>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
        <TextField type="date" label="Desde" InputLabelProps={{ shrink: true }} value={analyticsFrom} onChange={(event) => setAnalyticsFrom(event.target.value)} />
        <TextField type="date" label="Hasta" InputLabelProps={{ shrink: true }} value={analyticsTo} onChange={(event) => setAnalyticsTo(event.target.value)} />
      </Stack>
      {analytics.isLoading && <LinearProgress />}
      {analytics.isError && <Alert severity="info">Las métricas requieren el permiso de analítica del artista y la feature de autoría habilitada.</Alert>}
      {analytics.data && <>
        <Alert severity="warning">{analytics.data.disclaimer}</Alert>
        <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
          <Chip label={`Inicios ${analytics.data.totals.playStarts}`} />
          <Chip label={`Escuchas válidas ${analytics.data.totals.eligiblePlays}`} color="primary" />
          <Chip label={`Finalizaciones ${analytics.data.totals.completions}`} />
          <Chip label={`Skips ${analytics.data.totals.skips}`} />
          <Chip label={`Oyentes ${analytics.data.totals.uniqueListeners}`} />
          <Chip label={`Tiempo ${Math.round(analytics.data.totals.listenedMs / 60000)} min`} />
          <Chip label={`Compras ${analytics.data.totals.purchases}`} />
          <Chip label={`Descargas ${analytics.data.totals.downloads}`} />
        </Stack>
        {analytics.data.tracks.map((track) => <Stack key={track.recordingId} direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <Typography flex={1} fontWeight={600}>{track.title}</Typography>
          <Typography variant="body2">{track.eligiblePlays} válidas · {track.completions} completas · {track.skips} skips</Typography>
        </Stack>)}
        {analytics.data.territories.length > 0 && <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
          {analytics.data.territories.map((territory) => <Chip key={territory.territoryCode} variant="outlined" label={`${territory.territoryCode}: ${territory.eligiblePlays} válidas`} />)}
        </Stack>}
      </>}
    </Stack></CardContent></Card>

    {isAdmin && <Card><CardContent><Stack spacing={2}>
      <Typography variant="h6">6. Exportación DDEX</Typography>
      <Alert severity="info">Matriz fijada: ERN 4.3.2 · Audio Release Profile 2.3.1 · AVS 011 · DD-ERN-432 · Cloud Storage 1.8.1. ERN 4 no usa Business Profile separado.</Alert>
      <Typography variant="subtitle1" fontWeight={700}>Registro verificado de partes</Typography>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
        <TextField label="Nombre" value={ddexPartyName} onChange={(event) => setDdexPartyName(event.target.value)} fullWidth />
        <TextField label="DPID real" value={ddexPartyDpid} onChange={(event) => setDdexPartyDpid(event.target.value)} fullWidth />
        <TextField select label="Rol" value={ddexPartyRole} onChange={(event) => setDdexPartyRole(event.target.value as typeof ddexPartyRole)}>
          <MenuItem value="sender">Emisor</MenuItem><MenuItem value="recipient">Destinatario</MenuItem><MenuItem value="both">Ambos</MenuItem>
        </TextField>
      </Stack>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
        <TextField label="Autoridad de verificación" value={ddexPartyAuthority} onChange={(event) => setDdexPartyAuthority(event.target.value)} fullWidth />
        <TextField label="Referencia de evidencia" value={ddexPartyEvidence} onChange={(event) => setDdexPartyEvidence(event.target.value)} helperText="Registro, documento interno o ticket verificable; nunca un placeholder." fullWidth />
        <Button variant="outlined" disabled={actionPending} onClick={() => void registerDdexParty()}>Registrar</Button>
      </Stack>
      {ddexParties.isError && <Alert severity="warning">No se pudo leer el registro DDEX.</Alert>}
      <Divider />
      <Typography variant="subtitle1" fontWeight={700}>Generar paquete inmutable</Typography>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
        <TextField select label="Operación" value={ddexOperation} onChange={(event) => setDdexOperation(event.target.value as typeof ddexOperation)}>
          <MenuItem value="new_release">Nuevo release</MenuItem><MenuItem value="update">Actualización completa</MenuItem><MenuItem value="takedown">Retiro</MenuItem>
        </TextField>
        <TextField select label="Emisor" value={ddexSenderId} onChange={(event) => setDdexSenderId(event.target.value)} fullWidth>
          {(ddexParties.data ?? []).filter((party) => party.active && ['sender','both'].includes(party.role)).map((party) => <MenuItem key={party.id} value={party.id}>{party.name} · {party.dpid}</MenuItem>)}
        </TextField>
        <TextField select label="Destinatario" value={ddexRecipientId} onChange={(event) => setDdexRecipientId(event.target.value)} fullWidth>
          {(ddexParties.data ?? []).filter((party) => party.active && ['recipient','both'].includes(party.role)).map((party) => <MenuItem key={party.id} value={party.id}>{party.name} · {party.dpid}</MenuItem>)}
        </TextField>
        <Button variant="contained" disabled={actionPending || !ddexSenderId || !ddexRecipientId} onClick={() => void createDdexExport()}>Generar</Button>
        <Button disabled={ddexExports.isFetching} onClick={() => void ddexExports.refetch()}>Actualizar</Button>
      </Stack>
      {ddexExports.isError && <Alert severity="warning">La feature DDEX está deshabilitada o no se pudo cargar el historial.</Alert>}
      {(ddexExports.data ?? []).map((entry) => <Stack key={entry.id} direction={{ xs: 'column', md: 'row' }} spacing={1} alignItems={{ md: 'center' }} sx={{ p: 1.5, border: '1px solid', borderColor: 'divider', borderRadius: 1 }}>
        <Box flex={1}><Typography fontWeight={700}>{entry.operation} · {entry.status}</Typography>
          <Typography variant="caption" color="text.secondary">{entry.messageId} · {entry.senderDpid} → {entry.recipientDpid}{entry.packageSha256 ? ` · SHA-256 ${entry.packageSha256}` : ''}</Typography></Box>
        <Button disabled={entry.status !== 'valid' || actionPending} onClick={() => void downloadDdexExport(entry.id)}>Descargar paquete exacto</Button>
      </Stack>)}
    </Stack></CardContent></Card>}

    {isAdmin && <Card><CardContent><Stack spacing={2}>
      <Typography variant="h6">7. Infracciones y suspensión</Typography>
      <TextField label="Notas de triage o resolución" value={infringementNotes} onChange={(event) => setInfringementNotes(event.target.value)} multiline minRows={2} inputProps={{ maxLength: 10000 }} />
      <FormControlLabel control={<Checkbox checked={suspendOnAction} onChange={(event) => setSuspendOnAction(event.target.checked)} />} label="Suspender esta versión al marcar el reporte como accionado" />
      {infringementReports.isError && <Alert severity="warning">No se pudo cargar la cola de infracciones.</Alert>}
      {(infringementReports.data ?? []).map((report) => <Stack key={report.id} spacing={1} sx={{ p: 1.5, border: '1px solid', borderColor: 'divider', borderRadius: 1 }}>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}><Typography fontWeight={700} flex={1}>{report.reasonCode} · {report.status}</Typography><Typography variant="caption">{new Date(report.createdAt).toLocaleString('es-EC')}</Typography></Stack>
        <Typography>{report.description}</Typography>
        {report.resolutionNotes && <Typography variant="body2" color="text.secondary">Resolución: {report.resolutionNotes}</Typography>}
        {!['actioned','dismissed'].includes(report.status) && <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
          {report.status === 'received' && <Button disabled={actionPending} onClick={() => void actionInfringement(report, 'triage')}>Iniciar triage</Button>}
          {report.status === 'triage' && <Button disabled={actionPending} onClick={() => void actionInfringement(report, 'investigating')}>Investigar</Button>}
          {['triage','investigating'].includes(report.status) && <Button color="error" disabled={actionPending} onClick={() => void actionInfringement(report, 'actioned')}>Accionar</Button>}
          <Button disabled={actionPending} onClick={() => void actionInfringement(report, 'dismissed')}>Desestimar</Button>
        </Stack>}
      </Stack>)}
      {infringementReports.data?.length === 0 && <Typography color="text.secondary">No hay reportes para este release.</Typography>}
    </Stack></CardContent></Card>}
    </Stack>
    </Box>
  </Stack>;
}
