import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  InputLabel,
  LinearProgress,
  MenuItem,
  Select,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
} from '@mui/material';
import FactCheckOutlinedIcon from '@mui/icons-material/FactCheckOutlined';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';

import PageShell, { EmptyState } from '../components/PageShell';
import {
  ArtistEnrichment,
  type ArtistEnrichmentDecision,
  type ArtistEnrichmentSuggestion,
} from '../api/artistEnrichment';

type ReviewTab = 'profiles' | 'suggestions' | 'identities' | 'audit';
type VerificationFilter = 'all' | 'never' | 'stale';

interface EditDialogState {
  suggestion: ArtistEnrichmentSuggestion;
  editedValue: string;
  note: string;
}

const formatConfidence = (value?: number | null) =>
  value === null || value === undefined ? '—' : `${Math.round(value * 100)}%`;

const formatDate = (value?: string | null) =>
  value ? new Intl.DateTimeFormat('es-EC', { dateStyle: 'medium', timeStyle: 'short' }).format(new Date(value)) : '—';

const statusColor = (status: string): 'default' | 'warning' | 'success' | 'error' | 'info' => {
  if (status === 'pending' || status === 'unverified') return 'warning';
  if (status === 'approved' || status === 'auto_applied' || status === 'completed') return 'success';
  if (status === 'rejected' || status === 'failed') return 'error';
  return 'info';
};

const safeEvidence = (raw: string) => {
  try {
    return JSON.stringify(JSON.parse(raw), null, 2);
  } catch {
    return raw;
  }
};

function MetricCard({ label, value, tone = 'text.primary' }: { label: string; value: number; tone?: string }) {
  return (
    <Card variant="outlined">
      <CardContent>
        <Typography variant="overline" color="text.secondary">{label}</Typography>
        <Typography variant="h4" sx={{ color: tone }}>{value}</Typography>
      </CardContent>
    </Card>
  );
}

export default function ArtistEnrichmentReviewPage() {
  const queryClient = useQueryClient();
  const [tab, setTab] = useState<ReviewTab>('profiles');
  const [status, setStatus] = useState('');
  const [artistId, setArtistId] = useState<number | null>(null);
  const [minimumConfidence, setMinimumConfidence] = useState(0);
  const [missingField, setMissingField] = useState('');
  const [brokenOnly, setBrokenOnly] = useState(false);
  const [verificationFilter, setVerificationFilter] = useState<VerificationFilter>('all');
  const [identityTargets, setIdentityTargets] = useState<Record<number, string>>({});
  const [editDialog, setEditDialog] = useState<EditDialogState | null>(null);
  const [notice, setNotice] = useState<string | null>(null);

  const queryKey = ['artist-enrichment', status, artistId] as const;
  const overviewQuery = useQuery({
    queryKey,
    queryFn: () => ArtistEnrichment.overview(status || undefined, artistId),
  });
  const overview = overviewQuery.data;

  const refresh = async () => {
    setNotice(null);
    await queryClient.invalidateQueries({ queryKey: ['artist-enrichment'] });
  };

  const runMutation = useMutation({
    mutationFn: (targetArtistId: number | null) => targetArtistId === null
      ? ArtistEnrichment.run({ aerrMode: 'dry_run', aerrBatchSize: 50 })
      : ArtistEnrichment.rerunArtist(targetArtistId, { aerrMode: 'dry_run', aerrArtistId: targetArtistId, aerrBatchSize: 50 }),
    onSuccess: async (run) => {
      setNotice(`Auditoría ${run.aerRunKey} completada sin publicar cambios.`);
      await refresh();
    },
  });

  const decisionMutation = useMutation({
    mutationFn: ({ suggestionId, decision }: { suggestionId: number; decision: ArtistEnrichmentDecision }) =>
      ArtistEnrichment.decideSuggestion(suggestionId, decision),
    onSuccess: async () => {
      setEditDialog(null);
      setNotice('Decisión guardada en el historial de auditoría.');
      await refresh();
    },
  });

  const setDecisionMutation = useMutation({
    mutationFn: ({ targetArtistId, decision }: { targetArtistId: number; decision: ArtistEnrichmentDecision }) =>
      ArtistEnrichment.decideSuggestionSet(targetArtistId, decision),
    onSuccess: refresh,
  });

  const identityMutation = useMutation({
    mutationFn: ({ candidateId, decision }: { candidateId: number; decision: ArtistEnrichmentDecision }) =>
      ArtistEnrichment.decideIdentity(candidateId, decision),
    onSuccess: refresh,
  });

  const profiles = useMemo(() => overview?.aeoProfiles ?? [], [overview?.aeoProfiles]);
  const suggestions = useMemo(() => overview?.aeoSuggestions ?? [], [overview?.aeoSuggestions]);
  const identities = useMemo(() => overview?.aeoIdentityCandidates ?? [], [overview?.aeoIdentityCandidates]);
  const pendingSuggestions = suggestions.filter((item) => item.aesStatus === 'pending' && item.aesConfidence >= minimumConfidence);
  const pendingIdentities = identities.filter((item) => item.aicStatus === 'pending' && item.aicConfidence >= minimumConfidence);
  const incompleteProfiles = profiles.filter((item) => item.apeMissingFields.length > 0);
  const allMissingFields = useMemo(() => [...new Set(profiles.flatMap((item) => item.apeMissingFields))].sort(), [profiles]);
  const filteredProfiles = useMemo(() => profiles.filter((profile) => {
    if ((profile.apeConfidence ?? 0) < minimumConfidence) return false;
    if (missingField && !profile.apeMissingFields.includes(missingField)) return false;
    if (brokenOnly && profile.apeBrokenFields.length === 0) return false;
    if (verificationFilter === 'never' && profile.apeLastVerifiedAt != null) return false;
    if (verificationFilter === 'stale') {
      if (!profile.apeLastVerifiedAt) return false;
      const cutoff = Date.now() - 90 * 24 * 60 * 60 * 1000;
      if (new Date(profile.apeLastVerifiedAt).getTime() >= cutoff) return false;
    }
    return true;
  }), [brokenOnly, minimumConfidence, missingField, profiles, verificationFilter]);
  const groupedPending = useMemo(() => {
    const groups = new Map<number, ArtistEnrichmentSuggestion[]>();
    pendingSuggestions.forEach((suggestion) => {
      if (suggestion.aesArtistId === null || suggestion.aesArtistId === undefined) return;
      groups.set(suggestion.aesArtistId, [...(groups.get(suggestion.aesArtistId) ?? []), suggestion]);
    });
    return groups;
  }, [pendingSuggestions]);
  const mutationError = runMutation.error
    ?? decisionMutation.error
    ?? setDecisionMutation.error
    ?? identityMutation.error;

  return (
    <PageShell
      title="Enriquecimiento de artistas"
      subtitle="Inventario completo, investigación, matching y publicación auditable."
      actions={(
        <>
          <Button
            variant="outlined"
            startIcon={<RefreshIcon />}
            onClick={() => void refresh()}
            disabled={overviewQuery.isFetching}
          >
            Actualizar
          </Button>
          <Button
            variant="contained"
            startIcon={<PlayArrowIcon />}
            onClick={() => runMutation.mutate(artistId)}
            disabled={runMutation.isPending}
          >
            {artistId === null ? 'Auditar plataforma' : 'Auditar artista'}
          </Button>
        </>
      )}
    >
      <Stack spacing={3}>
        {(overviewQuery.isLoading || overviewQuery.isFetching || runMutation.isPending) && <LinearProgress />}
        {overviewQuery.error && <Alert severity="error">No se pudo cargar la cola de enriquecimiento.</Alert>}
        {mutationError && <Alert severity="error">{mutationError instanceof Error ? mutationError.message : 'La operación falló.'}</Alert>}
        {notice && <Alert severity="success" onClose={() => setNotice(null)}>{notice}</Alert>}

        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} flexWrap="wrap" useFlexGap>
          <FormControl size="small" sx={{ minWidth: 220 }}>
            <InputLabel id="artist-enrichment-status-label">Estado</InputLabel>
            <Select
              labelId="artist-enrichment-status-label"
              label="Estado"
              value={status}
              onChange={(event) => setStatus(event.target.value)}
            >
              <MenuItem value="">Todos</MenuItem>
              <MenuItem value="pending">Pendiente</MenuItem>
              <MenuItem value="approved">Aprobado</MenuItem>
              <MenuItem value="rejected">Rechazado</MenuItem>
              <MenuItem value="auto_applied">Autoaplicado</MenuItem>
            </Select>
          </FormControl>
          <FormControl size="small" sx={{ minWidth: 260 }}>
            <InputLabel id="artist-enrichment-artist-label">Artista</InputLabel>
            <Select
              labelId="artist-enrichment-artist-label"
              label="Artista"
              value={artistId ?? ''}
              onChange={(event) => setArtistId(event.target.value === '' ? null : Number(event.target.value))}
            >
              <MenuItem value="">Todos los artistas</MenuItem>
              {profiles.map((profile) => (
                <MenuItem key={profile.apeArtistId} value={profile.apeArtistId}>
                  {profile.apeArtistName}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
          <FormControl size="small" sx={{ minWidth: 180 }}>
            <InputLabel id="artist-enrichment-confidence-label">Confianza mínima</InputLabel>
            <Select
              labelId="artist-enrichment-confidence-label"
              label="Confianza mínima"
              value={minimumConfidence}
              onChange={(event) => setMinimumConfidence(Number(event.target.value))}
            >
              <MenuItem value={0}>Cualquiera</MenuItem>
              <MenuItem value={0.5}>50%</MenuItem>
              <MenuItem value={0.8}>80%</MenuItem>
              <MenuItem value={0.9}>90%</MenuItem>
            </Select>
          </FormControl>
          <FormControl size="small" sx={{ minWidth: 200 }}>
            <InputLabel id="artist-enrichment-missing-label">Campo faltante</InputLabel>
            <Select
              labelId="artist-enrichment-missing-label"
              label="Campo faltante"
              value={missingField}
              onChange={(event) => setMissingField(event.target.value)}
            >
              <MenuItem value="">Cualquiera</MenuItem>
              {allMissingFields.map((field) => <MenuItem key={field} value={field}>{field}</MenuItem>)}
            </Select>
          </FormControl>
          <FormControl size="small" sx={{ minWidth: 190 }}>
            <InputLabel id="artist-enrichment-verification-label">Verificación</InputLabel>
            <Select
              labelId="artist-enrichment-verification-label"
              label="Verificación"
              value={verificationFilter}
              onChange={(event) => setVerificationFilter(event.target.value as VerificationFilter)}
            >
              <MenuItem value="all">Cualquiera</MenuItem>
              <MenuItem value="never">Nunca verificado</MenuItem>
              <MenuItem value="stale">Más de 90 días</MenuItem>
            </Select>
          </FormControl>
          <Button
            size="small"
            variant={brokenOnly ? 'contained' : 'outlined'}
            color={brokenOnly ? 'warning' : 'inherit'}
            onClick={() => setBrokenOnly((current) => !current)}
          >
            {brokenOnly ? 'Mostrando links rotos' : 'Filtrar links rotos'}
          </Button>
        </Stack>

        <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr 1fr', md: 'repeat(4, 1fr)' }, gap: 2 }}>
          <MetricCard label="Perfiles" value={profiles.length} />
          <MetricCard label="Incompletos" value={incompleteProfiles.length} tone="warning.main" />
          <MetricCard label="Campos pendientes" value={pendingSuggestions.length} tone="warning.main" />
          <MetricCard label="Identidades ambiguas" value={pendingIdentities.length} tone="error.main" />
        </Box>

        <Tabs
          value={tab}
          onChange={(_event, value: ReviewTab) => setTab(value)}
          variant="scrollable"
          allowScrollButtonsMobile
        >
          <Tab value="profiles" label="Cobertura" />
          <Tab value="suggestions" label={`Sugerencias (${pendingSuggestions.length})`} />
          <Tab value="identities" label={`Identidades (${pendingIdentities.length})`} />
          <Tab value="audit" label="Fuentes e historial" />
        </Tabs>

        {tab === 'profiles' && (
          <Stack spacing={2}>
            {filteredProfiles.length === 0 ? (
              <EmptyState icon={<FactCheckOutlinedIcon />} title="No hay perfiles para mostrar" />
            ) : filteredProfiles.map((profile) => (
              <Card key={profile.apeArtistId} variant="outlined">
                <CardContent>
                  <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" spacing={2}>
                    <Stack spacing={1} sx={{ minWidth: 0 }}>
                      <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                        <Typography variant="h6">{profile.apeArtistName}</Typography>
                        <Chip size="small" label={profile.apeReviewStatus} color={statusColor(profile.apeReviewStatus)} />
                        <Chip size="small" variant="outlined" label={`Confianza ${formatConfidence(profile.apeConfidence)}`} />
                      </Stack>
                      <Typography variant="body2" color="text.secondary">
                        Verificado: {formatDate(profile.apeLastVerifiedAt)}
                      </Typography>
                      <Stack direction="row" spacing={0.75} flexWrap="wrap" useFlexGap>
                        {profile.apeMissingFields.length === 0
                          ? <Chip size="small" color="success" label="Perfil completo" />
                          : profile.apeMissingFields.map((field) => (
                            <Chip key={field} size="small" color="warning" variant="outlined" label={field} />
                          ))}
                      </Stack>
                      {profile.apeBrokenFields.length > 0 && (
                        <Stack direction="row" spacing={0.75} flexWrap="wrap" useFlexGap>
                          {profile.apeBrokenFields.map((field) => (
                            <Chip key={field} size="small" color="error" label={`Link roto: ${field}`} />
                          ))}
                        </Stack>
                      )}
                    </Stack>
                    <Button
                      size="small"
                      startIcon={<PlayArrowIcon />}
                      onClick={() => runMutation.mutate(profile.apeArtistId)}
                    >
                      Reauditar
                    </Button>
                  </Stack>
                </CardContent>
              </Card>
            ))}
          </Stack>
        )}

        {tab === 'suggestions' && (
          <Stack spacing={2}>
            {suggestions.filter((item) => item.aesConfidence >= minimumConfidence).length === 0 ? (
              <EmptyState title="No hay sugerencias con estos filtros" />
            ) : suggestions.filter((item) => item.aesConfidence >= minimumConfidence).map((suggestion) => (
              <Card key={suggestion.aesId} variant="outlined">
                <CardContent>
                  <Stack spacing={2}>
                    <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
                      <Box>
                        <Typography variant="h6">{suggestion.aesArtistName ?? 'Referencia sin perfil'}</Typography>
                        <Typography variant="body2" color="text.secondary">Campo: {suggestion.aesFieldName}</Typography>
                      </Box>
                      <Stack direction="row" spacing={1} alignItems="center">
                        <Chip size="small" label={formatConfidence(suggestion.aesConfidence)} variant="outlined" />
                        <Chip size="small" label={suggestion.aesStatus} color={statusColor(suggestion.aesStatus)} />
                      </Stack>
                    </Stack>
                    <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', md: '1fr 1fr' }, gap: 2 }}>
                      <Box sx={{ bgcolor: 'action.hover', borderRadius: 1, p: 1.5 }}>
                        <Typography variant="caption" color="text.secondary">Actual</Typography>
                        <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>
                          {suggestion.aesCurrentValue ?? 'Sin valor'}
                        </Typography>
                      </Box>
                      <Box sx={{ bgcolor: 'success.50', borderRadius: 1, p: 1.5 }}>
                        <Typography variant="caption" color="text.secondary">Propuesto</Typography>
                        <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>
                          {suggestion.aesProposedValue ?? 'Eliminar valor'}
                        </Typography>
                      </Box>
                    </Box>
                    <Box component="details">
                      <Typography component="summary" variant="body2" sx={{ cursor: 'pointer' }}>Ver evidencia</Typography>
                      <Box component="pre" sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere', fontSize: 12 }}>
                        {safeEvidence(suggestion.aesEvidence)}
                      </Box>
                    </Box>
                    {suggestion.aesDecidedAt && (
                      <Typography variant="caption" color="text.secondary">
                        Decidido por {suggestion.aesDecidedBy ?? 'sistema'} · {formatDate(suggestion.aesDecidedAt)}
                        {suggestion.aesDecisionNote ? ` · ${suggestion.aesDecisionNote}` : ''}
                      </Typography>
                    )}
                    {suggestion.aesStatus === 'pending' && (
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                        <Button
                          variant="contained"
                          color="success"
                          onClick={() => decisionMutation.mutate({ suggestionId: suggestion.aesId, decision: { aedDecision: 'approve' } })}
                        >
                          Aprobar campo
                        </Button>
                        <Button
                          variant="outlined"
                          onClick={() => setEditDialog({
                            suggestion,
                            editedValue: suggestion.aesProposedValue ?? '',
                            note: '',
                          })}
                        >
                          Editar y aprobar
                        </Button>
                        <Button
                          color="error"
                          onClick={() => decisionMutation.mutate({ suggestionId: suggestion.aesId, decision: { aedDecision: 'reject' } })}
                        >
                          Rechazar
                        </Button>
                        {suggestion.aesArtistId != null && (groupedPending.get(suggestion.aesArtistId)?.length ?? 0) > 1 && (
                          <>
                            <Button
                              color="success"
                              variant="text"
                              onClick={() => setDecisionMutation.mutate({
                                targetArtistId: suggestion.aesArtistId!,
                                decision: { aedDecision: 'approve' },
                              })}
                            >
                              Aprobar conjunto
                            </Button>
                            <Button
                              color="error"
                              variant="text"
                              onClick={() => setDecisionMutation.mutate({
                                targetArtistId: suggestion.aesArtistId!,
                                decision: { aedDecision: 'reject' },
                              })}
                            >
                              Rechazar conjunto
                            </Button>
                          </>
                        )}
                      </Stack>
                    )}
                  </Stack>
                </CardContent>
              </Card>
            ))}
          </Stack>
        )}

        {tab === 'identities' && (
          <Stack spacing={2}>
            {identities.filter((item) => item.aicConfidence >= minimumConfidence).length === 0 ? (
              <EmptyState title="No hay coincidencias ambiguas con estos filtros" />
            ) : identities.filter((item) => item.aicConfidence >= minimumConfidence).map((candidate) => {
              const reference = overview?.aeoInventory.find((item) => item.airId === candidate.aicInventoryReferenceId);
              const profile = profiles.find((item) => item.apeArtistId === candidate.aicArtistId);
              return (
                <Card key={candidate.aicId} variant="outlined">
                  <CardContent>
                    <Stack spacing={1.5}>
                      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1}>
                        <Box>
                          <Typography variant="h6">{reference?.airOriginalName ?? `Referencia ${candidate.aicInventoryReferenceId}`}</Typography>
                          <Typography variant="body2" color="text.secondary">
                            Candidato: {profile?.apeArtistName ?? candidate.aicCandidateUrl ?? candidate.aicExternalId ?? 'Sin identidad asignada'}
                          </Typography>
                        </Box>
                        <Stack direction="row" spacing={1}>
                          <Chip size="small" label={formatConfidence(candidate.aicConfidence)} variant="outlined" />
                          <Chip size="small" label={candidate.aicStatus} color={statusColor(candidate.aicStatus)} />
                        </Stack>
                      </Stack>
                      <Box component="pre" sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere', fontSize: 12 }}>
                        {safeEvidence(candidate.aicEvidence)}
                      </Box>
                      {candidate.aicDecidedAt && (
                        <Typography variant="caption" color="text.secondary">
                          Decidido por {candidate.aicDecidedBy ?? 'sistema'} · {formatDate(candidate.aicDecidedAt)}
                          {candidate.aicDecisionNote ? ` · ${candidate.aicDecisionNote}` : ''}
                        </Typography>
                      )}
                      {candidate.aicStatus === 'pending' && (
                        <Stack spacing={1}>
                          {(candidate.aicArtistId === null || candidate.aicArtistId === undefined) && (
                            <FormControl size="small" sx={{ maxWidth: 420 }}>
                              <InputLabel id={`identity-target-${candidate.aicId}`}>Destino aprobado</InputLabel>
                              <Select
                                labelId={`identity-target-${candidate.aicId}`}
                                label="Destino aprobado"
                                value={identityTargets[candidate.aicId] ?? ''}
                                onChange={(event) => setIdentityTargets((current) => ({
                                  ...current,
                                  [candidate.aicId]: event.target.value,
                                }))}
                              >
                                <MenuItem value="new">Crear un perfil distinto</MenuItem>
                                {profiles.map((target) => (
                                  <MenuItem key={target.apeArtistId} value={String(target.apeArtistId)}>
                                    Vincular a {target.apeArtistName} (#{target.apeArtistId})
                                  </MenuItem>
                                ))}
                              </Select>
                            </FormControl>
                          )}
                          <Stack direction="row" spacing={1}>
                          <Button
                            variant="contained"
                            color="success"
                            disabled={(candidate.aicArtistId === null || candidate.aicArtistId === undefined)
                              && !identityTargets[candidate.aicId]}
                            onClick={() => identityMutation.mutate({
                              candidateId: candidate.aicId,
                              decision: {
                                aedDecision: 'approve',
                                aedEditedValue: candidate.aicArtistId === null || candidate.aicArtistId === undefined
                                  ? identityTargets[candidate.aicId]
                                  : undefined,
                              },
                            })}
                          >
                            Aprobar identidad
                          </Button>
                          <Button
                            color="error"
                            onClick={() => identityMutation.mutate({ candidateId: candidate.aicId, decision: { aedDecision: 'reject' } })}
                          >
                            Rechazar
                          </Button>
                          </Stack>
                        </Stack>
                      )}
                    </Stack>
                  </CardContent>
                </Card>
              );
            })}
          </Stack>
        )}

        {tab === 'audit' && (
          <Stack spacing={3}>
            <Typography variant="h5">Fuentes ({overview?.aeoSources.length ?? 0})</Typography>
            <Stack spacing={1}>
              {(overview?.aeoSources ?? []).map((source) => (
                <Card key={source.arsId} variant="outlined">
                  <CardContent>
                    <Typography component="a" href={source.arsSourceUrl} target="_blank" rel="noreferrer" sx={{ overflowWrap: 'anywhere' }}>
                      {source.arsSourceUrl}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {source.arsSourceType} · campos: {source.arsSupportedFields} · {formatDate(source.arsRetrievedAt)}
                    </Typography>
                    {source.arsAttribution && <Typography variant="caption">{source.arsAttribution}</Typography>}
                  </CardContent>
                </Card>
              ))}
            </Stack>
            <Typography variant="h5">Cambios ({overview?.aeoChanges.length ?? 0})</Typography>
            <Stack spacing={1}>
              {(overview?.aeoChanges ?? []).map((change) => (
                <Card key={change.afcId} variant="outlined">
                  <CardContent>
                    <Typography fontWeight={600}>{change.afcFieldName} · artista {change.afcArtistId}</Typography>
                    <Typography variant="body2" sx={{ overflowWrap: 'anywhere' }}>
                      {change.afcPreviousValue ?? '∅'} → {change.afcNewValue ?? '∅'}
                    </Typography>
                    <Typography variant="caption" color="text.secondary">
                      {change.afcActor} · {formatConfidence(change.afcConfidence)} · {formatDate(change.afcChangedAt)}
                    </Typography>
                  </CardContent>
                </Card>
              ))}
            </Stack>
            <Typography variant="h5">Medios ({overview?.aeoMedia.length ?? 0})</Typography>
            <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, 1fr)', lg: 'repeat(3, 1fr)' }, gap: 2 }}>
              {(overview?.aeoMedia ?? []).map((asset) => (
                <Card key={asset.amaId} variant="outlined">
                  <Box
                    component="img"
                    src={asset.amaPublicUrl}
                    alt={`Vista previa ${asset.amaAssetKind}`}
                    loading="lazy"
                    sx={{
                      width: '100%',
                      aspectRatio: '16 / 9',
                      objectFit: 'cover',
                      objectPosition: asset.amaFocalPoint ?? 'center',
                      bgcolor: 'action.hover',
                    }}
                  />
                  <CardContent>
                    <Typography fontWeight={600}>{asset.amaAssetKind}</Typography>
                    <Typography variant="body2" color="text.secondary">
                      {asset.amaWidth}×{asset.amaHeight} · {Math.round(asset.amaByteSize / 1024)} KB · {asset.amaMimeType}
                    </Typography>
                    <Typography variant="caption" color="text.secondary" display="block">
                      Fuente: {asset.amaSourceWidth}×{asset.amaSourceHeight} · {Math.round(asset.amaSourceByteSize / 1024)} KB · {asset.amaSourceMimeType}
                    </Typography>
                    <Typography variant="caption" color="text.secondary">
                      Derechos: {asset.amaRightsStatus}{asset.amaSourceAttribution ? ` · ${asset.amaSourceAttribution}` : ''}
                    </Typography>
                  </CardContent>
                </Card>
              ))}
            </Box>
            <Typography variant="h5">Ejecuciones ({overview?.aeoRuns.length ?? 0})</Typography>
            <Stack spacing={1}>
              {(overview?.aeoRuns ?? []).map((run) => (
                <Card key={run.aerId} variant="outlined">
                  <CardContent>
                    <Stack direction="row" justifyContent="space-between" alignItems="center" spacing={1}>
                      <Box sx={{ minWidth: 0 }}>
                        <Typography fontWeight={600} sx={{ overflowWrap: 'anywhere' }}>{run.aerRunKey}</Typography>
                        <Typography variant="body2" color="text.secondary">
                          {run.aerMode} · {run.aerScope} · {formatDate(run.aerStartedAt)}
                        </Typography>
                      </Box>
                      <Chip size="small" label={run.aerStatus} color={statusColor(run.aerStatus)} />
                    </Stack>
                    {run.aerCounters && (
                      <Box component="pre" sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere', fontSize: 12 }}>
                        {safeEvidence(run.aerCounters)}
                      </Box>
                    )}
                  </CardContent>
                </Card>
              ))}
            </Stack>
          </Stack>
        )}
      </Stack>

      <Dialog open={editDialog != null} onClose={() => setEditDialog(null)} fullWidth maxWidth="md">
        <DialogTitle>Editar y aprobar {editDialog?.suggestion.aesFieldName}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ pt: 1 }}>
            <TextField
              label="Valor aprobado"
              value={editDialog?.editedValue ?? ''}
              onChange={(event) => setEditDialog((current) => current ? { ...current, editedValue: event.target.value } : current)}
              multiline
              minRows={3}
            />
            <TextField
              label="Nota de decisión"
              value={editDialog?.note ?? ''}
              onChange={(event) => setEditDialog((current) => current ? { ...current, note: event.target.value } : current)}
              multiline
              minRows={2}
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setEditDialog(null)}>Cancelar</Button>
          <Button
            variant="contained"
            color="success"
            onClick={() => {
              if (!editDialog) return;
              decisionMutation.mutate({
                suggestionId: editDialog.suggestion.aesId,
                decision: {
                  aedDecision: 'approve',
                  aedEditedValue: editDialog.editedValue,
                  aedNote: editDialog.note || undefined,
                },
              });
            }}
          >
            Aprobar valor
          </Button>
        </DialogActions>
      </Dialog>
    </PageShell>
  );
}
