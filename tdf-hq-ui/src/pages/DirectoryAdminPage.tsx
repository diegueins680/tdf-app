import {
  Alert,
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
  Chip,
  CircularProgress,
  Container,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';

import { Directory } from '../api/directory';

type ReviewRecord = Record<string, unknown>;
type QueueKind = 'claims' | 'verifications' | 'moderation';
const moderationDecisionValues = ['dismiss', 'warn', 'pause', 'remove', 'suspend', 'close'] as const;
type ModerationDecision = typeof moderationDecisionValues[number];

const stringValue = (value: unknown, fallback = ''): string => typeof value === 'string' ? value : fallback;
const isModerationDecision = (value: string): value is ModerationDecision =>
  (moderationDecisionValues as readonly string[]).includes(value);

export default function DirectoryAdminPage() {
  const [queue, setQueue] = useState<QueueKind>('claims');
  const claims = useQuery({ queryKey: ['directory', 'admin', 'claims'], queryFn: Directory.adminClaims, enabled: queue === 'claims' });
  const verifications = useQuery({ queryKey: ['directory', 'admin', 'verifications'], queryFn: Directory.adminVerifications, enabled: queue === 'verifications' });
  const moderation = useQuery({ queryKey: ['directory', 'admin', 'moderation'], queryFn: Directory.moderationQueue, enabled: queue === 'moderation' });
  const selected = queue === 'claims' ? claims : queue === 'verifications' ? verifications : moderation;

  return (
    <Container component="main" id="main-content" maxWidth="xl" sx={{ py: 4 }}>
      <Stack spacing={3}>
        <Box>
          <Typography component="h1" variant="h3" fontWeight={900}>Confianza y moderación del directorio</Typography>
          <Typography color="text.secondary" mt={1}>Las decisiones requieren el módulo Admin, quedan auditadas y nunca convierten una profesión pública en permiso interno.</Typography>
        </Box>
        <Alert severity="warning">No apruebes identidad, propiedad, créditos ni consentimiento sin revisar la evidencia y la política operativa vigente. La fusión de duplicados permanece fuera de esta acción genérica.</Alert>
        <Tabs value={queue} onChange={(_, value: unknown) => { if (value === 'claims' || value === 'verifications' || value === 'moderation') setQueue(value); }} aria-label="Colas administrativas del directorio">
          <Tab value="claims" label="Reclamos" />
          <Tab value="verifications" label="Verificaciones" />
          <Tab value="moderation" label="Moderación" />
        </Tabs>
        {selected.isLoading && <CircularProgress />}
        {selected.isError && <Alert severity="error">No tienes acceso a esta cola o no se pudo cargar.</Alert>}
        {!selected.isLoading && !selected.isError && (selected.data?.length ?? 0) === 0 && <Alert severity="info">La cola está vacía.</Alert>}
        <Stack spacing={2}>
          {(selected.data ?? []).map((record) => <ReviewCard key={stringValue(record['id'])} kind={queue} record={record} />)}
        </Stack>
        <ProfileMergePanel />
      </Stack>
    </Container>
  );
}

function ProfileMergePanel() {
  const [sourceProfileId, setSourceProfileId] = useState('');
  const [targetProfileId, setTargetProfileId] = useState('');
  const [reason, setReason] = useState('Duplicado confirmado tras reconciliar referencias y procedencia.');
  const merge = useMutation({
    mutationFn: () => Directory.mergeProfiles({ sourceProfileId: sourceProfileId.trim(), targetProfileId: targetProfileId.trim(), reason: reason.trim() }),
  });
  return (
    <Card variant="outlined">
      <CardContent>
        <Stack spacing={2}>
          <Box>
            <Typography variant="h5" fontWeight={850}>Fusión no destructiva de duplicados</Typography>
            <Typography color="text.secondary">El perfil fuente queda como alias del canónico; sus referencias históricas y conteos se conservan y la operación queda auditada.</Typography>
          </Box>
          <TextField label="UUID del perfil duplicado (fuente)" value={sourceProfileId} onChange={(event) => setSourceProfileId(event.target.value)} />
          <TextField label="UUID del perfil canónico (destino)" value={targetProfileId} onChange={(event) => setTargetProfileId(event.target.value)} />
          <TextField label="Razón y evidencia revisada" value={reason} onChange={(event) => setReason(event.target.value)} multiline minRows={2} inputProps={{ minLength: 10, maxLength: 2000 }} />
          {merge.error && <Alert severity="error">{merge.error.message}</Alert>}
          {merge.isSuccess && <Alert severity="success">Fusión ejecutada. Verifica ambos slugs públicos y los conteos registrados.</Alert>}
        </Stack>
      </CardContent>
      <CardActions sx={{ px: 2, pb: 2 }}>
        <Button variant="contained" color="warning" onClick={() => merge.mutate()} disabled={merge.isPending || sourceProfileId.trim() === targetProfileId.trim() || !sourceProfileId.trim() || !targetProfileId.trim() || reason.trim().length < 10}>Fusionar perfiles</Button>
      </CardActions>
    </Card>
  );
}

function ReviewCard({ kind, record }: { kind: QueueKind; record: ReviewRecord }) {
  const client = useQueryClient();
  const [notes, setNotes] = useState('Decisión revisada según la política operativa del directorio.');
  const id = stringValue(record['id']);
  const title = stringValue(record['profileName']) || `${stringValue(record['targetKind'], kind)} · ${stringValue(record['targetId'], id)}`;
  const status = stringValue(record['status'], 'pendiente');
  const decide = useMutation({
    mutationFn: async (action: string) => {
      if (kind === 'claims') return Directory.setClaimStatus(id, action, notes);
      if (kind === 'verifications') return Directory.setVerificationStatus(id, action, notes);
      if (!isModerationDecision(action)) throw new Error('Acción de moderación no permitida.');
      return Directory.decideModeration(id, { decision: action, reasonCode: 'admin-review', notes });
    },
    onSuccess: () => client.invalidateQueries({ queryKey: ['directory', 'admin', kind] }),
  });
  const actions: readonly (readonly [string, string])[] = kind === 'claims'
    ? [['under_review', 'Tomar revisión'], ['approved', 'Aprobar'], ['rejected', 'Rechazar']]
    : kind === 'verifications'
      ? [['under_review', 'Tomar revisión'], ['verified', 'Verificar'], ['rejected', 'Rechazar']]
      : [['dismiss', 'Descartar'], ['warn', 'Advertir'], ['pause', 'Pausar'], ['remove', 'Retirar'], ['suspend', 'Suspender']];

  return (
    <Card variant="outlined">
      <CardContent>
        <Stack spacing={1.5}>
          <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={1}>
            <Typography variant="h6" fontWeight={800}>{title}</Typography>
            <Chip label={status} size="small" />
          </Stack>
          <Typography color="text.secondary">
            {kind === 'claims' ? `Tipo: ${stringValue(record['claimType'], 'reclamo')}` : kind === 'verifications' ? `Tipo: ${stringValue(record['verificationType'], 'verificación')}` : `Prioridad: ${stringValue(record['priority'], 'normal')}`}
          </Typography>
          <TextField label="Notas de revisión" value={notes} onChange={(event) => setNotes(event.target.value)} multiline minRows={2} inputProps={{ minLength: 10, maxLength: 5000 }} />
          {decide.error && <Alert severity="error">{decide.error.message}</Alert>}
        </Stack>
      </CardContent>
      <CardActions sx={{ flexWrap: 'wrap', px: 2, pb: 2 }}>
        {actions.map(([action, label]) => <Button key={action} onClick={() => decide.mutate(action)} disabled={decide.isPending || notes.trim().length < 10}>{label}</Button>)}
      </CardActions>
    </Card>
  );
}
