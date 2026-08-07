import { useMemo, useState } from 'react';
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Alert,
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
  Chip,
  CircularProgress,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Link as RouterLink, useNavigate, useSearchParams } from 'react-router-dom';
import { useTranslation } from 'react-i18next';

import {
  AccessRequests,
  type FeatureAccessRequestDTO,
  type FeatureAccessRequestStatus,
} from '../api/accessRequests';
import { getAnalyticsClient } from '../analytics/posthog';
import {
  evaluateFeatureAccess,
  featureLabel,
  getFeatureById,
  type FeatureAction,
} from '../features/featureRegistry';
import { useSession } from '../session/SessionContext';

const supportedActions = new Set<FeatureAction>([
  'discover', 'view', 'create', 'edit', 'delete', 'archive', 'deactivate',
  'import', 'export', 'submit', 'validate', 'approve', 'reject', 'assign',
  'publish', 'report', 'administer',
]);

const statusTone: Record<FeatureAccessRequestStatus, 'default' | 'info' | 'success' | 'error' | 'warning'> = {
  pending: 'info',
  approved: 'success',
  rejected: 'error',
  cancelled: 'default',
  expired: 'warning',
};

const copy = {
  es: {
    title: 'Solicitudes de acceso',
    newRequest: 'Nueva solicitud',
    review: 'Revisar solicitudes',
    empty: 'Todavía no tienes solicitudes de acceso.',
    pending: 'Pendiente',
    approved: 'Aprobada',
    rejected: 'Rechazada',
    cancelled: 'Cancelada',
    expired: 'Expirada',
    action: 'Acción',
    requested: 'Solicitada',
    justification: 'Justificación',
    reviewerNotes: 'Notas del revisor',
    cancel: 'Cancelar solicitud',
    audit: 'Historial completo',
    provisioning: 'Una aprobación acepta la solicitud para provisión. No modifica roles ni módulos automáticamente; el acceso solo cambia mediante un permiso compatible y auditable.',
    loadError: 'No se pudieron cargar las solicitudes.',
    createTitle: 'Solicitar acceso',
    missingTarget: 'El destino solicitado no existe, es técnico o no admite solicitudes de acceso.',
    alreadyAllowed: 'Ya tienes acceso a esta acción. No hace falta enviar una solicitud.',
    missingAccess: 'Categoría de acceso faltante',
    optionalReason: 'Justificación opcional',
    reasonHelp: 'Explica brevemente para qué necesitas esta acción. No incluyas datos personales ni secretos.',
    send: 'Enviar solicitud',
    sending: 'Enviando…',
    back: 'Volver a mis solicitudes',
    reviewTitle: 'Revisar solicitudes de acceso',
    requestReference: 'Solicitud',
    currentContext: 'Contexto al solicitar',
    reviewerNote: 'Nota del revisor',
    approve: 'Aprobar para provisión',
    reject: 'Rechazar',
    rejectionNote: 'Se requiere una nota clara para rechazar.',
    noReview: 'No hay solicitudes que puedas revisar en este estado.',
    status: 'Estado',
  },
  en: {
    title: 'Access requests',
    newRequest: 'New request',
    review: 'Review requests',
    empty: 'You do not have any access requests yet.',
    pending: 'Pending',
    approved: 'Approved',
    rejected: 'Rejected',
    cancelled: 'Cancelled',
    expired: 'Expired',
    action: 'Action',
    requested: 'Requested',
    justification: 'Justification',
    reviewerNotes: 'Reviewer notes',
    cancel: 'Cancel request',
    audit: 'Complete history',
    provisioning: 'Approval accepts the request for provisioning. It does not automatically change roles or modules; access changes only through a compatible, auditable permission.',
    loadError: 'Access requests could not be loaded.',
    createTitle: 'Request access',
    missingTarget: 'The requested destination does not exist, is technical, or is not eligible for access requests.',
    alreadyAllowed: 'You already have this action. No request is necessary.',
    missingAccess: 'Missing access category',
    optionalReason: 'Optional justification',
    reasonHelp: 'Briefly explain why you need this action. Do not include personal data or secrets.',
    send: 'Send request',
    sending: 'Sending…',
    back: 'Back to my requests',
    reviewTitle: 'Review access requests',
    requestReference: 'Request',
    currentContext: 'Context when requested',
    reviewerNote: 'Reviewer note',
    approve: 'Approve for provisioning',
    reject: 'Reject',
    rejectionNote: 'A clear reviewer note is required for rejection.',
    noReview: 'There are no requests you can review in this state.',
    status: 'Status',
  },
} as const;

function useAccessCopy() {
  const { i18n } = useTranslation();
  const activeLanguage = i18n.resolvedLanguage ?? i18n.language ?? 'es';
  const locale: 'es' | 'en' = activeLanguage.toLowerCase().startsWith('en') ? 'en' : 'es';
  return { locale, text: copy[locale] };
}

function formatDate(value: string, locale: 'es' | 'en') {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat(locale === 'en' ? 'en-US' : 'es-EC', {
    dateStyle: 'medium',
    timeStyle: 'short',
  }).format(date);
}

function requestTitle(request: FeatureAccessRequestDTO, locale: 'es' | 'en') {
  const feature = getFeatureById(request.featureId);
  return feature ? featureLabel(feature, locale) : locale === 'en' ? 'Unavailable feature' : 'Función no disponible';
}

function History({ request }: { request: FeatureAccessRequestDTO }) {
  const { locale, text } = useAccessCopy();
  if (request.history.length === 0) return null;
  return (
    <Accordion disableGutters elevation={0} sx={{ mt: 1 }}>
      <AccordionSummary expandIcon={<ExpandMoreIcon />}>
        <Typography variant="body2">{text.audit}</Typography>
      </AccordionSummary>
      <AccordionDetails>
        <Stack component="ol" spacing={1} sx={{ m: 0, pl: 2.5 }}>
          {request.history.map((entry) => (
            <Box component="li" key={entry.id}>
              <Typography variant="body2">
                {entry.transition} · {formatDate(entry.createdAt, locale)}
              </Typography>
              {entry.note ? <Typography variant="caption">{entry.note}</Typography> : null}
            </Box>
          ))}
        </Stack>
      </AccordionDetails>
    </Accordion>
  );
}

export default function AccessRequestsPage() {
  const { locale, text } = useAccessCopy();
  const queryClient = useQueryClient();
  const requestsQuery = useQuery({ queryKey: ['access-requests', 'mine'], queryFn: AccessRequests.listMine });
  const cancelMutation = useMutation({
    mutationFn: (requestId: number) => AccessRequests.cancel(requestId),
    onSuccess: (request) => {
      getAnalyticsClient().capture('feature_access_request_cancelled', {
        feature_id: request.featureId,
        feature_action: request.action,
      });
      void queryClient.invalidateQueries({ queryKey: ['access-requests'] });
    },
  });

  return (
    <Stack spacing={3} component="section" aria-labelledby="access-requests-title">
      <Box>
        <Typography id="access-requests-title" variant="h4" component="h1">{text.title}</Typography>
        <Typography color="text.secondary">{text.provisioning}</Typography>
      </Box>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
        <Button component={RouterLink} to="/solicitudes-acceso/nueva" variant="contained" sx={{ minHeight: 44 }}>
          {text.newRequest}
        </Button>
        <Button component={RouterLink} to="/solicitudes-acceso/revision" variant="outlined" sx={{ minHeight: 44 }}>
          {text.review}
        </Button>
      </Stack>
      {requestsQuery.isPending ? <CircularProgress aria-label={text.title} /> : null}
      {requestsQuery.isError ? <Alert severity="error">{text.loadError}</Alert> : null}
      {cancelMutation.isError ? <Alert severity="error">{cancelMutation.error.message}</Alert> : null}
      {requestsQuery.data?.length === 0 ? <Alert severity="info">{text.empty}</Alert> : null}
      <Stack spacing={2} aria-live="polite">
        {requestsQuery.data?.map((request) => (
          <Card key={request.id} variant="outlined">
            <CardContent>
              <Stack direction="row" justifyContent="space-between" alignItems="flex-start" gap={2}>
                <Box>
                  <Typography variant="h6" component="h2">{requestTitle(request, locale)}</Typography>
                  <Typography variant="body2">{text.action}: {request.action}</Typography>
                  <Typography variant="caption" color="text.secondary">
                    {text.requested}: {formatDate(request.requestedAt, locale)}
                  </Typography>
                </Box>
                <Chip label={text[request.status]} color={statusTone[request.status]} />
              </Stack>
              {request.justification ? (
                <Typography sx={{ mt: 2 }}><strong>{text.justification}:</strong> {request.justification}</Typography>
              ) : null}
              {request.reviewerNotes ? (
                <Alert severity={request.status === 'rejected' ? 'warning' : 'info'} sx={{ mt: 2 }}>
                  <strong>{text.reviewerNotes}:</strong> {request.reviewerNotes}
                </Alert>
              ) : null}
              <History request={request} />
            </CardContent>
            {request.status === 'pending' ? (
              <CardActions>
                <Button
                  color="error"
                  disabled={cancelMutation.isPending}
                  onClick={() => cancelMutation.mutate(request.id)}
                  sx={{ minHeight: 44 }}
                >
                  {text.cancel}
                </Button>
              </CardActions>
            ) : null}
          </Card>
        ))}
      </Stack>
    </Stack>
  );
}

export function NewAccessRequestPage() {
  const { locale, text } = useAccessCopy();
  const { session } = useSession();
  const [searchParams] = useSearchParams();
  const navigate = useNavigate();
  const [justification, setJustification] = useState('');
  const requestedFeatureId = searchParams.get('feature')?.trim() ?? '';
  const actionParam = searchParams.get('action')?.trim().toLowerCase() ?? 'view';
  const action = supportedActions.has(actionParam as FeatureAction) ? actionParam as FeatureAction : null;
  const feature = getFeatureById(requestedFeatureId);
  const decision = feature && action
    ? evaluateFeatureAccess(feature, {
        authenticated: Boolean(session),
        roles: session?.roles,
        modules: session?.modules,
      }, action)
    : null;
  const requestable = Boolean(feature && action && feature.accessRequestEligible && !feature.technical && decision?.state === 'locked');
  const createMutation = useMutation({
    mutationFn: () => AccessRequests.create({
      featureId: feature?.id ?? '',
      action: action ?? 'view',
      justification: justification.trim() || null,
    }),
    onSuccess: (request) => {
      getAnalyticsClient().capture('feature_access_request_submitted', {
        feature_id: request.featureId,
        feature_action: request.action,
      });
      navigate('/solicitudes-acceso', { replace: true });
    },
  });

  return (
    <Stack spacing={3} component="section" aria-labelledby="new-access-request-title" sx={{ maxWidth: 720 }}>
      <Typography id="new-access-request-title" variant="h4" component="h1">{text.createTitle}</Typography>
      {!feature || !action || !feature.accessRequestEligible || feature.technical ? (
        <Alert severity="error">{text.missingTarget}</Alert>
      ) : null}
      {decision?.state === 'allowed' ? <Alert severity="info">{text.alreadyAllowed}</Alert> : null}
      {decision?.state === 'locked' && feature ? (
        <Card variant="outlined">
          <CardContent>
            <Typography variant="h6" component="h2">{featureLabel(feature, locale)}</Typography>
            <Typography>{feature.description[locale]}</Typography>
            <Typography sx={{ mt: 2 }}>
              <strong>{text.action}:</strong> {action}
            </Typography>
            <Typography color="text.secondary">
              {text.missingAccess}: {[
                ...decision.missingRoles.map(() => locale === 'en' ? 'role' : 'rol'),
                ...decision.missingModules.map(() => locale === 'en' ? 'module' : 'módulo'),
              ].filter((value, index, values) => values.indexOf(value) === index).join(', ')}
            </Typography>
          </CardContent>
        </Card>
      ) : null}
      {requestable ? (
        <Box component="form" onSubmit={(event) => { event.preventDefault(); createMutation.mutate(); }}>
          <Stack spacing={2}>
            <TextField
              label={text.optionalReason}
              value={justification}
              onChange={(event) => setJustification(event.target.value)}
              helperText={`${text.reasonHelp} (${justification.length}/2000)`}
              multiline
              minRows={4}
              inputProps={{ maxLength: 2000 }}
              fullWidth
            />
            {createMutation.isError ? <Alert severity="error">{createMutation.error.message}</Alert> : null}
            <Button type="submit" variant="contained" disabled={createMutation.isPending} sx={{ minHeight: 44 }}>
              {createMutation.isPending ? text.sending : text.send}
            </Button>
          </Stack>
        </Box>
      ) : null}
      <Button component={RouterLink} to="/solicitudes-acceso" sx={{ minHeight: 44, alignSelf: 'flex-start' }}>
        {text.back}
      </Button>
    </Stack>
  );
}

function ReviewCard({ request, onChanged }: { request: FeatureAccessRequestDTO; onChanged: () => void }) {
  const { locale, text } = useAccessCopy();
  const [notes, setNotes] = useState('');
  const decisionMutation = useMutation({
    mutationFn: (decision: 'approved' | 'rejected') =>
      AccessRequests.decide(request.id, decision, notes.trim() || null),
    onSuccess: (updated) => {
      getAnalyticsClient().capture('feature_access_request_reviewed', {
        feature_id: updated.featureId,
        feature_action: updated.action,
        decision: updated.status,
      });
      onChanged();
    },
  });
  const context = useMemo(
    () => [...request.roleContext.map(() => locale === 'en' ? 'role' : 'rol'), ...request.moduleContext.map(() => locale === 'en' ? 'module' : 'módulo')],
    [locale, request.moduleContext, request.roleContext],
  );

  return (
    <Card variant="outlined">
      <CardContent>
        <Typography variant="h6" component="h2">{requestTitle(request, locale)}</Typography>
        <Typography>{text.requestReference}: #{request.id}</Typography>
        <Typography>{text.action}: {request.action}</Typography>
        <Typography color="text.secondary">{text.currentContext}: {context.join(', ') || '—'}</Typography>
        {request.justification ? <Alert severity="info" sx={{ mt: 2 }}>{request.justification}</Alert> : null}
        {request.status === 'pending' ? (
          <TextField
            label={text.reviewerNote}
            value={notes}
            onChange={(event) => setNotes(event.target.value)}
            helperText={text.rejectionNote}
            inputProps={{ maxLength: 2000 }}
            multiline
            minRows={3}
            fullWidth
            sx={{ mt: 2 }}
          />
        ) : null}
        {request.reviewerNotes ? <Typography sx={{ mt: 2 }}>{request.reviewerNotes}</Typography> : null}
        {decisionMutation.isError ? <Alert severity="error" sx={{ mt: 2 }}>{decisionMutation.error.message}</Alert> : null}
        <History request={request} />
      </CardContent>
      {request.status === 'pending' ? (
        <CardActions sx={{ flexWrap: 'wrap', gap: 1 }}>
          <Button
            variant="contained"
            disabled={decisionMutation.isPending}
            onClick={() => decisionMutation.mutate('approved')}
            sx={{ minHeight: 44 }}
          >
            {text.approve}
          </Button>
          <Button
            color="error"
            variant="outlined"
            disabled={decisionMutation.isPending || notes.trim() === ''}
            onClick={() => decisionMutation.mutate('rejected')}
            sx={{ minHeight: 44 }}
          >
            {text.reject}
          </Button>
        </CardActions>
      ) : null}
    </Card>
  );
}

export function AccessRequestReviewPage() {
  const { text } = useAccessCopy();
  const queryClient = useQueryClient();
  const [status, setStatus] = useState<FeatureAccessRequestStatus>('pending');
  const reviewQuery = useQuery({
    queryKey: ['access-requests', 'review', status],
    queryFn: () => AccessRequests.listReview(status),
  });

  return (
    <Stack spacing={3} component="section" aria-labelledby="review-access-requests-title">
      <Box>
        <Typography id="review-access-requests-title" variant="h4" component="h1">{text.reviewTitle}</Typography>
        <Typography color="text.secondary">{text.provisioning}</Typography>
      </Box>
      <FormControl sx={{ minWidth: 220, alignSelf: 'flex-start' }}>
        <InputLabel id="access-request-status-label">{text.status}</InputLabel>
        <Select
          labelId="access-request-status-label"
          label={text.status}
          value={status}
          onChange={(event) => setStatus(event.target.value as FeatureAccessRequestStatus)}
        >
          {(Object.keys(statusTone) as FeatureAccessRequestStatus[]).map((value) => (
            <MenuItem key={value} value={value}>{text[value]}</MenuItem>
          ))}
        </Select>
      </FormControl>
      {reviewQuery.isPending ? <CircularProgress aria-label={text.reviewTitle} /> : null}
      {reviewQuery.isError ? <Alert severity="error">{reviewQuery.error.message}</Alert> : null}
      {reviewQuery.data?.length === 0 ? <Alert severity="info">{text.noReview}</Alert> : null}
      <Stack spacing={2} aria-live="polite">
        {reviewQuery.data?.map((request) => (
          <ReviewCard
            key={request.id}
            request={request}
            onChanged={() => { void queryClient.invalidateQueries({ queryKey: ['access-requests'] }); }}
          />
        ))}
      </Stack>
    </Stack>
  );
}
