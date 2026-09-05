import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  FormControlLabel,
  Grid,
  IconButton,
  List,
  ListItem,
  ListItemText,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Tooltip,
  Typography,
} from '@mui/material';
import AddTaskIcon from '@mui/icons-material/AddTask';
import CampaignIcon from '@mui/icons-material/Campaign';
import CheckCircleOutlineIcon from '@mui/icons-material/CheckCircleOutline';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import GroupsIcon from '@mui/icons-material/Groups';
import PauseCircleOutlineIcon from '@mui/icons-material/PauseCircleOutline';
import PlayCircleOutlineIcon from '@mui/icons-material/PlayCircleOutline';
import PreviewIcon from '@mui/icons-material/Preview';
import StopCircleOutlinedIcon from '@mui/icons-material/StopCircleOutlined';

import {
  CampaignAutomations,
  type CampaignAutomation,
  type CampaignAutomationStatus,
  type CampaignAutomationTemplate,
  type CampaignEnrollment,
  type CampaignEnrollmentResult,
  type CampaignPreview,
} from '../api/campaignAutomations';
import type { PartySelectorOption } from '../api/partySelector';
import { PartyMultiSelector } from '../components/party-selector/PartySelector';
import PageShell from '../components/PageShell';
import { formatDateTimeForUser, formatNumberForUser, resolveRuntimeFormatOptions } from '../utils/formatters';

interface Notice {
  severity: 'success' | 'error' | 'info' | 'warning';
  message: string;
}

const statusLabels: Record<CampaignAutomationStatus, string> = {
  draft: 'Borrador',
  active: 'Activa',
  paused: 'Pausada',
  completed: 'Completada',
};

const statusColors: Record<
  CampaignAutomationStatus,
  'default' | 'success' | 'warning' | 'info'
> = {
  draft: 'default',
  active: 'success',
  paused: 'warning',
  completed: 'info',
};

const enrollmentLabels: Record<CampaignEnrollment['status'], string> = {
  scheduled: 'Programado',
  completed: 'Secuencia completa',
  stopped: 'Detenido',
  replied: 'Respondió',
  converted: 'Convertido',
};

const formatDateTime = (value?: string | null) => {
  if (!value) return '—';
  return formatDateTimeForUser(value);
};

const templateForAutomation = (
  templates: CampaignAutomationTemplate[],
  automation: CampaignAutomation,
) => templates.find((template) => template.key === automation.templateKey);

function Metric({
  label,
  value,
  tone = 'text.primary',
}: {
  label: string;
  value: number;
  tone?: string;
}) {
  return (
    <Box>
      <Typography variant="h6" color={tone}>
        {formatNumberForUser(value)}
      </Typography>
      <Typography variant="caption" color="text.secondary">
        {label}
      </Typography>
    </Box>
  );
}

interface CampaignCardProps {
  template: CampaignAutomationTemplate;
  automation?: CampaignAutomation;
  busy: boolean;
  onInstall: () => void;
  onEnroll: () => void;
  onPreview: () => void;
  onRecipients: () => void;
  onActivate: () => void;
  onPause: () => void;
}

function CampaignCard({
  template,
  automation,
  busy,
  onInstall,
  onEnroll,
  onPreview,
  onRecipients,
  onActivate,
  onPause,
}: CampaignCardProps) {
  return (
    <Card
      variant="outlined"
      sx={{
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        borderColor: automation?.status === 'active' ? 'success.main' : 'divider',
      }}
    >
      <CardContent sx={{ flex: 1 }}>
        <Stack spacing={2}>
          <Stack direction="row" justifyContent="space-between" gap={2} alignItems="flex-start">
            <Box>
              <Typography variant="h5">{template.name}</Typography>
              <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                {template.objective}
              </Typography>
            </Box>
            <Chip
              size="small"
              label={automation ? statusLabels[automation.status] : 'Sin crear'}
              color={automation ? statusColors[automation.status] : 'default'}
              variant={automation?.status === 'active' ? 'filled' : 'outlined'}
            />
          </Stack>

          <Alert severity="info" icon={false}>
            <Typography variant="subtitle2">Oferta</Typography>
            <Typography variant="body2">{template.offer}</Typography>
          </Alert>

          <Box>
            <Typography variant="subtitle2">Audiencia permitida</Typography>
            <Typography variant="body2" color="text.secondary">
              {template.audience}
            </Typography>
          </Box>

          <Divider />

          <Typography variant="subtitle2">
            Secuencia automática · {template.steps.length} mensajes
          </Typography>
          <List disablePadding>
            {template.steps.map((step) => (
              <ListItem key={step.position} disableGutters alignItems="flex-start">
                <Chip
                  size="small"
                  label={step.position === 1 ? 'Inicio' : `+${step.delayDays} días`}
                  sx={{ mr: 1.5, mt: 0.25, minWidth: 72 }}
                />
                <ListItemText
                  primary={step.providerTemplateName}
                  secondary={step.body.split('\n')[0]}
                  primaryTypographyProps={{ variant: 'body2', fontWeight: 700 }}
                  secondaryTypographyProps={{ variant: 'caption' }}
                />
              </ListItem>
            ))}
          </List>

          {automation && (
            <>
              <Divider />
              <Grid container spacing={2}>
                <Grid item xs={6} sm={3}>
                  <Metric label="Contactos" value={automation.enrollmentCount} />
                </Grid>
                <Grid item xs={6} sm={3}>
                  <Metric label="Programados" value={automation.scheduledCount} />
                </Grid>
                <Grid item xs={6} sm={3}>
                  <Metric label="Enviados" value={automation.sentCount} tone="success.main" />
                </Grid>
                <Grid item xs={6} sm={3}>
                  <Metric
                    label="Conversiones"
                    value={automation.convertedCount}
                    tone="success.main"
                  />
                </Grid>
                <Grid item xs={6} sm={3}>
                  <Metric label="Detenidos" value={automation.stoppedCount} />
                </Grid>
                <Grid item xs={6} sm={3}>
                  <Metric label="Fallidos" value={automation.failedCount} tone="error.main" />
                </Grid>
                <Grid item xs={6} sm={3}>
                  <Metric label="Tope diario" value={automation.dailyLimit} />
                </Grid>
              </Grid>
              <Typography variant="caption" color="text.secondary">
                Próximo inicio: {formatDateTime(automation.startAt)}
                {' · '}
                Última ejecución: {formatDateTime(automation.lastRunAt)}
              </Typography>
            </>
          )}
        </Stack>
      </CardContent>

      <CardActions sx={{ px: 2, pb: 2, flexWrap: 'wrap', gap: 1 }}>
        {!automation ? (
          <Button
            variant="contained"
            startIcon={busy ? <CircularProgress size={16} /> : <AddTaskIcon />}
            onClick={onInstall}
            disabled={busy}
          >
            Crear borrador
          </Button>
        ) : (
          <>
            <Button
              variant="outlined"
              startIcon={<GroupsIcon />}
              onClick={onEnroll}
              disabled={busy || automation.status === 'completed'}
            >
              Agregar contactos
            </Button>
            <Button variant="text" startIcon={<PreviewIcon />} onClick={onPreview}>
              Vista previa
            </Button>
            <Button variant="text" onClick={onRecipients}>
              Ver contactos
            </Button>
            {automation.status === 'active' ? (
              <Button
                color="warning"
                startIcon={<PauseCircleOutlineIcon />}
                onClick={onPause}
                disabled={busy}
              >
                Pausar
              </Button>
            ) : automation.status !== 'completed' ? (
              <Button
                color="success"
                variant="contained"
                startIcon={<PlayCircleOutlineIcon />}
                onClick={onActivate}
                disabled={busy || automation.scheduledCount === 0}
              >
                Activar envíos
              </Button>
            ) : null}
          </>
        )}
      </CardActions>
    </Card>
  );
}

export default function CampaignAutomationsPage() {
  const queryClient = useQueryClient();
  const [notice, setNotice] = useState<Notice | null>(null);
  const [enrollAutomation, setEnrollAutomation] = useState<CampaignAutomation | null>(null);
  const [selectedParties, setSelectedParties] = useState<PartySelectorOption[]>([]);
  const [activationAutomation, setActivationAutomation] = useState<CampaignAutomation | null>(null);
  const [activationConfirmed, setActivationConfirmed] = useState(false);
  const [previewAutomation, setPreviewAutomation] = useState<CampaignAutomation | null>(null);
  const [previewRows, setPreviewRows] = useState<CampaignPreview[]>([]);
  const [previewLoading, setPreviewLoading] = useState(false);
  const [recipientsAutomation, setRecipientsAutomation] = useState<CampaignAutomation | null>(null);

  const templatesQuery = useQuery({
    queryKey: ['campaign-automation-templates'],
    queryFn: CampaignAutomations.templates,
  });
  const automationsQuery = useQuery({
    queryKey: ['campaign-automations'],
    queryFn: CampaignAutomations.list,
  });
  const recipientsQuery = useQuery({
    queryKey: ['campaign-enrollments', recipientsAutomation?.id],
    queryFn: () => CampaignAutomations.enrollments(recipientsAutomation!.id),
    enabled: recipientsAutomation != null,
  });

  const templates = templatesQuery.data ?? [];
  const automations = automationsQuery.data ?? [];
  const automationByTemplate = useMemo(
    () => new Map(automations.map((automation) => [automation.templateKey, automation])),
    [automations],
  );

  const refreshCampaignData = async (automationId?: number) => {
    await queryClient.invalidateQueries({ queryKey: ['campaign-automations'] });
    if (automationId) {
      await queryClient.invalidateQueries({
        queryKey: ['campaign-enrollments', automationId],
      });
    }
  };

  const installMutation = useMutation({
    mutationFn: (template: CampaignAutomationTemplate) =>
      CampaignAutomations.install(template.key),
    onSuccess: async (automation) => {
      await refreshCampaignData();
      setNotice({
        severity: 'success',
        message: `${automation.name} quedó creada como borrador. No se enviaron mensajes.`,
      });
    },
    onError: (error: Error) => {
      setNotice({ severity: 'error', message: error.message });
    },
  });

  const enrollMutation = useMutation<
    CampaignEnrollmentResult,
    Error,
    { automationId: number; partyIds: number[] }
  >({
    mutationFn: ({ automationId, partyIds }) =>
      CampaignAutomations.enroll(automationId, partyIds),
    onSuccess: async (result, variables) => {
      await refreshCampaignData(variables.automationId);
      setEnrollAutomation(null);
      setSelectedParties([]);
      const accepted = result.acceptedPartyIds.length;
      const rejected = result.rejected.length;
      setNotice({
        severity: rejected > 0 ? 'warning' : 'success',
        message:
          `${accepted} contacto${accepted === 1 ? '' : 's'} programado${accepted === 1 ? '' : 's'}.`
          + (rejected > 0
            ? ` ${rejected} rechazado${rejected === 1 ? '' : 's'} por falta de consentimiento, teléfono válido o inscripción previa.`
            : ''),
      });
    },
    onError: (error) => {
      setNotice({ severity: 'error', message: error.message });
    },
  });

  const statusMutation = useMutation({
    mutationFn: ({
      automationId,
      status,
      templatesApproved,
    }: {
      automationId: number;
      status: CampaignAutomationStatus;
      templatesApproved?: boolean;
    }) => CampaignAutomations.setStatus(automationId, status, templatesApproved),
    onSuccess: async (automation) => {
      await refreshCampaignData(automation.id);
      setActivationAutomation(null);
      setActivationConfirmed(false);
      setNotice({
        severity: automation.status === 'active' ? 'success' : 'info',
        message:
          automation.status === 'active'
            ? `${automation.name} está activa. El worker procesará únicamente contactos consentidos y mensajes vencidos.`
            : `${automation.name} quedó ${statusLabels[automation.status].toLocaleLowerCase(resolveRuntimeFormatOptions().locale)}.`,
      });
    },
    onError: (error: Error) => {
      setNotice({ severity: 'error', message: error.message });
    },
  });

  const enrollmentStatusMutation = useMutation({
    mutationFn: ({
      automationId,
      enrollmentId,
      status,
    }: {
      automationId: number;
      enrollmentId: number;
      status: 'converted' | 'stopped';
    }) =>
      CampaignAutomations.setEnrollmentStatus(
        automationId,
        enrollmentId,
        status,
        status === 'converted' ? 'operator_marked_converted' : 'operator_stopped',
      ),
    onSuccess: async (_, variables) => {
      await refreshCampaignData(variables.automationId);
    },
    onError: (error: Error) => {
      setNotice({ severity: 'error', message: error.message });
    },
  });

  const openEnrollDialog = (automation: CampaignAutomation) => {
    setSelectedParties([]);
    setEnrollAutomation(automation);
  };

  const openPreview = async (automation: CampaignAutomation) => {
    setPreviewAutomation(automation);
    setPreviewRows([]);
    setPreviewLoading(true);
    try {
      setPreviewRows(await CampaignAutomations.preview(automation.id));
    } catch (error) {
      setNotice({
        severity: 'error',
        message: error instanceof Error ? error.message : 'No se pudo cargar la vista previa.',
      });
    } finally {
      setPreviewLoading(false);
    }
  };

  const copyText = async (value: string) => {
    try {
      await navigator.clipboard.writeText(value);
      setNotice({ severity: 'success', message: 'Mensaje copiado.' });
    } catch {
      setNotice({ severity: 'error', message: 'No se pudo copiar el mensaje.' });
    }
  };

  const loading =
    (templatesQuery.isLoading && templates.length === 0)
    || (automationsQuery.isLoading && automations.length === 0);
  const loadError = templatesQuery.error ?? automationsQuery.error;
  const allInstalled =
    templates.length > 0
    && templates.every((template) => automationByTemplate.has(template.key));

  const installMissing = async () => {
    const missing = templates.filter(
      (template) => !automationByTemplate.has(template.key),
    );
    for (const template of missing) {
      try {
        await CampaignAutomations.install(template.key);
      } catch (error) {
        setNotice({
          severity: 'error',
          message:
            error instanceof Error
              ? error.message
              : `No se pudo crear ${template.name}.`,
        });
        return;
      }
    }
    await refreshCampaignData();
    setNotice({
      severity: 'success',
      message: 'Las cuatro campañas quedaron creadas como borradores. No se enviaron mensajes.',
    });
  };

  return (
    <PageShell
      title="Campañas automáticas"
      subtitle="Secuencias de adquisición con consentimiento, plantillas aprobadas y detención automática."
      loading={loading}
      actions={(
        <Button
          variant="contained"
          startIcon={<CampaignIcon />}
          onClick={() => void installMissing()}
          disabled={allInstalled || loading}
        >
          {allInstalled ? 'Campañas creadas' : 'Crear campañas faltantes'}
        </Button>
      )}
    >
      <Stack spacing={3}>
        <Alert severity="warning">
          Crear o previsualizar una campaña no envía nada. Para activar envíos reales debes
          registrar consentimiento de WhatsApp, conseguir la aprobación de cada plantilla en
          Meta y confirmar la activación. La automatización se detiene al recibir una respuesta,
          al revocarse el consentimiento, al convertir el lead o al terminar la secuencia.
        </Alert>

        {notice && (
          <Alert severity={notice.severity} onClose={() => setNotice(null)}>
            {notice.message}
          </Alert>
        )}
        {loadError && (
          <Alert severity="error">
            {loadError instanceof Error
              ? loadError.message
              : 'No se pudieron cargar las campañas.'}
          </Alert>
        )}

        <Grid container spacing={3}>
          {templates.map((template) => {
            const automation = automationByTemplate.get(template.key);
            const busy =
              installMutation.isPending
              || statusMutation.isPending
              || enrollMutation.isPending;
            return (
              <Grid item xs={12} lg={6} key={template.key}>
                <CampaignCard
                  template={template}
                  automation={automation}
                  busy={busy}
                  onInstall={() => installMutation.mutate(template)}
                  onEnroll={() => automation && openEnrollDialog(automation)}
                  onPreview={() => automation && void openPreview(automation)}
                  onRecipients={() => automation && setRecipientsAutomation(automation)}
                  onActivate={() => {
                    if (!automation) return;
                    setActivationConfirmed(false);
                    setActivationAutomation(automation);
                  }}
                  onPause={() => {
                    if (!automation) return;
                    statusMutation.mutate({
                      automationId: automation.id,
                      status: 'paused',
                    });
                  }}
                />
              </Grid>
            );
          })}
        </Grid>
      </Stack>

      <Dialog
        open={enrollAutomation != null}
        onClose={() => !enrollMutation.isPending && setEnrollAutomation(null)}
        fullWidth
        maxWidth="md"
      >
        <DialogTitle>Agregar contactos · {enrollAutomation?.name}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <Alert severity="info">
              Puedes seleccionar contactos con teléfono, pero el backend solo inscribirá aquellos
              con consentimiento de WhatsApp activo y no revocado.
            </Alert>
            <PartyMultiSelector
              value={selectedParties}
              onChange={setSelectedParties}
              field={{
                label: 'Buscar contactos',
                required: true,
                helperText: 'Busca por nombre o username. Puedes conservar varias selecciones.',
              }}
              search={{ context: 'campaign_enrollment', kind: 'any', accountOnly: false }}
            />
            <Chip label={`${selectedParties.length} seleccionados`} sx={{ alignSelf: 'flex-start' }} />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setEnrollAutomation(null)} disabled={enrollMutation.isPending}>
            Cancelar
          </Button>
          <Button
            variant="contained"
            disabled={
              enrollMutation.isPending
              || selectedParties.length === 0
              || enrollAutomation == null
            }
            onClick={() => {
              if (!enrollAutomation) return;
              enrollMutation.mutate({
                automationId: enrollAutomation.id,
                partyIds: selectedParties.map((party) => party.partyId),
              });
            }}
          >
            {enrollMutation.isPending ? 'Validando…' : 'Programar contactos'}
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={activationAutomation != null}
        onClose={() => !statusMutation.isPending && setActivationAutomation(null)}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>Activar envíos reales</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            <Alert severity="warning">
              Al confirmar, el worker podrá empezar a enviar mensajes vencidos de
              {' '}
              <strong>{activationAutomation?.name}</strong>.
            </Alert>
            <FormControlLabel
              control={(
                <Checkbox
                  checked={activationConfirmed}
                  onChange={(event) => setActivationConfirmed(event.target.checked)}
                />
              )}
              label="Confirmo que los contactos dieron consentimiento y que las plantillas indicadas están aprobadas en Meta."
            />
            <Typography variant="body2" color="text.secondary">
              Tope de la campaña: {activationAutomation?.dailyLimit ?? 0} intentos por día.
              Respuestas y revocaciones detienen automáticamente el seguimiento.
            </Typography>
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setActivationAutomation(null)} disabled={statusMutation.isPending}>
            Cancelar
          </Button>
          <Button
            color="success"
            variant="contained"
            disabled={!activationConfirmed || statusMutation.isPending || !activationAutomation}
            onClick={() => {
              if (!activationAutomation) return;
              statusMutation.mutate({
                automationId: activationAutomation.id,
                status: 'active',
                templatesApproved: activationConfirmed,
              });
            }}
          >
            Activar campaña
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={previewAutomation != null}
        onClose={() => setPreviewAutomation(null)}
        fullWidth
        maxWidth="md"
      >
        <DialogTitle>Vista previa · {previewAutomation?.name}</DialogTitle>
        <DialogContent>
          {previewLoading ? (
            <Stack alignItems="center" sx={{ py: 6 }}>
              <CircularProgress />
            </Stack>
          ) : previewRows.length === 0 ? (
            <Alert severity="info" sx={{ mt: 1 }}>
              Agrega al menos un contacto consentido para ver la personalización del próximo paso.
              Las copias base y los nombres de plantilla siguen visibles en la tarjeta.
            </Alert>
          ) : (
            <Stack spacing={2} sx={{ mt: 1 }}>
              {previewRows.map((row) => (
                <Paper key={`${row.partyId}-${row.stepPosition}`} variant="outlined" sx={{ p: 2 }}>
                  <Stack direction="row" justifyContent="space-between" gap={2}>
                    <Box>
                      <Typography variant="subtitle2">
                        {row.partyName} · paso {row.stepPosition}
                      </Typography>
                      <Typography variant="caption" color="text.secondary">
                        {row.providerTemplateName} · {row.languageCode}
                      </Typography>
                    </Box>
                    <Tooltip title="Copiar mensaje">
                      <IconButton
                        tabIndex={0}
                        onClick={(event) => {
                          event.currentTarget.focus();
                          void copyText(row.renderedBody);
                        }}
                        aria-label={`Copiar mensaje personalizado para ${row.partyName}`}
                      >
                        <ContentCopyIcon />
                      </IconButton>
                    </Tooltip>
                  </Stack>
                  <Typography
                    component="pre"
                    variant="body2"
                    sx={{ whiteSpace: 'pre-wrap', fontFamily: 'inherit', mt: 2, mb: 0 }}
                  >
                    {row.renderedBody}
                  </Typography>
                </Paper>
              ))}
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setPreviewAutomation(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={recipientsAutomation != null}
        onClose={() => setRecipientsAutomation(null)}
        fullWidth
        maxWidth="lg"
      >
        <DialogTitle>Contactos · {recipientsAutomation?.name}</DialogTitle>
        <DialogContent>
          {recipientsQuery.isLoading ? (
            <Stack alignItems="center" sx={{ py: 6 }}>
              <CircularProgress />
            </Stack>
          ) : recipientsQuery.error ? (
            <Alert severity="error" sx={{ mt: 1 }}>
              {recipientsQuery.error.message}
            </Alert>
          ) : (
            <TableContainer component={Paper} variant="outlined" sx={{ mt: 1 }}>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Contacto</TableCell>
                    <TableCell>Consentimiento</TableCell>
                    <TableCell>Estado</TableCell>
                    <TableCell>Próximo paso</TableCell>
                    <TableCell>Último envío</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {(recipientsQuery.data ?? []).map((enrollment) => (
                    <TableRow key={enrollment.id}>
                      <TableCell>
                        <Typography variant="body2" fontWeight={700}>
                          {enrollment.partyName}
                        </Typography>
                        <Typography variant="caption" color="text.secondary">
                          {enrollment.phoneE164 || 'Sin teléfono'}
                        </Typography>
                      </TableCell>
                      <TableCell>
                        <Chip
                          size="small"
                          color={enrollment.consentActive ? 'success' : 'error'}
                          label={enrollment.consentActive ? 'Activo' : 'No activo'}
                          variant="outlined"
                        />
                      </TableCell>
                      <TableCell>
                        <Typography variant="body2">
                          {enrollmentLabels[enrollment.status]}
                        </Typography>
                        {enrollment.stopReason && (
                          <Typography variant="caption" color="text.secondary">
                            {enrollment.stopReason}
                          </Typography>
                        )}
                      </TableCell>
                      <TableCell>
                        Paso {enrollment.nextStepPosition}
                        <Typography display="block" variant="caption" color="text.secondary">
                          {formatDateTime(enrollment.nextRunAt)}
                        </Typography>
                      </TableCell>
                      <TableCell>{formatDateTime(enrollment.lastSentAt)}</TableCell>
                      <TableCell align="right">
                        {enrollment.status === 'scheduled' && recipientsAutomation && (
                          <Stack direction="row" justifyContent="flex-end">
                            <Tooltip title="Marcar convertido">
                              <IconButton
                                color="success"
                                aria-label={`Marcar a ${enrollment.partyName} como convertido`}
                                onClick={() =>
                                  enrollmentStatusMutation.mutate({
                                    automationId: recipientsAutomation.id,
                                    enrollmentId: enrollment.id,
                                    status: 'converted',
                                  })}
                              >
                                <CheckCircleOutlineIcon />
                              </IconButton>
                            </Tooltip>
                            <Tooltip title="Detener seguimiento">
                              <IconButton
                                color="warning"
                                aria-label={`Detener seguimiento de ${enrollment.partyName}`}
                                onClick={() =>
                                  enrollmentStatusMutation.mutate({
                                    automationId: recipientsAutomation.id,
                                    enrollmentId: enrollment.id,
                                    status: 'stopped',
                                  })}
                              >
                                <StopCircleOutlinedIcon />
                              </IconButton>
                            </Tooltip>
                          </Stack>
                        )}
                      </TableCell>
                    </TableRow>
                  ))}
                  {(recipientsQuery.data ?? []).length === 0 && (
                    <TableRow>
                      <TableCell colSpan={6}>
                        <Typography color="text.secondary">
                          Esta campaña todavía no tiene contactos.
                        </Typography>
                      </TableCell>
                    </TableRow>
                  )}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setRecipientsAutomation(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>
    </PageShell>
  );
}

export {
  templateForAutomation,
};
