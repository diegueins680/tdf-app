import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import BugReportOutlinedIcon from '@mui/icons-material/BugReportOutlined';
import CheckCircleOutlineIcon from '@mui/icons-material/CheckCircleOutline';
import {
  Accordion,
  AccordionDetails,
  AccordionSummary,
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  LinearProgress,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useMemo, useState, type FormEvent } from 'react';
import { Link as RouterLink, useParams } from 'react-router-dom';
import { InternAudit } from '../api/internAudit';
import type {
  InternDailySummaryCreate,
  InternExecutionStatus,
  InternTestCaseDTO,
  InternTestExecutionCreate,
} from '../api/types';
import PageShell, { EmptyState } from '../components/PageShell';
import { useSession } from '../session/SessionContext';
import { hasInternshipsAdminAccess } from '../utils/accessControl';

const STATUS_LABELS: Record<InternExecutionStatus, string> = {
  pending: 'Pendiente',
  in_progress: 'En progreso',
  passed: 'Aprobado',
  failed: 'Fallido',
  blocked: 'Bloqueado',
  not_applicable: 'No aplicable',
  ready_for_retest: 'Listo para retest',
  verified: 'Verificado',
};

const STATUS_COLORS: Record<InternExecutionStatus, 'default' | 'info' | 'success' | 'error' | 'warning'> = {
  pending: 'default',
  in_progress: 'info',
  passed: 'success',
  failed: 'error',
  blocked: 'warning',
  not_applicable: 'default',
  ready_for_retest: 'info',
  verified: 'success',
};

const TERMINAL_STATUSES: InternExecutionStatus[] = [
  'passed', 'failed', 'blocked', 'not_applicable', 'verified',
];

const errorMessage = (error: unknown, fallback: string) =>
  error instanceof Error && error.message.trim() ? error.message : fallback;

const emptyExecution: InternTestExecutionCreate = {
  itecStatus: 'in_progress',
  itecActualResult: '',
  itecPersistedStateObserved: '',
  itecSideEffectsObserved: '',
  itecBlockerReason: '',
  itecEvidenceSummary: '',
};

const reportHref = (testCase: InternTestCaseDTO, plan: { iapProjectId: string; iapTaskId: string }) => {
  const query = new URLSearchParams({
    planId: testCase.itcPlanId,
    projectId: plan.iapProjectId,
    taskId: plan.iapTaskId,
    testCaseId: testCase.itcId,
    module: testCase.itcModuleName,
    feature: testCase.itcFeatureName,
  });
  if (testCase.itcLatestExecution?.itexId) {
    query.set('executionId', testCase.itcLatestExecution.itexId);
  }
  return `/feedback/interno/nuevo?${query.toString()}`;
};

function ExecutionHistory({ testCaseId }: { testCaseId: string }) {
  const query = useQuery({
    queryKey: ['intern-audit', 'executions', testCaseId],
    queryFn: () => InternAudit.listExecutions(testCaseId),
  });

  if (query.isLoading) return <LinearProgress aria-label="Cargando historial de ejecuciones" />;
  if (!query.data?.length) return <Typography variant="body2">Todavía no hay ejecuciones.</Typography>;
  return (
    <Stack spacing={1}>
      {query.data.map((execution) => (
        <Box key={execution.itexId} sx={{ borderLeft: 3, borderColor: 'divider', pl: 1.5 }}>
          <Typography variant="body2" fontWeight={700}>
            Ejecución {execution.itexExecutionNumber}: {STATUS_LABELS[execution.itexStatus]}
          </Typography>
          <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap' }}>
            {execution.itexActualResult || execution.itexBlockerReason || 'Sin observación.'}
          </Typography>
        </Box>
      ))}
    </Stack>
  );
}

export default function InternAuditPlanPage() {
  const { planId = '' } = useParams();
  const { session } = useSession();
  const queryClient = useQueryClient();
  const isAdmin = hasInternshipsAdminAccess(session?.roles, session?.modules);
  const [moduleFilter, setModuleFilter] = useState('');
  const [statusFilter, setStatusFilter] = useState('');
  const [expandedCase, setExpandedCase] = useState<string | false>(false);
  const [executionForm, setExecutionForm] = useState<InternTestExecutionCreate>(emptyExecution);
  const [feedback, setFeedback] = useState<{ severity: 'success' | 'error'; text: string } | null>(null);
  const [daily, setDaily] = useState<InternDailySummaryCreate>({
    idscWorkDate: new Date().toISOString().slice(0, 10),
    idscMinutesWorked: 60,
    idscModulesTested: '',
    idscCasesCompleted: 0,
    idscReportsCreated: 0,
    idscBlockers: '',
    idscNextStep: '',
  });
  const [conclusions, setConclusions] = useState('');

  const planQuery = useQuery({
    queryKey: ['intern-audit', 'plan', planId],
    queryFn: () => InternAudit.getPlan(planId),
    enabled: Boolean(planId),
  });
  const casesQuery = useQuery({
    queryKey: ['intern-audit', 'cases', planId],
    queryFn: () => InternAudit.listCases(planId),
    enabled: Boolean(planId),
  });
  const dailyQuery = useQuery({
    queryKey: ['intern-audit', 'daily', planId],
    queryFn: () => InternAudit.listDailySummaries(planId),
    enabled: Boolean(planId),
  });
  const finalQuery = useQuery({
    queryKey: ['intern-audit', 'final', planId],
    queryFn: () => InternAudit.getFinalSummary(planId),
    enabled: Boolean(planId),
    retry: false,
  });

  const modules = useMemo(
    () => [...new Set((casesQuery.data ?? []).map((item) => item.itcModuleName))].sort(),
    [casesQuery.data],
  );
  const visibleCases = useMemo(() => (casesQuery.data ?? []).filter((item) => {
    const status = item.itcLatestExecution?.itexStatus ?? 'pending';
    return (!moduleFilter || item.itcModuleName === moduleFilter)
      && (!statusFilter || status === statusFilter);
  }), [casesQuery.data, moduleFilter, statusFilter]);

  const saveExecution = useMutation({
    mutationFn: ({ testCaseId, payload }: { testCaseId: string; payload: InternTestExecutionCreate }) =>
      InternAudit.createExecution(testCaseId, payload),
    onSuccess: async () => {
      setExecutionForm(emptyExecution);
      setFeedback({ severity: 'success', text: 'El resultado y la evidencia quedaron registrados.' });
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['intern-audit', 'cases', planId] }),
        queryClient.invalidateQueries({ queryKey: ['intern-audit', 'plan', planId] }),
        queryClient.invalidateQueries({ queryKey: ['intern-audit', 'executions'] }),
      ]);
    },
    onError: (error) => setFeedback({ severity: 'error', text: errorMessage(error, 'No se pudo guardar el resultado.') }),
  });

  const activatePlan = useMutation({
    mutationFn: () => InternAudit.activatePlan(planId),
    onSuccess: (plan) => {
      queryClient.setQueryData(['intern-audit', 'plan', planId], plan);
      setFeedback({ severity: 'success', text: 'El plan quedó activo y fue asignado.' });
    },
    onError: (error) => setFeedback({ severity: 'error', text: errorMessage(error, 'No se pudo activar el plan.') }),
  });

  const completePlan = useMutation({
    mutationFn: () => InternAudit.completePlan(planId),
    onSuccess: async (completedPlan) => {
      queryClient.setQueryData(['intern-audit', 'plan', planId], completedPlan);
      setFeedback({ severity: 'success', text: 'La revisión administrativa quedó aprobada y la auditoría se completó.' });
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['intern-audit', 'final', planId] }),
        queryClient.invalidateQueries({ queryKey: ['internships', 'tasks'] }),
      ]);
    },
    onError: (error) => setFeedback({ severity: 'error', text: errorMessage(error, 'No se pudo completar la auditoría.') }),
  });

  const saveDaily = useMutation({
    mutationFn: () => InternAudit.createDailySummary(planId, daily),
    onSuccess: async () => {
      setFeedback({ severity: 'success', text: 'Resumen de jornada guardado.' });
      setDaily((current) => ({ ...current, idscModulesTested: '', idscCasesCompleted: 0, idscReportsCreated: 0, idscBlockers: '', idscNextStep: '' }));
      await queryClient.invalidateQueries({ queryKey: ['intern-audit', 'daily', planId] });
    },
    onError: (error) => setFeedback({ severity: 'error', text: errorMessage(error, 'No se pudo guardar el resumen.') }),
  });

  const saveFinal = useMutation({
    mutationFn: (submit: boolean) => InternAudit.saveFinalSummary(planId, conclusions, submit),
    onSuccess: async (_, submitted) => {
      setFeedback({ severity: 'success', text: submitted ? 'Informe final enviado para revisión.' : 'Borrador del informe final guardado.' });
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['intern-audit', 'final', planId] }),
        queryClient.invalidateQueries({ queryKey: ['intern-audit', 'plan', planId] }),
      ]);
    },
    onError: (error) => setFeedback({ severity: 'error', text: errorMessage(error, 'No se pudo guardar el informe final.') }),
  });

  const submitExecution = (event: FormEvent, testCase: InternTestCaseDTO) => {
    event.preventDefault();
    setFeedback(null);
    saveExecution.mutate({ testCaseId: testCase.itcId, payload: executionForm });
  };

  const plan = planQuery.data;
  const loading = planQuery.isLoading || casesQuery.isLoading;

  return (
    <PageShell
      title="Auditoría funcional y de experiencia del manejo del estudio"
      subtitle="Plan de ejecución, evidencia y trazabilidad"
      loading={loading}
      maxWidth="lg"
      actions={(
        <Button component={RouterLink} to="/practicas" startIcon={<ArrowBackIcon />}>
          Volver a Prácticas
        </Button>
      )}
    >
      {!loading && (!plan || planQuery.error || casesQuery.error) && (
        <EmptyState title="Plan no disponible" description="No existe o no tienes permiso para verlo." />
      )}
      {plan && (
        <Stack spacing={3}>
          {feedback && <Alert severity={feedback.severity}>{feedback.text}</Alert>}
          <Alert severity="warning">
            <strong>Detente y contacta a Diego</strong> si una prueba puede afectar producción, una persona real,
            un cobro, credenciales, inventario operativo, permisos o servicios externos. No continúes ni intentes
            reparar el sistema por tu cuenta.
          </Alert>

          <Card variant="outlined">
            <CardContent>
              <Stack spacing={2}>
                <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={1}>
                  <Box>
                    <Typography variant="h6">Avance calculado: {plan.iapCalculatedProgress}%</Typography>
                    <Typography variant="body2" color="text.secondary">
                      {plan.iapExecutedCaseCount} de {plan.iapCaseCount} casos ejecutados · Entorno: {plan.iapEnvironment}
                    </Typography>
                  </Box>
                  <Stack direction="row" gap={1} flexWrap="wrap">
                    <Chip label={`Críticos pendientes: ${plan.iapCriticalRemaining}`} color={plan.iapCriticalRemaining ? 'error' : 'success'} />
                    <Chip label={`Bloqueos: ${plan.iapOpenBlockerCount}`} color={plan.iapOpenBlockerCount ? 'warning' : 'success'} />
                    <Chip label={`Fallos sin reporte: ${plan.iapFailedWithoutReport}`} color={plan.iapFailedWithoutReport ? 'error' : 'success'} />
                    <Chip label={`Evidencias pendientes: ${plan.iapEvidenceMissing}`} color={plan.iapEvidenceMissing ? 'warning' : 'success'} />
                  </Stack>
                </Stack>
                <LinearProgress variant="determinate" value={plan.iapCalculatedProgress} aria-label="Avance calculado" />
                {plan.iapCanComplete ? (
                  <Alert severity="success" icon={<CheckCircleOutlineIcon />}>Se cumplen los criterios automáticos; falta la aprobación administrativa final.</Alert>
                ) : (
                  <Typography variant="body2">La tarea no puede cerrarse mientras falten casos, reportes, evidencia, resúmenes o retests requeridos.</Typography>
                )}
                {isAdmin && plan.iapStatus === 'draft' && (
                  <Button
                    variant="contained"
                    color="warning"
                    onClick={() => {
                      if (window.confirm('Activar asignará la tarea y generará las notificaciones configuradas. ¿Continuar?')) activatePlan.mutate();
                    }}
                    disabled={activatePlan.isPending}
                  >
                    Activar plan aprobado
                  </Button>
                )}
                {isAdmin && plan.iapStatus === 'active' && plan.iapCanComplete && (
                  <Button
                    variant="contained"
                    color="success"
                    onClick={() => {
                      if (window.confirm('Aprobar la revisión final y completar esta auditoría. ¿Continuar?')) completePlan.mutate();
                    }}
                    disabled={completePlan.isPending}
                  >
                    Aprobar y completar auditoría
                  </Button>
                )}
              </Stack>
            </CardContent>
          </Card>

          <Box component="section" aria-labelledby="casos-heading">
            <Typography id="casos-heading" variant="h5" gutterBottom>Casos de prueba</Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} sx={{ mb: 2 }}>
              <TextField select label="Módulo" value={moduleFilter} onChange={(event) => setModuleFilter(event.target.value)} sx={{ minWidth: 220 }}>
                <MenuItem value="">Todos</MenuItem>
                {modules.map((module) => <MenuItem key={module} value={module}>{module}</MenuItem>)}
              </TextField>
              <TextField select label="Resultado" value={statusFilter} onChange={(event) => setStatusFilter(event.target.value)} sx={{ minWidth: 220 }}>
                <MenuItem value="">Todos</MenuItem>
                {Object.entries(STATUS_LABELS).map(([value, label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}
              </TextField>
            </Stack>
            {visibleCases.map((testCase) => {
              const latestStatus = testCase.itcLatestExecution?.itexStatus ?? 'pending';
              return (
                <Accordion
                  key={testCase.itcId}
                  expanded={expandedCase === testCase.itcId}
                  onChange={(_, expanded) => {
                    setExpandedCase(expanded ? testCase.itcId : false);
                    setExecutionForm(emptyExecution);
                  }}
                >
                  <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                    <Stack direction={{ xs: 'column', sm: 'row' }} gap={1} alignItems={{ sm: 'center' }} width="100%">
                      <Typography fontWeight={700}>{testCase.itcStableId}</Typography>
                      <Typography flex={1}>{testCase.itcFeatureName}</Typography>
                      <Chip size="small" label={STATUS_LABELS[latestStatus]} color={STATUS_COLORS[latestStatus]} />
                      <Chip size="small" label={testCase.itcCriticality} variant="outlined" />
                    </Stack>
                  </AccordionSummary>
                  <AccordionDetails>
                    <Stack spacing={2}>
                      <Typography><strong>Objetivo:</strong> {testCase.itcObjective}</Typography>
                      <Typography><strong>Para qué sirve:</strong> {testCase.itcBusinessPurpose}</Typography>
                      <Typography sx={{ whiteSpace: 'pre-wrap' }}><strong>Antes de comenzar:</strong>{'\n'}{testCase.itcPreconditions}</Typography>
                      <Typography sx={{ whiteSpace: 'pre-wrap' }}><strong>Datos ficticios:</strong>{'\n'}{testCase.itcRequiredTestData}</Typography>
                      <Typography sx={{ whiteSpace: 'pre-wrap' }}><strong>Pasos:</strong>{'\n'}{testCase.itcDetailedSteps}</Typography>
                      <Alert severity="info"><strong>Resultado esperado:</strong> {testCase.itcExpectedResult}<br /><strong>Estado guardado esperado:</strong> {testCase.itcExpectedPersistedState}<br /><strong>Efectos esperados:</strong> {testCase.itcExpectedSideEffects}</Alert>
                      {testCase.itcExploratoryCharter && <Alert severity="success"><strong>Exploración:</strong> {testCase.itcExploratoryCharter}</Alert>}
                      <Typography variant="body2"><strong>Evidencia:</strong> {testCase.itcEvidenceRequirement === 'strong' ? 'fuerte (captura, video externo seguro o referencia verificable)' : 'confirmación breve'}.</Typography>
                      <Divider />
                      {plan.iapStatus === 'active' ? <Box component="form" onSubmit={(event) => submitExecution(event, testCase)}>
                        <Stack spacing={2}>
                          <Typography variant="h6">Registrar nueva ejecución</Typography>
                          <TextField select label="Resultado" value={executionForm.itecStatus} onChange={(event) => setExecutionForm((current) => ({ ...current, itecStatus: event.target.value as InternExecutionStatus }))} required>
                            {Object.entries(STATUS_LABELS).map(([value, label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}
                          </TextField>
                          <TextField label="Qué ocurrió" value={executionForm.itecActualResult ?? ''} onChange={(event) => setExecutionForm((current) => ({ ...current, itecActualResult: event.target.value }))} multiline minRows={2} required={TERMINAL_STATUSES.includes(executionForm.itecStatus) && executionForm.itecStatus !== 'blocked'} />
                          <TextField label="Estado guardado que comprobaste" value={executionForm.itecPersistedStateObserved ?? ''} onChange={(event) => setExecutionForm((current) => ({ ...current, itecPersistedStateObserved: event.target.value }))} multiline minRows={2} />
                          <TextField label="Notificaciones o efectos observados" value={executionForm.itecSideEffectsObserved ?? ''} onChange={(event) => setExecutionForm((current) => ({ ...current, itecSideEffectsObserved: event.target.value }))} multiline minRows={2} />
                          {executionForm.itecStatus === 'blocked' && <TextField label="Motivo del bloqueo" value={executionForm.itecBlockerReason ?? ''} onChange={(event) => setExecutionForm((current) => ({ ...current, itecBlockerReason: event.target.value }))} required multiline minRows={2} />}
                          <TextField label="Resumen o enlace de evidencia" value={executionForm.itecEvidenceSummary ?? ''} onChange={(event) => setExecutionForm((current) => ({ ...current, itecEvidenceSummary: event.target.value }))} required={testCase.itcEvidenceRequirement === 'strong'} helperText="No incluyas contraseñas ni datos personales. Los videos pesados deben enlazarse desde un servicio externo autorizado." />
                          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                            <Button type="submit" variant="contained" disabled={saveExecution.isPending}>Guardar resultado</Button>
                            <Button component={RouterLink} to={reportHref(testCase, plan)} startIcon={<BugReportOutlinedIcon />} variant="outlined">Crear reporte vinculado</Button>
                          </Stack>
                        </Stack>
                      </Box> : <Alert severity="info">La auditoría finalizada conserva el historial en modo de solo lectura.</Alert>}
                      <Divider />
                      <Typography variant="h6">Historial preservado</Typography>
                      <ExecutionHistory testCaseId={testCase.itcId} />
                    </Stack>
                  </AccordionDetails>
                </Accordion>
              );
            })}
          </Box>

          <Card variant="outlined" component="section">
            <CardContent>
              <Stack spacing={2}>
                <Typography variant="h5">Resumen de la jornada</Typography>
                <Typography variant="body2">Registra primero tu salida en Prácticas y luego deja este resumen breve.</Typography>
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                  <TextField label="Fecha" type="date" value={daily.idscWorkDate} onChange={(event) => setDaily((current) => ({ ...current, idscWorkDate: event.target.value }))} InputLabelProps={{ shrink: true }} required />
                  <TextField label="Minutos trabajados" type="number" value={daily.idscMinutesWorked} onChange={(event) => setDaily((current) => ({ ...current, idscMinutesWorked: Number(event.target.value) }))} inputProps={{ min: 1, max: 1440 }} required />
                  <TextField label="Casos completados" type="number" value={daily.idscCasesCompleted} onChange={(event) => setDaily((current) => ({ ...current, idscCasesCompleted: Number(event.target.value) }))} inputProps={{ min: 0 }} />
                  <TextField label="Reportes creados" type="number" value={daily.idscReportsCreated} onChange={(event) => setDaily((current) => ({ ...current, idscReportsCreated: Number(event.target.value) }))} inputProps={{ min: 0 }} />
                </Stack>
                <TextField label="Módulos probados" value={daily.idscModulesTested} onChange={(event) => setDaily((current) => ({ ...current, idscModulesTested: event.target.value }))} required multiline />
                <TextField label="Bloqueos" value={daily.idscBlockers ?? ''} onChange={(event) => setDaily((current) => ({ ...current, idscBlockers: event.target.value }))} multiline />
                <TextField label="Próximo paso" value={daily.idscNextStep} onChange={(event) => setDaily((current) => ({ ...current, idscNextStep: event.target.value }))} required multiline />
                <Button variant="contained" onClick={() => saveDaily.mutate()} disabled={plan.iapStatus !== 'active' || saveDaily.isPending || !daily.idscModulesTested.trim() || !daily.idscNextStep.trim()}>Guardar resumen</Button>
                <Typography variant="body2">Jornadas registradas: {dailyQuery.data?.length ?? 0}</Typography>
              </Stack>
            </CardContent>
          </Card>

          <Card variant="outlined" component="section">
            <CardContent>
              <Stack spacing={2}>
                <Typography variant="h5">Informe final</Typography>
                {finalQuery.data?.ifsGeneratedSnapshot && (
                  <Alert severity="info"><pre style={{ whiteSpace: 'pre-wrap', margin: 0 }}>{JSON.stringify(JSON.parse(finalQuery.data.ifsGeneratedSnapshot), null, 2)}</pre></Alert>
                )}
                <TextField label="Conclusiones y tres recomendaciones prioritarias" value={conclusions || finalQuery.data?.ifsConclusions || ''} onChange={(event) => setConclusions(event.target.value)} multiline minRows={8} helperText="Incluye diferencias web/móvil, accesibilidad, riesgos restantes y lo que no pudiste encontrar o entender." />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  <Button variant="outlined" onClick={() => saveFinal.mutate(false)} disabled={plan.iapStatus !== 'active' || saveFinal.isPending}>Guardar borrador</Button>
                  <Button variant="contained" onClick={() => saveFinal.mutate(true)} disabled={plan.iapStatus !== 'active' || saveFinal.isPending || !(conclusions || finalQuery.data?.ifsConclusions || '').trim()}>Enviar a revisión final</Button>
                </Stack>
              </Stack>
            </CardContent>
          </Card>
        </Stack>
      )}
    </PageShell>
  );
}
