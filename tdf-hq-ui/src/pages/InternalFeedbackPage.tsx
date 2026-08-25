import AddIcon from '@mui/icons-material/Add';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import DownloadIcon from '@mui/icons-material/Download';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  Grid,
  LinearProgress,
  Link,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useMemo, useState } from 'react';
import { Link as RouterLink, useNavigate, useParams, useSearchParams } from 'react-router-dom';
import { Catalogs, type CatalogItem, type CatalogPage } from '../api/catalogs';
import {
  InternalFeedback,
  type InternalFeedbackCreate,
  type InternalFeedbackUpdate,
} from '../api/internalFeedback';
import type { InternalReportState, InternalReportType } from '../api/types';
import PageShell, { EmptyState } from '../components/PageShell';
import { useSession } from '../session/SessionContext';
import { hasInternshipsAdminAccess } from '../utils/accessControl';
import {
  internalReportAdminTransitions,
  internalReportContextDefaults,
  internalReportMutationsAllowed,
  internalReportRetestAllowed,
} from './internalFeedbackLogic';

const CATEGORY_CATALOG = 'feedback-categories';
const SEVERITY_CATALOG = 'feedback-severities';

const TYPE_LABELS: Record<InternalReportType, string> = {
  error: 'Error',
  suggestion: 'Sugerencia',
  idea: 'Idea',
  question: 'Pregunta',
  accessibility: 'Accesibilidad',
  permissions: 'Permisos',
  performance: 'Rendimiento',
  content_translation: 'Contenido o traducción',
};

const REPORT_TYPE_BY_CATEGORY_CODE: Record<string, InternalReportType> = {
  bug: 'error',
  suggestion: 'suggestion',
  idea: 'idea',
  question: 'question',
  accessibility: 'accessibility',
  permissions: 'permissions',
  performance: 'performance',
  content_translation: 'content_translation',
};

const STATE_LABELS: Record<InternalReportState, string> = {
  draft: 'Borrador',
  submitted: 'Enviado',
  received: 'Recibido',
  needs_information: 'Necesita información',
  confirmed: 'Confirmado',
  prioritized: 'Priorizado',
  in_progress: 'En progreso',
  ready_for_retest: 'Listo para retest',
  verified: 'Verificado',
  closed: 'Cerrado',
  duplicate: 'Duplicado',
  discarded: 'Descartado',
};

const publishedItems = (page?: CatalogPage): CatalogItem[] =>
  page?.items.filter((item) => item.active && item.workflowState === 'published' && !item.deprecatedAt) ?? [];

const defaultId = (page: CatalogPage | undefined, scopeKind: string) =>
  page?.defaults.find((item) => item.scopeKind === scopeKind && item.scopeId === 'global' && !item.localeId)?.entityId ?? '';

const internalReportTypeOptions = (categories: CatalogItem[]) => categories.flatMap((category) => {
  const reportType = REPORT_TYPE_BY_CATEGORY_CODE[category.code];
  return reportType ? [{ category, reportType }] : [];
});

const internalReportCategory = (categories: CatalogItem[], reportType: InternalReportType) =>
  internalReportTypeOptions(categories).find((option) => option.reportType === reportType)?.category;

const internalReportTypeLabel = (categories: CatalogItem[], reportType: InternalReportType) =>
  internalReportCategory(categories, reportType)?.name ?? TYPE_LABELS[reportType];

const errorMessage = (error: unknown, fallback: string) =>
  error instanceof Error && error.message.trim() ? error.message : fallback;

const saveBlob = (body: BlobPart, fileName: string, type: string) => {
  const url = URL.createObjectURL(new Blob([body], { type }));
  const anchor = document.createElement('a');
  anchor.href = url;
  anchor.download = fileName;
  anchor.click();
  URL.revokeObjectURL(url);
};

function useFeedbackCatalogs() {
  const query = useQuery({
    queryKey: ['catalogs', 'internal-feedback'],
    queryFn: () => Catalogs.listPublicBatch([CATEGORY_CATALOG, SEVERITY_CATALOG], { locale: 'es', page: 1, pageSize: 100 }),
    staleTime: 600_000,
  });
  const categoryPage = query.data?.catalogs.find((page) => page.catalog.code === CATEGORY_CATALOG);
  const severityPage = query.data?.catalogs.find((page) => page.catalog.code === SEVERITY_CATALOG);
  return {
    query,
    categories: publishedItems(categoryPage),
    severities: publishedItems(severityPage),
    defaultCategoryId: defaultId(categoryPage, 'feedback-category'),
    defaultSeverityId: defaultId(severityPage, 'feedback-severity'),
  };
}

function NewInternalReport() {
  const { session } = useSession();
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const catalogs = useFeedbackCatalogs();
  const typeOptions = internalReportTypeOptions(catalogs.categories);
  const reportContext = internalReportContextDefaults(searchParams, session?.roles);
  const [feedback, setFeedback] = useState<string | null>(null);
  const [form, setForm] = useState<InternalFeedbackCreate>({
    ifcTitle: '',
    ifcDescription: '',
    ifcCategoryId: '',
    ifcProposedSeverityId: '',
    ifcReportType: 'error',
    ifcModuleName: searchParams.get('module') || '',
    ifcFeatureName: searchParams.get('feature') || '',
    ifcEnvironment: reportContext.environment,
    ifcUrlOrScreen: '',
    ifcPlatform: 'web',
    ifcDevice: '',
    ifcBrowser: '',
    ifcLanguage: 'es',
    ifcAccountRole: reportContext.accountRole,
    ifcReproductionSteps: '',
    ifcExpectedResult: '',
    ifcActualResult: '',
    ifcFrequency: '',
    ifcTestCaseId: searchParams.get('testCaseId'),
    ifcTestExecutionId: searchParams.get('executionId'),
    ifcInternshipProjectId: searchParams.get('projectId'),
    ifcInternshipTaskId: searchParams.get('taskId'),
    ifcBlocking: false,
    ifcVideoLinks: '',
  });

  useEffect(() => {
    if (!form.ifcCategoryId) {
      const category = internalReportCategory(catalogs.categories, form.ifcReportType);
      if (category) setForm((current) => ({ ...current, ifcCategoryId: category.id }));
    }
    if (!form.ifcProposedSeverityId && catalogs.defaultSeverityId) {
      setForm((current) => ({ ...current, ifcProposedSeverityId: catalogs.defaultSeverityId }));
    }
  }, [catalogs.categories, catalogs.defaultSeverityId, form.ifcCategoryId, form.ifcProposedSeverityId, form.ifcReportType]);

  const mutation = useMutation({
    mutationFn: () => InternalFeedback.create(form),
    onSuccess: (report) => navigate(`/feedback/interno/${report.ifrSummary.ifsId}`),
    onError: (error) => setFeedback(errorMessage(error, 'No se pudo guardar el borrador.')),
  });

  const update = <K extends keyof InternalFeedbackCreate>(key: K, value: InternalFeedbackCreate[K]) =>
    setForm((current) => ({ ...current, [key]: value }));

  const updateReportType = (reportType: InternalReportType) => {
    const selectedDraftCategory = internalReportCategory(catalogs.categories, reportType);
    setForm((current) => ({
      ...current,
      ifcReportType: reportType,
      ifcCategoryId: selectedDraftCategory?.id ?? '',
    }));
  };

  const errorRequired = form.ifcReportType === 'error';
  const canSave = form.ifcTitle.trim() && form.ifcDescription.trim() && form.ifcCategoryId
    && form.ifcProposedSeverityId && form.ifcModuleName.trim() && form.ifcPlatform.trim()
    && form.ifcAccountRole.trim();

  return (
    <PageShell title="Crear reporte interno" subtitle="Guarda un borrador antes de enviarlo" maxWidth="md" actions={<Button component={RouterLink} to="/feedback/interno" startIcon={<ArrowBackIcon />}>Mis reportes</Button>}>
      <Stack spacing={2.5}>
        <Alert severity="info">Registra cada hallazgo no relacionado por separado. Sé preciso y no exageres la gravedad: tu severidad es una propuesta que el equipo revisará.</Alert>
        {feedback && <Alert severity="error">{feedback}</Alert>}
        {catalogs.query.isError && <Alert severity="error">No se pudieron cargar los catálogos publicados.</Alert>}
        <Card variant="outlined"><CardContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={8}><TextField label="Título claro" value={form.ifcTitle} onChange={(event) => update('ifcTitle', event.target.value)} required fullWidth /></Grid>
            <Grid item xs={12} md={4}><TextField select label="Tipo de reporte" value={form.ifcReportType} onChange={(event) => updateReportType(event.target.value as InternalReportType)} disabled={!typeOptions.length} fullWidth>{typeOptions.map(({ category, reportType }) => <MenuItem key={category.id} value={reportType}>{category.name}</MenuItem>)}</TextField></Grid>
            <Grid item xs={12}><TextField label="Qué estabas intentando hacer y qué observaste" value={form.ifcDescription} onChange={(event) => update('ifcDescription', event.target.value)} required fullWidth multiline minRows={4} /></Grid>
            <Grid item xs={12} md={6}><TextField select label="Gravedad propuesta" value={form.ifcProposedSeverityId} onChange={(event) => update('ifcProposedSeverityId', event.target.value)} fullWidth disabled={!catalogs.severities.length}>{catalogs.severities.map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</TextField></Grid>
            <Grid item xs={12} md={6}><TextField label="Módulo" value={form.ifcModuleName} onChange={(event) => update('ifcModuleName', event.target.value)} required fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="Función" value={form.ifcFeatureName ?? ''} onChange={(event) => update('ifcFeatureName', event.target.value)} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField select label="Entorno" value={form.ifcEnvironment} onChange={(event) => update('ifcEnvironment', event.target.value)} disabled={reportContext.auditLinked} helperText={reportContext.auditLinked ? 'Heredado del caso de auditoría vinculado.' : undefined} fullWidth><MenuItem value="staging">Staging</MenuItem><MenuItem value="test">Pruebas</MenuItem><MenuItem value="local">Local</MenuItem><MenuItem value="production-read-only">Producción: sólo lectura autorizada</MenuItem></TextField></Grid>
            <Grid item xs={12} md={6}><TextField label="Rol bajo prueba" value={form.ifcAccountRole} onChange={(event) => update('ifcAccountRole', event.target.value)} helperText={reportContext.auditLinked ? 'Heredado del caso; corrígelo si ejecutaste con otro rol autorizado.' : 'Indica el rol con el que reprodujiste el hallazgo.'} required fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField select label="Plataforma" value={form.ifcPlatform} onChange={(event) => update('ifcPlatform', event.target.value)} fullWidth><MenuItem value="web">Web</MenuItem><MenuItem value="native-mobile">App móvil</MenuItem><MenuItem value="mobile-web">Web móvil</MenuItem><MenuItem value="api">API</MenuItem></TextField></Grid>
            <Grid item xs={12} md={6}><TextField label="Idioma" value={form.ifcLanguage} onChange={(event) => update('ifcLanguage', event.target.value)} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="Pantalla o URL" value={form.ifcUrlOrScreen ?? ''} onChange={(event) => update('ifcUrlOrScreen', event.target.value)} fullWidth /></Grid>
            <Grid item xs={12} md={3}><TextField label="Dispositivo" value={form.ifcDevice ?? ''} onChange={(event) => update('ifcDevice', event.target.value)} fullWidth /></Grid>
            <Grid item xs={12} md={3}><TextField label="Navegador" value={form.ifcBrowser ?? ''} onChange={(event) => update('ifcBrowser', event.target.value)} fullWidth /></Grid>
            <Grid item xs={12}><TextField label="Pasos exactos para repetirlo" value={form.ifcReproductionSteps ?? ''} onChange={(event) => update('ifcReproductionSteps', event.target.value)} required={errorRequired} fullWidth multiline minRows={4} /></Grid>
            <Grid item xs={12} md={6}><TextField label="Qué esperabas" value={form.ifcExpectedResult ?? ''} onChange={(event) => update('ifcExpectedResult', event.target.value)} required={errorRequired} fullWidth multiline minRows={3} /></Grid>
            <Grid item xs={12} md={6}><TextField label="Qué ocurrió" value={form.ifcActualResult ?? ''} onChange={(event) => update('ifcActualResult', event.target.value)} required={errorRequired} fullWidth multiline minRows={3} /></Grid>
            <Grid item xs={12} md={6}><TextField label="Frecuencia" placeholder="Siempre, 2 de 3 veces, intermitente…" value={form.ifcFrequency ?? ''} onChange={(event) => update('ifcFrequency', event.target.value)} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="Enlaces de video" helperText="Usa enlaces HTTPS; no cargues videos pesados aquí." value={form.ifcVideoLinks ?? ''} onChange={(event) => update('ifcVideoLinks', event.target.value)} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField select label="¿Este hallazgo bloquea la prueba?" value={form.ifcBlocking ? 'yes' : 'no'} onChange={(event) => update('ifcBlocking', event.target.value === 'yes')} fullWidth><MenuItem value="no">No</MenuItem><MenuItem value="yes">Sí</MenuItem></TextField></Grid>
          </Grid>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="flex-end" sx={{ mt: 3 }}>
            <Button component={RouterLink} to="/feedback/interno">Cancelar</Button>
            <Button variant="contained" onClick={() => mutation.mutate()} disabled={mutation.isPending || !canSave}>Guardar borrador</Button>
          </Stack>
        </CardContent></Card>
      </Stack>
    </PageShell>
  );
}

function ReportDetail({ reportId }: { reportId: string }) {
  const { session } = useSession();
  const queryClient = useQueryClient();
  const detailCatalogs = useFeedbackCatalogs();
  const isAdmin = hasInternshipsAdminAccess(session?.roles, session?.modules);
  const [message, setMessage] = useState<{ severity: 'success' | 'error'; text: string } | null>(null);
  const [comment, setComment] = useState('');
  const [attachment, setAttachment] = useState<File | null>(null);
  const [evidenceUrl, setEvidenceUrl] = useState('');
  const [adminUpdate, setAdminUpdate] = useState<InternalFeedbackUpdate>({});
  const [reporterUpdate, setReporterUpdate] = useState<InternalFeedbackUpdate>({});
  const [retestResult, setRetestResult] = useState('passed');
  const [retestNotes, setRetestNotes] = useState('');

  const reportQuery = useQuery({
    queryKey: ['internal-feedback', reportId],
    queryFn: () => InternalFeedback.get(reportId),
  });
  const refresh = async () => {
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: ['internal-feedback', reportId] }),
      queryClient.invalidateQueries({ queryKey: ['internal-feedback', 'list'] }),
    ]);
  };
  const action = useMutation({
    mutationFn: async (run: () => Promise<unknown>) => run(),
    onSuccess: async () => { setMessage({ severity: 'success', text: 'Cambio guardado y registrado en el historial.' }); await refresh(); },
    onError: (error) => setMessage({ severity: 'error', text: errorMessage(error, 'No se pudo guardar el cambio.') }),
  });

  const report = reportQuery.data;
  useEffect(() => {
    if (!report) return;
    setReporterUpdate({
      ifuTitle: report.ifrSummary.ifsTitle,
      ifuDescription: report.ifrDescription,
      ifuCategoryId: report.ifrCategoryId ?? undefined,
      ifuProposedSeverityId: report.ifrSummary.ifsProposedSeverityId ?? undefined,
      ifuReportType: report.ifrSummary.ifsReportType,
      ifuModuleName: report.ifrSummary.ifsModuleName,
      ifuFeatureName: report.ifrSummary.ifsFeatureName ?? null,
      ifuEnvironment: report.ifrSummary.ifsEnvironment,
      ifuUrlOrScreen: report.ifrUrlOrScreen ?? null,
      ifuPlatform: report.ifrSummary.ifsPlatform,
      ifuDevice: report.ifrDevice ?? null,
      ifuBrowser: report.ifrBrowser ?? null,
      ifuLanguage: report.ifrLanguage,
      ifuAccountRole: report.ifrAccountRole,
      ifuReproductionSteps: report.ifrReproductionSteps ?? null,
      ifuExpectedResult: report.ifrExpectedResult ?? null,
      ifuActualResult: report.ifrActualResult ?? null,
      ifuFrequency: report.ifrFrequency ?? null,
      ifuBlocking: report.ifrSummary.ifsBlocking,
      ifuVideoLinks: report.ifrVideoLinks ?? null,
    });
  }, [report]);
  if (reportQuery.isLoading) return <LinearProgress aria-label="Cargando reporte" />;
  if (!report) return <EmptyState title="Reporte no disponible" description="No existe o no tienes permiso para verlo." />;
  const summary = report.ifrSummary;
  const reportIsMutable = internalReportMutationsAllowed(report.ifrAuditPlanMutable);
  const commentKind = summary.ifsState === 'needs_information' && !isAdmin ? 'information_response' : 'comment';
  const mayEditReporterFields = reportIsMutable && summary.ifsReporterPartyId === session?.partyId
    && (summary.ifsState === 'draft' || summary.ifsState === 'needs_information');
  const setReporterField = <K extends keyof InternalFeedbackUpdate>(key: K, value: InternalFeedbackUpdate[K]) =>
    setReporterUpdate((current) => ({ ...current, [key]: value }));
  const setReporterReportType = (reportType: InternalReportType) => {
    const selectedDetailCategory = internalReportCategory(detailCatalogs.categories, reportType);
    setReporterUpdate((current) => ({
      ...current,
      ifuReportType: reportType,
      ifuCategoryId: selectedDetailCategory?.id,
    }));
  };
  const saveReporterUpdate = () => {
    if (summary.ifsState === 'draft') return InternalFeedback.update(reportId, reporterUpdate);
    const informationUpdate = { ...reporterUpdate };
    delete informationUpdate.ifuBlocking;
    return InternalFeedback.update(reportId, informationUpdate);
  };

  return (
    <PageShell title={summary.ifsTitle} subtitle={`${internalReportTypeLabel(detailCatalogs.categories, summary.ifsReportType)} · ${STATE_LABELS[summary.ifsState]}`} maxWidth="lg" actions={<Button component={RouterLink} to="/feedback/interno" startIcon={<ArrowBackIcon />}>Reportes</Button>}>
      <Stack spacing={2.5}>
        {message && <Alert severity={message.severity}>{message.text}</Alert>}
        {!reportIsMutable && <Alert severity="info">La auditoría terminó. Este reporte y su evidencia permanecen disponibles en modo de sólo lectura.</Alert>}
        <Stack direction="row" spacing={1} flexWrap="wrap">
          <Chip label={STATE_LABELS[summary.ifsState]} color={summary.ifsBlocking ? 'error' : 'default'} />
          <Chip label={summary.ifsModuleName} variant="outlined" />
          {summary.ifsFeatureName && <Chip label={summary.ifsFeatureName} variant="outlined" />}
          {summary.ifsPriority && <Chip label={`Prioridad: ${summary.ifsPriority}`} />}
        </Stack>
        <Card variant="outlined"><CardContent><Stack spacing={1.5}>
          <Typography sx={{ whiteSpace: 'pre-wrap' }}>{report.ifrDescription}</Typography>
          <Divider />
          <Typography><strong>Rol:</strong> {report.ifrAccountRole} · <strong>Entorno:</strong> {summary.ifsEnvironment} · <strong>Plataforma:</strong> {summary.ifsPlatform}</Typography>
          {report.ifrReproductionSteps && <Typography sx={{ whiteSpace: 'pre-wrap' }}><strong>Pasos:</strong>{'\n'}{report.ifrReproductionSteps}</Typography>}
          {report.ifrExpectedResult && <Typography sx={{ whiteSpace: 'pre-wrap' }}><strong>Esperado:</strong>{'\n'}{report.ifrExpectedResult}</Typography>}
          {report.ifrActualResult && <Typography sx={{ whiteSpace: 'pre-wrap' }}><strong>Ocurrió:</strong>{'\n'}{report.ifrActualResult}</Typography>}
          {summary.ifsDuplicateOf && <Alert severity="info">Este reporte es duplicado de <Link component={RouterLink} to={`/feedback/interno/${summary.ifsDuplicateOf}`}>{summary.ifsDuplicateOf}</Link>.</Alert>}
          {reportIsMutable && summary.ifsState === 'draft' && <Button variant="contained" onClick={() => action.mutate(() => InternalFeedback.submit(reportId))}>Enviar reporte</Button>}
        </Stack></CardContent></Card>

        {mayEditReporterFields && <Card variant="outlined"><CardContent><Stack spacing={2}>
          <Typography variant="h6">{summary.ifsState === 'draft' ? 'Editar borrador' : 'Ampliar información solicitada'}</Typography>
          <Alert severity="info">Guarda los cambios antes de enviar. El historial registra qué campos cambiaron.</Alert>
          <Grid container spacing={2}>
            <Grid item xs={12} md={8}><TextField label="Título" value={reporterUpdate.ifuTitle ?? ''} onChange={(event) => setReporterField('ifuTitle', event.target.value)} required fullWidth /></Grid>
            <Grid item xs={12} md={4}><TextField select label="Tipo de reporte" value={reporterUpdate.ifuReportType ?? 'error'} onChange={(event) => setReporterReportType(event.target.value as InternalReportType)} disabled={!detailCatalogs.categories.length} fullWidth>{internalReportTypeOptions(detailCatalogs.categories).map(({ category, reportType }) => <MenuItem key={category.id} value={reportType}>{category.name}</MenuItem>)}</TextField></Grid>
            <Grid item xs={12}><TextField label="Descripción" value={reporterUpdate.ifuDescription ?? ''} onChange={(event) => setReporterField('ifuDescription', event.target.value)} required fullWidth multiline minRows={4} /></Grid>
            <Grid item xs={12} md={6}><TextField select label="Gravedad propuesta" value={reporterUpdate.ifuProposedSeverityId ?? ''} onChange={(event) => setReporterField('ifuProposedSeverityId', event.target.value)} fullWidth>{detailCatalogs.severities.map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</TextField></Grid>
            <Grid item xs={12} md={6}><TextField label="Módulo" value={reporterUpdate.ifuModuleName ?? ''} onChange={(event) => setReporterField('ifuModuleName', event.target.value)} required fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="Función" value={reporterUpdate.ifuFeatureName ?? ''} onChange={(event) => setReporterField('ifuFeatureName', event.target.value || null)} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField select label="Entorno" value={reporterUpdate.ifuEnvironment ?? ''} onChange={(event) => setReporterField('ifuEnvironment', event.target.value)} fullWidth><MenuItem value="staging">Staging</MenuItem><MenuItem value="test">Pruebas</MenuItem><MenuItem value="local">Local</MenuItem><MenuItem value="production-read-only">Producción: sólo lectura autorizada</MenuItem></TextField></Grid>
            <Grid item xs={12} md={6}><TextField label="Rol bajo prueba" value={reporterUpdate.ifuAccountRole ?? ''} onChange={(event) => setReporterField('ifuAccountRole', event.target.value)} required fullWidth /></Grid>
            <Grid item xs={12}><TextField label="Pasos exactos" value={reporterUpdate.ifuReproductionSteps ?? ''} onChange={(event) => setReporterField('ifuReproductionSteps', event.target.value || null)} fullWidth multiline minRows={3} /></Grid>
            <Grid item xs={12} md={6}><TextField label="Resultado esperado" value={reporterUpdate.ifuExpectedResult ?? ''} onChange={(event) => setReporterField('ifuExpectedResult', event.target.value || null)} fullWidth multiline minRows={3} /></Grid>
            <Grid item xs={12} md={6}><TextField label="Resultado observado" value={reporterUpdate.ifuActualResult ?? ''} onChange={(event) => setReporterField('ifuActualResult', event.target.value || null)} fullWidth multiline minRows={3} /></Grid>
            <Grid item xs={12} md={6}><TextField label="Frecuencia" value={reporterUpdate.ifuFrequency ?? ''} onChange={(event) => setReporterField('ifuFrequency', event.target.value || null)} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="Enlaces HTTPS de video" value={reporterUpdate.ifuVideoLinks ?? ''} onChange={(event) => setReporterField('ifuVideoLinks', event.target.value || null)} fullWidth /></Grid>
            {summary.ifsState === 'draft' && <Grid item xs={12} md={6}><TextField select label="¿Bloquea la prueba?" value={reporterUpdate.ifuBlocking ? 'yes' : 'no'} onChange={(event) => setReporterField('ifuBlocking', event.target.value === 'yes')} fullWidth><MenuItem value="no">No</MenuItem><MenuItem value="yes">Sí</MenuItem></TextField></Grid>}
          </Grid>
          <Button variant="contained" onClick={() => action.mutate(saveReporterUpdate)}>Guardar cambios</Button>
        </Stack></CardContent></Card>}

        {report.ifrPotentialDuplicates.length > 0 && <Alert severity="warning"><Typography fontWeight={700}>Posibles reportes similares</Typography>{report.ifrPotentialDuplicates.map((candidate) => <Link key={candidate.ifsId} component={RouterLink} display="block" to={`/feedback/interno/${candidate.ifsId}`}>{candidate.ifsTitle} · {STATE_LABELS[candidate.ifsState]}</Link>)}</Alert>}

        <Grid container spacing={2}>
          <Grid item xs={12} md={6}><Card variant="outlined"><CardContent><Stack spacing={2}>
            <Typography variant="h6">Evidencia</Typography>
            {report.ifrEvidence.map((item) => <Box key={item.ifeId}>{item.ifeExternalUrl ? <Link href={item.ifeExternalUrl} target="_blank" rel="noreferrer">{item.ifeCaption || item.ifeExternalUrl}</Link> : <Button size="small" startIcon={<DownloadIcon />} onClick={() => action.mutate(async () => { const blob = await InternalFeedback.downloadEvidence(reportId, item.ifeId); saveBlob(blob, item.ifeOriginalFileName || 'evidencia', item.ifeContentType || 'application/octet-stream'); })}>{item.ifeOriginalFileName}</Button>}</Box>)}
            {reportIsMutable && <>
              <Button component="label" startIcon={<UploadFileIcon />} variant="outlined">{attachment?.name || 'Elegir captura o documento'}<input hidden type="file" accept="image/png,image/jpeg,image/webp,application/pdf,text/plain" onChange={(event) => setAttachment(event.target.files?.[0] || null)} /></Button>
              <Button disabled={!attachment} onClick={() => attachment && action.mutate(() => InternalFeedback.uploadEvidence(reportId, attachment))}>Adjuntar archivo</Button>
              <TextField label="Enlace HTTPS de video" value={evidenceUrl} onChange={(event) => setEvidenceUrl(event.target.value)} />
              <Button disabled={!evidenceUrl.trim()} onClick={() => action.mutate(() => InternalFeedback.linkEvidence(reportId, evidenceUrl))}>Agregar enlace</Button>
            </>}
          </Stack></CardContent></Card></Grid>
          <Grid item xs={12} md={6}><Card variant="outlined"><CardContent><Stack spacing={2}>
            <Typography variant="h6">Conversación</Typography>
            {report.ifrComments.map((item) => <Box key={item.ifcmId} sx={{ borderLeft: 3, borderColor: item.ifcmKind === 'information_request' ? 'warning.main' : 'divider', pl: 1.5 }}><Typography variant="caption">{item.ifcmAuthorName} · {item.ifcmKind}</Typography><Typography sx={{ whiteSpace: 'pre-wrap' }}>{item.ifcmBody}</Typography></Box>)}
            {reportIsMutable && <>
              <TextField label={commentKind === 'information_response' ? 'Información solicitada' : 'Comentario'} value={comment} onChange={(event) => setComment(event.target.value)} multiline minRows={3} />
              <Button disabled={!comment.trim()} onClick={() => action.mutate(async () => { await InternalFeedback.comment(reportId, comment, commentKind); setComment(''); })}>Agregar comentario</Button>
              {isAdmin && <Button color="warning" disabled={!comment.trim()} onClick={() => action.mutate(async () => { await InternalFeedback.comment(reportId, comment, 'information_request'); setComment(''); })}>Solicitar información</Button>}
            </>}
          </Stack></CardContent></Card></Grid>
        </Grid>

        {internalReportRetestAllowed(summary.ifsState, summary.ifsTestCaseId, report.ifrAuditPlanMutable) && <Card variant="outlined"><CardContent><Stack spacing={2}>
          <Typography variant="h6">Registrar retest</Typography>
          <TextField select label="Resultado" value={retestResult} onChange={(event) => setRetestResult(event.target.value)}><MenuItem value="passed">Aprobado</MenuItem><MenuItem value="failed">Fallido</MenuItem><MenuItem value="blocked">Bloqueado</MenuItem></TextField>
          <TextField label="Qué comprobaste y evidencia" value={retestNotes} onChange={(event) => setRetestNotes(event.target.value)} multiline minRows={3} />
          <Button variant="contained" disabled={!retestNotes.trim()} onClick={() => action.mutate(() => InternalFeedback.retest(reportId, { ifrcResult: retestResult, ifrcNotes: retestNotes, ifrcEvidenceSummary: retestNotes }))}>Guardar retest</Button>
        </Stack></CardContent></Card>}

        {isAdmin && reportIsMutable && <Card variant="outlined"><CardContent><Stack spacing={2}>
          <Typography variant="h6">Triage administrativo</Typography>
          <Grid container spacing={2}>
            <Grid item xs={12} md={4}><TextField select label="Nuevo estado" value={adminUpdate.ifuState ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuState: event.target.value as InternalReportState }))} fullWidth><MenuItem value="">Sin cambio</MenuItem>{internalReportAdminTransitions(summary.ifsState, summary.ifsTestCaseId).map((state) => <MenuItem key={state} value={state}>{STATE_LABELS[state]}</MenuItem>)}</TextField></Grid>
            <Grid item xs={12} md={4}><TextField select label="Severidad administrativa" value={adminUpdate.ifuAuthoritativeSeverityId ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuAuthoritativeSeverityId: event.target.value || null }))} fullWidth><MenuItem value="">Sin cambio</MenuItem>{detailCatalogs.severities.map((item) => <MenuItem key={item.id} value={item.id}>{item.name}</MenuItem>)}</TextField></Grid>
            <Grid item xs={12} md={4}><TextField select label="Prioridad" value={adminUpdate.ifuPriority ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuPriority: event.target.value || null }))} fullWidth><MenuItem value="">Sin cambio</MenuItem><MenuItem value="low">Baja</MenuItem><MenuItem value="medium">Media</MenuItem><MenuItem value="high">Alta</MenuItem><MenuItem value="urgent">Urgente</MenuItem></TextField></Grid>
            <Grid item xs={12}><TextField label="Resolución" value={adminUpdate.ifuResolution ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuResolution: event.target.value || null }))} fullWidth multiline /></Grid>
            <Grid item xs={12} md={6}><TextField label="Motivo de cierre" value={adminUpdate.ifuClosureReason ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuClosureReason: event.target.value || null }))} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="ID de reporte canónico si es duplicado" value={adminUpdate.ifuDuplicateOf ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuDuplicateOf: event.target.value || null }))} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="Party ID responsable" type="number" value={adminUpdate.ifuAssignedTo ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuAssignedTo: event.target.value ? Number(event.target.value) : null }))} helperText={report.ifrAssignedTo ? `Actual: ${report.ifrAssignedTo}` : 'Usa un Party ID existente.'} fullWidth /></Grid>
            <Grid item xs={12} md={6}><TextField label="Issue de GitHub confirmado" value={adminUpdate.ifuGithubIssueUrl ?? ''} onChange={(event) => setAdminUpdate((current) => ({ ...current, ifuGithubIssueUrl: event.target.value || null }))} helperText={report.ifrGithubIssueUrl || 'Sólo https://github.com/owner/repo/issues/número'} fullWidth /></Grid>
          </Grid>
          <Button variant="contained" onClick={() => action.mutate(() => InternalFeedback.update(reportId, adminUpdate))}>Guardar triage</Button>
        </Stack></CardContent></Card>}

        <Card variant="outlined"><CardContent><Typography variant="h6" gutterBottom>Historial auditable</Typography><Stack spacing={1}>{report.ifrHistory.map((item) => <Typography key={item.ifhId} variant="body2">{new Date(item.ifhCreatedAt).toLocaleString()} · {item.ifhActorName} · {item.ifhAction}{item.ifhNewState ? ` → ${item.ifhNewState}` : ''}</Typography>)}</Stack></CardContent></Card>
      </Stack>
    </PageShell>
  );
}

function ReportsList() {
  const { session } = useSession();
  const listCatalogs = useFeedbackCatalogs();
  const canAdministerList = hasInternshipsAdminAccess(session?.roles, session?.modules);
  const [state, setState] = useState('');
  const [moduleName, setModuleName] = useState('');
  const [search, setSearch] = useState('');
  const reportsQuery = useQuery({
    queryKey: ['internal-feedback', 'list', state, moduleName, search],
    queryFn: () => InternalFeedback.list({ state, module: moduleName, q: search }),
  });
  const legacyQuery = useQuery({
    queryKey: ['internal-feedback', 'legacy'],
    queryFn: InternalFeedback.listLegacy,
    enabled: canAdministerList,
  });
  const modules = useMemo(() => [...new Set((reportsQuery.data ?? []).map((item) => item.ifsModuleName))].sort(), [reportsQuery.data]);

  return (
    <PageShell title={canAdministerList ? 'Reportes internos de pruebas' : 'Mis reportes'} subtitle="Seguimiento desde borrador hasta verificación y cierre" maxWidth="lg" actions={canAdministerList ? <Button component={RouterLink} to="/feedback/interno/nuevo" variant="contained" startIcon={<AddIcon />}>Crear reporte</Button> : undefined}>
      <Stack spacing={2}>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
          <TextField label="Buscar" value={search} onChange={(event) => setSearch(event.target.value)} />
          <TextField select label="Estado" value={state} onChange={(event) => setState(event.target.value)} sx={{ minWidth: 220 }}><MenuItem value="">Todos</MenuItem>{Object.entries(STATE_LABELS).map(([value, label]) => <MenuItem key={value} value={value}>{label}</MenuItem>)}</TextField>
          <TextField select label="Módulo" value={moduleName} onChange={(event) => setModuleName(event.target.value)} sx={{ minWidth: 220 }}><MenuItem value="">Todos</MenuItem>{modules.map((module) => <MenuItem key={module} value={module}>{module}</MenuItem>)}</TextField>
          {canAdministerList && <Button startIcon={<DownloadIcon />} onClick={() => { void InternalFeedback.exportCsv({ state, module: moduleName }).then((body) => saveBlob(body, 'reportes-internos.csv', 'text/csv;charset=utf-8')); }}>CSV</Button>}
          {canAdministerList && <Button startIcon={<DownloadIcon />} onClick={() => { void InternalFeedback.exportJson({ state, module: moduleName }).then((body) => saveBlob(JSON.stringify(body, null, 2), 'reportes-internos.json', 'application/json')); }}>JSON</Button>}
        </Stack>
        {reportsQuery.isLoading && <LinearProgress />}
        {reportsQuery.error && <Alert severity="error">{errorMessage(reportsQuery.error, 'No se pudieron cargar los reportes.')}</Alert>}
        {!reportsQuery.isLoading && !reportsQuery.data?.length && <EmptyState title="No hay reportes" description={canAdministerList ? 'Los borradores y reportes enviados aparecerán aquí.' : 'Crea reportes desde un caso de tu plan de auditoría activo para conservar su trazabilidad.'} actionLabel={canAdministerList ? 'Crear reporte' : undefined} actionHref={canAdministerList ? '/feedback/interno/nuevo' : undefined} />}
        <Grid container spacing={2}>{reportsQuery.data?.map((report) => <Grid item xs={12} md={6} key={report.ifsId}><Card variant="outlined"><CardContent><Stack spacing={1}><Stack direction="row" justifyContent="space-between" gap={1}><Typography variant="h6">{report.ifsTitle}</Typography><Chip size="small" label={STATE_LABELS[report.ifsState]} color={report.ifsBlocking ? 'error' : 'default'} /></Stack><Typography variant="body2">{internalReportTypeLabel(listCatalogs.categories, report.ifsReportType)} · {report.ifsModuleName}{report.ifsFeatureName ? ` / ${report.ifsFeatureName}` : ''}</Typography>{canAdministerList && <Typography variant="caption">Reportó: {report.ifsReporterName}</Typography>}<Button component={RouterLink} to={`/feedback/interno/${report.ifsId}`} variant="outlined">Abrir seguimiento</Button></Stack></CardContent></Card></Grid>)}</Grid>
        {canAdministerList && Boolean(legacyQuery.data?.length) && <Card variant="outlined"><CardContent><Stack spacing={1}><Typography variant="h6">Feedback público anterior</Typography><Typography variant="body2">Estos {legacyQuery.data?.length} registros continúan legibles para administradores y no se convirtieron silenciosamente en reportes internos.</Typography>{legacyQuery.data?.slice(0, 10).map((item) => <Box key={item.lfdId}><Typography fontWeight={700}>{item.lfdTitle}</Typography><Typography variant="body2" sx={{ whiteSpace: 'pre-wrap' }}>{item.lfdDescription}</Typography><Typography variant="caption">{new Date(item.lfdCreatedAt).toLocaleString()} · consentimiento: {item.lfdConsent ? 'sí' : 'no'} · adjunto: {item.lfdHasAttachment ? 'sí' : 'no'}</Typography></Box>)}</Stack></CardContent></Card>}
      </Stack>
    </PageShell>
  );
}

export default function InternalFeedbackPage() {
  const { reportId } = useParams();
  if (reportId === 'nuevo') return <NewInternalReport />;
  if (reportId) return <ReportDetail reportId={reportId} />;
  return <ReportsList />;
}
