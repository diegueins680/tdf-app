import { useEffect, useMemo, useState, type ReactNode } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  Grid,
  IconButton,
  Paper,
  Stack,
  TextField,
  Typography,
  MenuItem,
} from '@mui/material';
import AddCircleOutlineIcon from '@mui/icons-material/AddCircleOutline';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';
import KeyboardArrowUpIcon from '@mui/icons-material/KeyboardArrowUp';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DownloadIcon from '@mui/icons-material/Download';
import AutoAwesomeIcon from '@mui/icons-material/AutoAwesome';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import EventNoteIcon from '@mui/icons-material/EventNote';
import PhotoLibraryIcon from '@mui/icons-material/PhotoLibrary';
import AccessTimeIcon from '@mui/icons-material/AccessTime';
import ReceiptLongIcon from '@mui/icons-material/ReceiptLong';
import PageShell from '../components/PageShell';
import {
  buildWorkAccountReport,
  buildWorkAccountReportPdfBlob,
  formatCurrency,
  formatDateLabel,
  type CustomFieldSource,
  type WorkAccountReportSource,
  type WorkBlockSource,
} from '../features/finance/davidCelayaReport';

type BuilderBlock = WorkBlockSource;
type CustomFieldType = 'text' | 'number' | 'date' | 'currency';
interface BuilderCustomField extends CustomFieldSource {
  type: CustomFieldType;
}

type WorkAccountTemplateSource = ReturnType<typeof buildWorkAccountReport>['source'];

interface WorkAccountTemplate {
  id: string;
  name: string;
  source: WorkAccountTemplateSource;
  savedAt: string;
}

const TEMPLATE_STORAGE_KEY = 'tdf-work-account-report-builder-templates';

const exampleSource: WorkAccountReportSource = {
  personName: 'David Celaya',
  reportTitle: 'Reporte de cuenta',
  periodLabel: '9 y 10 de julio de 2026',
  hourlyRateCents: 2_500,
  notes: 'Bloques de trabajo creados desde el sistema.',
  customFields: [
    { id: 'scope', label: 'Alcance', value: 'Sesiones de estudio y apoyo operativo.' },
    { id: 'client', label: 'Cliente', value: 'David Celaya' },
  ],
  workBlocks: [
    {
      id: 'block-1',
      date: '2026-07-09',
      startTime: '14:35:10',
      endTime: '20:43:50',
      startPhotoLabel: 'Foto inicio 1',
      endPhotoLabel: 'Foto fin 1',
      description: 'Bloque continuo en estudio.',
      discountHours: 1,
      discountReason: 'Descuento explícito de 1 hora.',
    },
    {
      id: 'block-2',
      date: '2026-07-10',
      startTime: '17:37:28',
      endTime: '23:50:32',
      startPhotoLabel: 'Foto inicio 2',
      endPhotoLabel: 'Foto fin 2',
      description: 'Bloque nocturno en estudio.',
      discountHours: 2,
      discountReason: 'Descuento explícito de 2 horas.',
    },
  ],
};

const makeId = () => `block-${Math.random().toString(36).slice(2, 8)}`;
const makeFieldId = () => `field-${Math.random().toString(36).slice(2, 8)}`;

const todayIsoDate = () => new Date().toISOString().slice(0, 10) as `${number}-${number}-${number}`;

const emptyBlock = (): BuilderBlock => ({
  id: makeId(),
  date: todayIsoDate(),
  startTime: '09:00:00',
  endTime: '10:00:00',
  startPhotoLabel: 'Foto inicio',
  endPhotoLabel: 'Foto fin',
  description: 'Bloque de trabajo.',
  discountHours: 0,
  discountReason: 'Sin descuento.',
});

const emptyField = (): BuilderCustomField => ({
  id: makeFieldId(),
  label: 'Campo nuevo',
  value: '',
  type: 'text',
});

const cloneBlocks = (blocks: BuilderBlock[]) => blocks.map((block) => ({ ...block, id: block.id || makeId() }));
const cloneFields = (fields: CustomFieldSource[]) =>
  fields.map((field) => ({ ...field, id: field.id || makeFieldId(), type: field.type ?? 'text' }));

const hasWindow = () => typeof window !== 'undefined';

const fieldTypeOptions: { value: CustomFieldType; label: string; helper: string }[] = [
  { value: 'text', label: 'Texto', helper: 'Etiqueta libre.' },
  { value: 'number', label: 'Número', helper: 'Cifra o cantidad.' },
  { value: 'date', label: 'Fecha', helper: 'Dato de calendario.' },
  { value: 'currency', label: 'Moneda', helper: 'Importe en USD.' },
];

const fieldTypeLabels = fieldTypeOptions.reduce<Record<CustomFieldType, string>>((acc, option) => {
  acc[option.value] = option.label;
  return acc;
}, {} as Record<CustomFieldType, string>);

const readTemplates = (): WorkAccountTemplate[] => {
  if (!hasWindow()) return [];
  try {
    const raw = window.localStorage.getItem(TEMPLATE_STORAGE_KEY);
    if (!raw) return [];
    const parsed = JSON.parse(raw) as unknown;
    if (!Array.isArray(parsed)) return [];
    return parsed
      .map((item) => {
        if (!item || typeof item !== 'object') return null;
        const candidate = item as Partial<WorkAccountTemplate>;
        if (
          typeof candidate.id !== 'string'
          || typeof candidate.name !== 'string'
          || typeof candidate.savedAt !== 'string'
          || !candidate.source
        ) {
          return null;
        }
        return candidate as WorkAccountTemplate;
      })
      .filter((item): item is WorkAccountTemplate => item !== null);
  } catch {
    return [];
  }
};

const writeTemplates = (templates: WorkAccountTemplate[]) => {
  if (!hasWindow()) return;
  window.localStorage.setItem(TEMPLATE_STORAGE_KEY, JSON.stringify(templates));
};

const formatSavedAt = (iso: string) => {
  const date = new Date(iso);
  if (Number.isNaN(date.getTime())) return iso;
  return new Intl.DateTimeFormat('es-EC', {
    dateStyle: 'medium',
    timeStyle: 'short',
  }).format(date);
};

const builderCardSx = {
  borderRadius: 3,
  borderColor: 'rgba(15, 23, 42, 0.10)',
  boxShadow: '0 14px 30px rgba(15, 23, 42, 0.06)',
  overflow: 'hidden',
} as const;

function SectionTitle({ title, subtitle, action }: { title: string; subtitle?: string; action?: ReactNode }) {
  return (
    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} justifyContent="space-between" alignItems={{ sm: 'center' }}>
      <Box>
        <Typography variant="h6" fontWeight={850}>
          {title}
        </Typography>
        {subtitle && <Typography color="text.secondary">{subtitle}</Typography>}
      </Box>
      {action}
    </Stack>
  );
}

function StatCard({ icon, label, value, caption }: { icon: React.ReactNode; label: string; value: string; caption: string }) {
  return (
    <Paper variant="outlined" sx={{ p: 2, borderRadius: 3, height: '100%', bgcolor: 'rgba(255,255,255,0.75)' }}>
      <Stack spacing={1}>
        <Box sx={{ color: 'primary.main', display: 'flex' }}>{icon}</Box>
        <Typography variant="body2" color="text.secondary">
          {label}
        </Typography>
        <Typography variant="h6" fontWeight={850}>
          {value}
        </Typography>
        <Typography variant="caption" color="text.secondary">
          {caption}
        </Typography>
      </Stack>
    </Paper>
  );
}

export default function WorkAccountReportBuilderPage() {
  const [templates, setTemplates] = useState<WorkAccountTemplate[]>([]);
  const [templateName, setTemplateName] = useState('');
  const [selectedTemplateId, setSelectedTemplateId] = useState('');
  const [personName, setPersonName] = useState('');
  const [reportTitle, setReportTitle] = useState('Reporte de cuenta');
  const [periodLabel, setPeriodLabel] = useState('');
  const [hourlyRate, setHourlyRate] = useState('25');
  const [notes, setNotes] = useState('');
  const [asOfDate, setAsOfDate] = useState(todayIsoDate());
  const [blocks, setBlocks] = useState<BuilderBlock[]>([]);
  const [customFields, setCustomFields] = useState<BuilderCustomField[]>([]);
  const [pdfUrl, setPdfUrl] = useState('');

  useEffect(() => {
    setTemplates(readTemplates());
  }, []);

  const source = useMemo<WorkAccountReportSource>(
    () => ({
      personName: personName.trim() || 'Sin nombre',
      reportTitle: reportTitle.trim() || 'Reporte de cuenta',
      periodLabel: periodLabel.trim() || 'Sin periodo',
      hourlyRateCents: Math.max(0, Math.round(Number.parseFloat(hourlyRate.replace(',', '.')) * 100) || 0),
      notes: notes.trim() || 'Sin notas.',
      customFields: customFields
        .map((field) => ({
          id: field.id,
          label: field.label.trim() || 'Campo',
          value: field.value.trim() || 'Sin valor',
          type: field.type,
          // customFields are rendered as free-form text in the report PDF.
        }))
        .filter((field) => field.label || field.value),
      workBlocks: blocks.map((block) => ({
        ...block,
        discountHours: Math.max(0, Number.isFinite(Number(block.discountHours)) ? Number(block.discountHours) : 0),
        discountReason: block.discountReason.trim() || 'Sin descuento.',
      })),
    }),
    [blocks, customFields, hourlyRate, notes, periodLabel, personName, reportTitle],
  );

  const report = useMemo(() => buildWorkAccountReport(source, { asOfDate }), [source, asOfDate]);

  useEffect(() => {
    const url = URL.createObjectURL(buildWorkAccountReportPdfBlob(report));
    setPdfUrl(url);
    return () => URL.revokeObjectURL(url);
  }, [report]);

  const loadExample = () => {
    setPersonName(exampleSource.personName);
    setReportTitle(exampleSource.reportTitle);
    setPeriodLabel(exampleSource.periodLabel);
    setHourlyRate('25');
    setNotes(exampleSource.notes);
    setAsOfDate('2026-07-10');
    setBlocks(cloneBlocks(exampleSource.workBlocks));
    setCustomFields(cloneFields(exampleSource.customFields ?? []));
  };

  const startBlank = () => {
    setPersonName('');
    setReportTitle('Reporte de cuenta');
    setPeriodLabel('');
    setHourlyRate('25');
    setNotes('');
    setAsOfDate(todayIsoDate());
    setBlocks([]);
    setCustomFields([]);
  };

  const addBlock = () => setBlocks((prev) => [...prev, emptyBlock()]);
  const addField = () => setCustomFields((prev) => [...prev, emptyField()]);

  const moveBlock = (id: string, direction: -1 | 1) => {
    setBlocks((prev) => {
      const index = prev.findIndex((block) => block.id === id);
      const target = index + direction;
      if (index < 0 || target < 0 || target >= prev.length) return prev;
      const next = [...prev];
      [next[index], next[target]] = [next[target]!, next[index]!];
      return next;
    });
  };

  const moveField = (id: string, direction: -1 | 1) => {
    setCustomFields((prev) => {
      const index = prev.findIndex((field) => field.id === id);
      const target = index + direction;
      if (index < 0 || target < 0 || target >= prev.length) return prev;
      const next = [...prev];
      [next[index], next[target]] = [next[target]!, next[index]!];
      return next;
    });
  };

  const updateBlock = (id: string, patch: Partial<BuilderBlock>) => {
    setBlocks((prev) => prev.map((block) => (block.id === id ? { ...block, ...patch } : block)));
  };

  const updateField = (id: string, patch: Partial<BuilderCustomField>) => {
    setCustomFields((prev) => prev.map((field) => (field.id === id ? { ...field, ...patch } : field)));
  };

  const removeBlock = (id: string) => setBlocks((prev) => prev.filter((block) => block.id !== id));
  const removeField = (id: string) => setCustomFields((prev) => prev.filter((field) => field.id !== id));

  const saveTemplate = () => {
    const name = templateName.trim();
    if (!name) return;
    const nextTemplate: WorkAccountTemplate = {
      id: selectedTemplateId || `template-${makeId()}`,
      name,
      source: report.source,
      savedAt: new Date().toISOString(),
    };
    const nextTemplates = (() => {
      const existingIndex = templates.findIndex((template) => template.id === nextTemplate.id);
      if (existingIndex >= 0) {
        const next = [...templates];
        next[existingIndex] = nextTemplate;
        return next;
      }
      return [nextTemplate, ...templates];
    })();
    setTemplates(nextTemplates);
    writeTemplates(nextTemplates);
    setSelectedTemplateId(nextTemplate.id);
  };

  const loadTemplate = (templateId: string) => {
    const template = templates.find((item) => item.id === templateId);
    if (!template) return;
    setSelectedTemplateId(template.id);
    setTemplateName(template.name);
    setPersonName(template.source.personName);
    setReportTitle(template.source.reportTitle);
    setPeriodLabel(template.source.periodLabel);
    setHourlyRate(String(template.source.hourlyRateCents / 100));
    setNotes(template.source.notes);
    setAsOfDate(template.source.cutoffDate ?? todayIsoDate());
    setBlocks(cloneBlocks(template.source.workBlocks));
    setCustomFields(cloneFields((template.source.customFields ?? []) as BuilderCustomField[]));
  };

  const deleteTemplate = (templateId: string) => {
    const nextTemplates = templates.filter((template) => template.id !== templateId);
    setTemplates(nextTemplates);
    writeTemplates(nextTemplates);
    if (selectedTemplateId === templateId) {
      setSelectedTemplateId('');
      setTemplateName('');
    }
  };

  return (
    <PageShell title="Creador de reportes de cuenta" subtitle="Construye reportes de tiempo y cuenta dentro del sistema, sin tocar código.">
      <Box
        sx={{
          position: 'relative',
          overflow: 'hidden',
          pb: 7,
          '&::before': {
            content: '""',
            position: 'absolute',
            inset: 0,
            background:
              'radial-gradient(circle at top left, rgba(14,165,233,0.14), transparent 30%), radial-gradient(circle at top right, rgba(15,23,42,0.08), transparent 28%), linear-gradient(180deg, rgba(248,250,252,0.96), rgba(255,255,255,1))',
            pointerEvents: 'none',
            zIndex: -1,
          },
        }}
      >
        <Stack spacing={3}>
          <Paper variant="outlined" sx={{ ...builderCardSx, p: { xs: 2.5, md: 3 } }}>
            <Stack spacing={2.5}>
              <Stack
                direction={{ xs: 'column', md: 'row' }}
                justifyContent="space-between"
                alignItems={{ xs: 'flex-start', md: 'center' }}
                spacing={2}
              >
                <Stack spacing={1} sx={{ maxWidth: 760 }}>
                  <Chip
                    icon={<AutoAwesomeIcon />}
                    label="Nuevo reporte"
                    color="primary"
                    variant="outlined"
                    sx={{ alignSelf: 'flex-start', fontWeight: 800 }}
                  />
                  <Typography variant="h4" fontWeight={900}>
                    Crea reportes desde cero, no desde una plantilla obligatoria.
                  </Typography>
                  <Typography color="text.secondary" sx={{ maxWidth: 680 }}>
                    Define los campos que necesites, agrega bloques de trabajo, ajusta descuentos y descarga el PDF sin salir del sistema.
                  </Typography>
                </Stack>
                <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                  <Button startIcon={<ContentCopyIcon />} variant="outlined" onClick={loadExample}>
                    Cargar ejemplo
                  </Button>
                  <Button startIcon={<DeleteOutlineIcon />} variant="text" onClick={startBlank}>
                    Empezar en blanco
                  </Button>
                  <Button
                    startIcon={<DownloadIcon />}
                    variant="contained"
                    component="a"
                    href={pdfUrl || undefined}
                    download="reporte-cuenta.pdf"
                    disabled={!pdfUrl}
                  >
                    Descargar PDF
                  </Button>
                </Stack>
              </Stack>

              <Grid container spacing={2}>
                <Grid item xs={12} sm={6} md={3}>
                  <StatCard
                    icon={<EventNoteIcon />}
                    label="Bloques"
                    value={String(report.workBlocks.length)}
                    caption="Agrega tantos como necesites."
                  />
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <StatCard
                    icon={<ReceiptLongIcon />}
                    label="Campos personalizados"
                    value={String(report.customFields?.length ?? 0)}
                    caption="Nombre, conceptos, notas y lo que haga falta."
                  />
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <StatCard
                    icon={<AccessTimeIcon />}
                    label="Horas netas"
                    value={`${report.totalBillableHours}h`}
                    caption={`Descuento total: ${report.totalDiscountHours}h.`}
                  />
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <StatCard
                    icon={<PhotoLibraryIcon />}
                    label="Importe neto"
                    value={formatCurrency(report.totalBillableCents)}
                    caption={`Tarifa base ${formatCurrency(report.source.hourlyRateCents)}/h.`}
                  />
                </Grid>
              </Grid>

              <Paper
                variant="outlined"
                sx={{
                  p: { xs: 2, md: 2.5 },
                  borderRadius: 3,
                  bgcolor: 'rgba(255,255,255,0.72)',
                  borderColor: 'rgba(15,23,42,0.10)',
                }}
              >
                <Stack spacing={2}>
                  <SectionTitle
                    title="Plantillas guardadas"
                    subtitle="Guarda el reporte actual como base para futuros clientes o reutiliza una configuración previa."
                  />
                  <Grid container spacing={2}>
                    <Grid item xs={12} md={7}>
                      <TextField
                        label="Nombre de la plantilla"
                        value={templateName}
                        onChange={(e) => setTemplateName(e.target.value)}
                        placeholder="Reporte base para David"
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} md={5}>
                      <Stack direction="row" spacing={1} sx={{ height: '100%' }} alignItems="stretch">
                        <Button variant="contained" onClick={saveTemplate} fullWidth>
                          Guardar plantilla
                        </Button>
                        <Button
                          variant="outlined"
                          onClick={() => {
                            if (!selectedTemplateId) return;
                            loadTemplate(selectedTemplateId);
                          }}
                          disabled={!selectedTemplateId}
                          fullWidth
                        >
                          Reaplicar
                        </Button>
                      </Stack>
                    </Grid>
                  </Grid>

                  {templates.length > 0 ? (
                    <Stack spacing={1.25}>
                      {templates.map((template) => (
                        <Paper
                          key={template.id}
                          variant="outlined"
                          sx={{
                            p: 1.5,
                            borderRadius: 2.5,
                            bgcolor: template.id === selectedTemplateId ? 'rgba(59,130,246,0.08)' : 'white',
                            borderColor: template.id === selectedTemplateId ? 'primary.main' : 'rgba(15,23,42,0.10)',
                          }}
                        >
                          <Stack
                            direction={{ xs: 'column', md: 'row' }}
                            alignItems={{ xs: 'flex-start', md: 'center' }}
                            justifyContent="space-between"
                            spacing={1.5}
                          >
                            <Box>
                              <Typography fontWeight={800}>{template.name}</Typography>
                              <Typography variant="body2" color="text.secondary">
                                Guardada {formatSavedAt(template.savedAt)} · {template.source.workBlocks.length} bloques ·{' '}
                                {template.source.customFields?.length ?? 0} campos
                              </Typography>
                            </Box>
                            <Stack direction="row" spacing={1} flexWrap="wrap">
                              <Button size="small" variant="outlined" onClick={() => loadTemplate(template.id)}>
                                Cargar
                              </Button>
                              <Button size="small" color="inherit" variant="text" onClick={() => deleteTemplate(template.id)}>
                                Eliminar
                              </Button>
                            </Stack>
                          </Stack>
                        </Paper>
                      ))}
                    </Stack>
                  ) : (
                    <Alert severity="info" sx={{ bgcolor: 'rgba(59,130,246,0.08)' }}>
                      Aún no has guardado plantillas. Crea una base y presióna guardar para reutilizarla después.
                    </Alert>
                  )}
                </Stack>
              </Paper>
            </Stack>
          </Paper>

          <Grid container spacing={3}>
            <Grid item xs={12} lg={7}>
              <Stack spacing={3}>
                <Card variant="outlined" sx={builderCardSx}>
                  <CardContent>
                    <Stack spacing={2.5}>
                      <SectionTitle
                        title="Datos del reporte"
                        subtitle="Llena los campos principales. Los vacíos se muestran como una guía visual, no como un requisito para arrancar."
                      />
                      <Grid container spacing={2}>
                        <Grid item xs={12} md={6}>
                          <TextField label="Nombre" value={personName} onChange={(e) => setPersonName(e.target.value)} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={6}>
                          <TextField label="Título del reporte" value={reportTitle} onChange={(e) => setReportTitle(e.target.value)} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={6}>
                          <TextField label="Periodo" value={periodLabel} onChange={(e) => setPeriodLabel(e.target.value)} fullWidth helperText="Ej: 9 y 10 de julio de 2026" />
                        </Grid>
                        <Grid item xs={12} md={3}>
                          <TextField label="Tarifa USD/h" value={hourlyRate} onChange={(e) => setHourlyRate(e.target.value)} fullWidth />
                        </Grid>
                        <Grid item xs={12} md={3}>
                          <TextField
                            label="Corte"
                            type="date"
                            value={asOfDate}
                            onChange={(e) => setAsOfDate(e.target.value as typeof asOfDate)}
                            InputLabelProps={{ shrink: true }}
                            fullWidth
                          />
                        </Grid>
                        <Grid item xs={12}>
                          <TextField label="Notas internas" value={notes} onChange={(e) => setNotes(e.target.value)} fullWidth multiline minRows={3} />
                        </Grid>
                      </Grid>
                    </Stack>
                  </CardContent>
                </Card>

                <Card variant="outlined" sx={builderCardSx}>
                  <CardContent>
                    <Stack spacing={2}>
                      <SectionTitle
                        title="Campos personalizados"
                        subtitle="Añade secciones o etiquetas que no estén en el formulario base."
                        action={
                          <Button startIcon={<AddCircleOutlineIcon />} onClick={addField} variant="outlined">
                            Agregar campo
                          </Button>
                        }
                      />

                      {customFields.length === 0 ? (
                        <Paper
                          variant="outlined"
                          sx={{
                            p: 3,
                            borderRadius: 3,
                            borderStyle: 'dashed',
                            bgcolor: 'rgba(248,250,252,0.9)',
                          }}
                        >
                          <Stack spacing={1}>
                            <Typography fontWeight={800}>No hay campos personalizados todavía.</Typography>
                            <Typography color="text.secondary">
                              Usa este espacio para crear etiquetas como propósito, ubicación, cliente secundario o cualquier dato que quieras ver en el PDF.
                            </Typography>
                            <Box>
                              <Button startIcon={<AddCircleOutlineIcon />} variant="contained" onClick={addField}>
                                Crear primer campo
                              </Button>
                            </Box>
                          </Stack>
                        </Paper>
                      ) : (
                        <Stack spacing={1.5}>
                          {customFields.map((field, index) => (
                            <Paper key={field.id} variant="outlined" sx={{ p: 2, borderRadius: 3 }}>
                              <Stack spacing={2}>
                                <Stack direction="row" alignItems="center" justifyContent="space-between">
                                  <Stack direction="row" spacing={1} alignItems="center">
                                    <DragIndicatorIcon fontSize="small" color="disabled" />
                                    <Typography fontWeight={800}>Campo {index + 1}</Typography>
                                  </Stack>
                                  <Stack direction="row" spacing={0.5}>
                                    <IconButton
                                      onClick={() => moveField(field.id, -1)}
                                      aria-label="Mover campo arriba"
                                      disabled={index === 0}
                                    >
                                      <KeyboardArrowUpIcon />
                                    </IconButton>
                                    <IconButton
                                      onClick={() => moveField(field.id, 1)}
                                      aria-label="Mover campo abajo"
                                      disabled={index === customFields.length - 1}
                                    >
                                      <KeyboardArrowDownIcon />
                                    </IconButton>
                                    <IconButton onClick={() => removeField(field.id)} aria-label="Eliminar campo">
                                      <DeleteOutlineIcon />
                                    </IconButton>
                                  </Stack>
                                </Stack>
                                <Grid container spacing={2}>
                                  <Grid item xs={12} md={5}>
                                    <TextField
                                      label="Etiqueta"
                                      value={field.label}
                                      onChange={(e) => updateField(field.id, { label: e.target.value })}
                                      fullWidth
                                    />
                                  </Grid>
                                  <Grid item xs={12} md={7}>
                                    <TextField
                                      label="Valor"
                                      value={field.value}
                                      onChange={(e) => updateField(field.id, { value: e.target.value })}
                                      fullWidth
                                    />
                                  </Grid>
                                  <Grid item xs={12} md={4}>
                                    <TextField
                                      select
                                      label="Tipo"
                                      value={field.type}
                                      onChange={(e) =>
                                        updateField(field.id, { type: e.target.value as BuilderCustomField['type'] })
                                      }
                                      fullWidth
                                      helperText={fieldTypeOptions.find((option) => option.value === field.type)?.helper}
                                    >
                                      {fieldTypeOptions.map((option) => (
                                        <MenuItem key={option.value} value={option.value}>
                                          {option.label}
                                        </MenuItem>
                                      ))}
                                    </TextField>
                                  </Grid>
                                </Grid>
                              </Stack>
                            </Paper>
                          ))}
                        </Stack>
                      )}
                    </Stack>
                  </CardContent>
                </Card>

                <Card variant="outlined" sx={builderCardSx}>
                  <CardContent>
                    <Stack spacing={2}>
                      <SectionTitle
                        title="Bloques de trabajo"
                        subtitle="Cada bloque puede tener su propia foto de inicio y fin, además de un descuento explícito."
                        action={
                          <Button startIcon={<AddCircleOutlineIcon />} onClick={addBlock} variant="outlined">
                            Agregar bloque
                          </Button>
                        }
                      />

                      {blocks.length === 0 ? (
                        <Paper
                          variant="outlined"
                          sx={{
                            p: 3,
                            borderRadius: 3,
                            borderStyle: 'dashed',
                            bgcolor: 'rgba(248,250,252,0.9)',
                          }}
                        >
                          <Stack spacing={1}>
                            <Typography fontWeight={800}>Todavía no has agregado bloques.</Typography>
                            <Typography color="text.secondary">
                              Empieza con un bloque de trabajo y el sistema calculará la duración, las horas facturables y el descuento.
                            </Typography>
                            <Box>
                              <Button startIcon={<AddCircleOutlineIcon />} variant="contained" onClick={addBlock}>
                                Crear primer bloque
                              </Button>
                            </Box>
                          </Stack>
                        </Paper>
                      ) : (
                        <Stack spacing={2}>
                          {blocks.map((block, index) => (
                            <Paper key={block.id} variant="outlined" sx={{ p: 2.25, borderRadius: 3 }}>
                              <Stack spacing={2}>
                                <Stack direction="row" justifyContent="space-between" alignItems="center">
                                  <Stack direction="row" spacing={1} alignItems="center">
                                    <Chip label={`Bloque ${index + 1}`} color="primary" size="small" />
                                    <Typography variant="body2" color="text.secondary">
                                      {formatDateLabel(block.date)}
                                    </Typography>
                                  </Stack>
                                  <Stack direction="row" spacing={0.5}>
                                    <IconButton
                                      onClick={() => moveBlock(block.id, -1)}
                                      aria-label="Mover bloque arriba"
                                      disabled={index === 0}
                                    >
                                      <KeyboardArrowUpIcon />
                                    </IconButton>
                                    <IconButton
                                      onClick={() => moveBlock(block.id, 1)}
                                      aria-label="Mover bloque abajo"
                                      disabled={index === blocks.length - 1}
                                    >
                                      <KeyboardArrowDownIcon />
                                    </IconButton>
                                    <IconButton onClick={() => removeBlock(block.id)} aria-label="Eliminar bloque">
                                      <DeleteOutlineIcon />
                                    </IconButton>
                                  </Stack>
                                </Stack>

                                <Grid container spacing={2}>
                                  <Grid item xs={12} sm={6} md={3}>
                                    <TextField
                                      label="Fecha"
                                      type="date"
                                      value={block.date}
                                      onChange={(e) =>
                                        updateBlock(block.id, {
                                          date: e.target.value as BuilderBlock['date'],
                                        })
                                      }
                                      InputLabelProps={{ shrink: true }}
                                      fullWidth
                                    />
                                  </Grid>
                                  <Grid item xs={12} sm={6} md={3}>
                                    <TextField
                                      label="Inicio"
                                      value={block.startTime}
                                      onChange={(e) => updateBlock(block.id, { startTime: e.target.value })}
                                      fullWidth
                                      helperText="HH:MM:SS"
                                    />
                                  </Grid>
                                  <Grid item xs={12} sm={6} md={3}>
                                    <TextField
                                      label="Fin"
                                      value={block.endTime}
                                      onChange={(e) => updateBlock(block.id, { endTime: e.target.value })}
                                      fullWidth
                                      helperText="HH:MM:SS"
                                    />
                                  </Grid>
                                  <Grid item xs={12} sm={6} md={3}>
                                    <TextField
                                      label="Descuento horas"
                                      type="number"
                                      value={String(block.discountHours)}
                                      onChange={(e) =>
                                        updateBlock(block.id, {
                                          discountHours: Number.parseInt(e.target.value || '0', 10) || 0,
                                        })
                                      }
                                      fullWidth
                                    />
                                  </Grid>
                                  <Grid item xs={12} md={4}>
                                    <TextField
                                      label="Foto de inicio"
                                      value={block.startPhotoLabel}
                                      onChange={(e) => updateBlock(block.id, { startPhotoLabel: e.target.value })}
                                      fullWidth
                                    />
                                  </Grid>
                                  <Grid item xs={12} md={4}>
                                    <TextField
                                      label="Foto de fin"
                                      value={block.endPhotoLabel}
                                      onChange={(e) => updateBlock(block.id, { endPhotoLabel: e.target.value })}
                                      fullWidth
                                    />
                                  </Grid>
                                  <Grid item xs={12} md={4}>
                                    <TextField
                                      label="Razón del descuento"
                                      value={block.discountReason}
                                      onChange={(e) => updateBlock(block.id, { discountReason: e.target.value })}
                                      fullWidth
                                    />
                                  </Grid>
                                  <Grid item xs={12}>
                                    <TextField
                                      label="Descripción"
                                      value={block.description}
                                      onChange={(e) => updateBlock(block.id, { description: e.target.value })}
                                      fullWidth
                                      multiline
                                      minRows={2}
                                    />
                                  </Grid>
                                </Grid>
                              </Stack>
                            </Paper>
                          ))}
                        </Stack>
                      )}
                    </Stack>
                  </CardContent>
                </Card>
              </Stack>
            </Grid>

            <Grid item xs={12} lg={5}>
              <Box
                sx={{
                  position: { lg: 'sticky' },
                  top: { lg: 24 },
                }}
              >
                <Card variant="outlined" sx={{ ...builderCardSx, bgcolor: 'rgba(15,23,42,0.96)', color: 'white' }}>
                  <CardContent>
                    <Stack spacing={2.5}>
                      <Box>
                        <Chip label="Vista previa en vivo" color="primary" sx={{ mb: 1, fontWeight: 800 }} />
                        <Typography variant="h5" fontWeight={900}>
                          {report.source.personName}
                        </Typography>
                        <Typography color="rgba(255,255,255,0.72)">
                          {report.source.reportTitle} · {report.source.periodLabel}
                        </Typography>
                      </Box>

                      <Grid container spacing={1.5}>
                        <Grid item xs={6}>
                          <StatCard
                            icon={<AccessTimeIcon />}
                            label="Horas netas"
                            value={`${report.totalBillableHours}h`}
                            caption="Facturables después del descuento."
                          />
                        </Grid>
                        <Grid item xs={6}>
                          <StatCard
                            icon={<PhotoLibraryIcon />}
                            label="Descuento"
                            value={`${report.totalDiscountHours}h`}
                            caption={formatCurrency(report.totalDiscountCents)}
                          />
                        </Grid>
                        <Grid item xs={12}>
                          <StatCard
                            icon={<ReceiptLongIcon />}
                            label="Importe neto"
                            value={formatCurrency(report.totalBillableCents)}
                            caption={`Tarifa base ${formatCurrency(report.source.hourlyRateCents)}/h`}
                          />
                        </Grid>
                      </Grid>

                      <Divider sx={{ borderColor: 'rgba(255,255,255,0.12)' }} />

                      <Alert severity="info" sx={{ bgcolor: 'rgba(56,189,248,0.12)', color: 'white' }}>
                        El documento saldrá con los bloques, los campos personalizados y el resumen de descuentos.
                      </Alert>

                      <Stack spacing={1}>
                        <Typography variant="subtitle2" sx={{ textTransform: 'uppercase', letterSpacing: 0.8, color: 'rgba(255,255,255,0.62)' }}>
                          Campos visibles
                        </Typography>
                        {report.customFields?.length ? (
                          <Stack spacing={1}>
                            {report.customFields.map((field) => (
                              <Paper
                                key={field.id}
                                variant="outlined"
                                sx={{ p: 1.5, borderRadius: 2, bgcolor: 'rgba(255,255,255,0.06)', borderColor: 'rgba(255,255,255,0.10)' }}
                              >
                                <Typography variant="caption" color="rgba(255,255,255,0.66)">
                                  {field.label} · {fieldTypeLabels[field.type]}
                                </Typography>
                                <Typography fontWeight={700}>{field.value}</Typography>
                              </Paper>
                            ))}
                          </Stack>
                        ) : (
                          <Typography color="rgba(255,255,255,0.72)">
                            No has añadido campos personalizados.
                          </Typography>
                        )}
                      </Stack>

                      <Stack spacing={1}>
                        <Typography variant="subtitle2" sx={{ textTransform: 'uppercase', letterSpacing: 0.8, color: 'rgba(255,255,255,0.62)' }}>
                          Bloques
                        </Typography>
                        {report.workBlocks.length ? (
                          <Stack spacing={1}>
                            {report.workBlocks.map((block, index) => (
                              <Paper
                                key={block.id}
                                variant="outlined"
                                sx={{ p: 1.5, borderRadius: 2, bgcolor: 'rgba(255,255,255,0.06)', borderColor: 'rgba(255,255,255,0.10)' }}
                              >
                                <Stack spacing={0.5}>
                                  <Typography fontWeight={800}>
                                    Bloque {index + 1} · {formatDateLabel(block.date)}
                                  </Typography>
                                  <Typography variant="body2" color="rgba(255,255,255,0.72)">
                                    {block.startTime} - {block.endTime} · {block.billableHours}h base · -{block.discountHours}h · {block.netBillableHours}h netas
                                  </Typography>
                                  <Typography variant="body2" color="rgba(255,255,255,0.72)">
                                    {block.billableLabel}
                                  </Typography>
                                </Stack>
                              </Paper>
                            ))}
                          </Stack>
                        ) : (
                          <Typography color="rgba(255,255,255,0.72)">
                            Aún no agregaste bloques.
                          </Typography>
                        )}
                      </Stack>
                    </Stack>
                  </CardContent>
                </Card>
              </Box>
            </Grid>
          </Grid>
        </Stack>
      </Box>
    </PageShell>
  );
}
