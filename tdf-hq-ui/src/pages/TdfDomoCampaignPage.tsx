import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import {
  Alert,
  Box,
  Button,
  Checkbox,
  Chip,
  Divider,
  Grid,
  IconButton,
  InputAdornment,
  LinearProgress,
  MenuItem,
  Paper,
  Stack,
  Tab,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Tabs,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import CampaignIcon from '@mui/icons-material/Campaign';
import ChecklistRtlIcon from '@mui/icons-material/ChecklistRtl';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import DownloadIcon from '@mui/icons-material/Download';
import GroupsIcon from '@mui/icons-material/Groups';
import InsightsIcon from '@mui/icons-material/Insights';
import LinkIcon from '@mui/icons-material/Link';
import MovieCreationIcon from '@mui/icons-material/MovieCreation';
import PaidIcon from '@mui/icons-material/Paid';
import PublicIcon from '@mui/icons-material/Public';
import ScoreboardIcon from '@mui/icons-material/Scoreboard';
import SendIcon from '@mui/icons-material/Send';
import TaskAltIcon from '@mui/icons-material/TaskAlt';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';

import PageShell from '../components/PageShell';
import { PUBLIC_BASE } from '../config/appConfig';
import { toLocalDateInputValue } from '../utils/dateOnly';
import {
  BAND_APPLICATION_STAGE_LABELS,
  BAND_APPLICATION_STAGE_OPTIONS,
  CAMPAIGN_PHASES,
  CONTENT_CALENDAR,
  COPY_BANK,
  CREATIVE_ASSET_STATUS_OPTIONS,
  CREATIVE_RULES,
  DEFAULT_CAMPAIGN_METRICS,
  EMPTY_BAND_SCORE,
  FORM_QUESTIONS,
  SELECTION_CRITERIA,
  VIDEO_ASSETS,
  VOICEOVER_SCRIPTS,
  WHATSAPP_TEMPLATES,
  buildBudgetRows,
  calculateBandScore,
  calculateCampaignSummary,
  getAssetLabel,
  getCampaignDayFromDates,
  getContentCalendarDate,
  getCurrentPhaseId,
  type BandApplicationStage,
  type BandScoreBreakdown,
  type CampaignMetricState,
  type CampaignPhaseId,
  type CreativeAssetStatus,
  type TrackedBand,
} from '../features/campaigns/tdfDomoCampaign';

interface AssetTrackingState {
  status: CreativeAssetStatus;
  notes: string;
}

interface CampaignTrackerState {
  startDate: string;
  totalBudgetUsd: number;
  metrics: CampaignMetricState;
  completedCalendarDays: number[];
  assetStatuses: Record<string, AssetTrackingState>;
  bands: TrackedBand[];
  operatorNotes: string;
}

interface NewBandDraft {
  name: string;
  city: string;
  genre: string;
  source: string;
  contact: string;
}

type TrackerTab = 'control' | 'calendar' | 'assets' | 'bands' | 'copies';

const STORAGE_KEY = 'tdf-domo-campaign-tracker-v1';
const HERO_IMAGE_URL = `${PUBLIC_BASE}/assets/tdf-ui/domo-pululahua-terrace-evening.jpg`;
const TARGET_SELECTED_BANDS = 10;

const PHASE_LABELS: Record<CampaignPhaseId, string> = {
  teaser: 'Teaser',
  launch: 'Lanzamiento',
  desire: 'Deseo',
  closing: 'Cierre',
};

const ASSET_STATUS_LABELS: Record<CreativeAssetStatus, string> = {
  pendiente: 'Pendiente',
  guion: 'Guion',
  edicion: 'En edición',
  aprobado: 'Aprobado',
  publicado: 'Publicado',
};

const STAGE_COLOR: Record<BandApplicationStage, 'default' | 'info' | 'warning' | 'success' | 'primary' | 'error'> = {
  nuevo: 'default',
  contactado: 'info',
  postulacion: 'warning',
  calificado: 'primary',
  preseleccion: 'success',
  seleccionado: 'success',
  descartado: 'error',
};

const metricFields: readonly {
  key: keyof CampaignMetricState;
  label: string;
  helper: string;
  adornment?: string;
  integer?: boolean;
}[] = [
  { key: 'reach', label: 'Alcance', helper: 'Personas alcanzadas', integer: true },
  { key: 'videoViews', label: 'Reproducciones', helper: 'Views totales', integer: true },
  { key: 'video50Views', label: 'Views 50%+', helper: 'Retención útil', integer: true },
  { key: 'saves', label: 'Guardados', helper: 'Señal de valor', integer: true },
  { key: 'shares', label: 'Compartidos', helper: 'Distribución orgánica', integer: true },
  { key: 'profileVisits', label: 'Visitas perfil', helper: 'Interés intermedio', integer: true },
  { key: 'formClicks', label: 'Clics formulario', helper: 'Intención', integer: true },
  { key: 'whatsappMessages', label: 'WhatsApp', helper: 'Mensajes recibidos', integer: true },
  { key: 'applications', label: 'Postulaciones', helper: 'Formularios completos', integer: true },
  { key: 'qualifiedApplications', label: 'Calificadas', helper: 'Pasan filtro mínimo', integer: true },
  { key: 'selectedBands', label: 'Seleccionadas', helper: 'Meta: 10 bandas', integer: true },
  { key: 'adSpendUsd', label: 'Pauta gastada', helper: 'USD invertidos', adornment: '$' },
];

const newBandInitialState: NewBandDraft = {
  name: '',
  city: '',
  genre: '',
  source: 'Formulario',
  contact: '',
};

const makeAssetStatuses = (): Record<string, AssetTrackingState> =>
  Object.fromEntries(
    VIDEO_ASSETS.map((asset) => [
      String(asset.id),
      { status: 'pendiente' as CreativeAssetStatus, notes: '' },
    ]),
  );

const makeDefaultTrackerState = (): CampaignTrackerState => ({
  startDate: toLocalDateInputValue(),
  totalBudgetUsd: 0,
  metrics: DEFAULT_CAMPAIGN_METRICS,
  completedCalendarDays: [],
  assetStatuses: makeAssetStatuses(),
  bands: [],
  operatorNotes: '',
});

const formatMoney = (value: number | null) =>
  value == null
    ? '—'
    : new Intl.NumberFormat('es-EC', {
      style: 'currency',
      currency: 'USD',
      maximumFractionDigits: 2,
      minimumFractionDigits: 2,
    }).format(value);

const formatPercent = (value: number | null) =>
  value == null ? '—' : `${(value * 100).toFixed(1)}%`;

const formatDateLabel = (dateInput: string) => {
  const parsed = new Date(`${dateInput}T12:00:00`);
  if (Number.isNaN(parsed.getTime())) return '';
  return parsed.toLocaleDateString('es-EC', { day: '2-digit', month: 'short' });
};

const coerceNumber = (value: unknown, fallback = 0) => {
  const parsed = typeof value === 'number' ? value : Number(value);
  return Number.isFinite(parsed) && parsed >= 0 ? parsed : fallback;
};

const clampScoreInput = (value: string) => {
  const parsed = Number(value);
  if (!Number.isFinite(parsed)) return 0;
  return Math.min(10, Math.max(0, parsed));
};

const isCreativeAssetStatus = (value: string): value is CreativeAssetStatus =>
  CREATIVE_ASSET_STATUS_OPTIONS.includes(value as CreativeAssetStatus);

const isBandApplicationStage = (value: string): value is BandApplicationStage =>
  BAND_APPLICATION_STAGE_OPTIONS.includes(value as BandApplicationStage);

const makeId = () => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return crypto.randomUUID();
  }
  return `band-${Date.now()}-${Math.round(Math.random() * 100_000)}`;
};

const normalizeMetrics = (value: unknown): CampaignMetricState => {
  const candidate = value && typeof value === 'object'
    ? value as Partial<Record<keyof CampaignMetricState, unknown>>
    : {};

  return (Object.entries(DEFAULT_CAMPAIGN_METRICS) as [keyof CampaignMetricState, number][])
    .reduce<CampaignMetricState>(
      (normalized, [key, fallback]) => ({
        ...normalized,
        [key]: coerceNumber(candidate[key], fallback),
      }),
      { ...DEFAULT_CAMPAIGN_METRICS },
    );
};

const normalizeCompletedDays = (value: unknown): number[] => {
  if (!Array.isArray(value)) return [];
  const days = value
    .map((item) => Number(item))
    .filter((day) => Number.isSafeInteger(day) && day >= 1 && day <= CONTENT_CALENDAR.length);
  return Array.from(new Set(days));
};

const normalizeAssetStatuses = (value: unknown): Record<string, AssetTrackingState> => {
  const defaults = makeAssetStatuses();
  const candidate = value && typeof value === 'object'
    ? value as Record<string, Partial<AssetTrackingState> | undefined>
    : {};

  VIDEO_ASSETS.forEach((asset) => {
    const key = String(asset.id);
    const stored = candidate[key];
    if (!stored) return;
    defaults[key] = {
      status: stored.status && isCreativeAssetStatus(stored.status) ? stored.status : defaults[key]?.status ?? 'pendiente',
      notes: typeof stored.notes === 'string' ? stored.notes : '',
    };
  });

  return defaults;
};

const normalizeBandScores = (value: unknown): BandScoreBreakdown => {
  const candidate = value && typeof value === 'object'
    ? value as Partial<Record<keyof BandScoreBreakdown, unknown>>
    : {};

  return (Object.entries(EMPTY_BAND_SCORE) as [keyof BandScoreBreakdown, number][])
    .reduce<BandScoreBreakdown>(
      (normalized, [key, fallback]) => ({
        ...normalized,
        [key]: coerceNumber(candidate[key], fallback),
      }),
      { ...EMPTY_BAND_SCORE },
    );
};

const normalizeBands = (value: unknown): TrackedBand[] => {
  if (!Array.isArray(value)) return [];
  return value
    .filter((item): item is Partial<TrackedBand> => Boolean(item) && typeof item === 'object')
    .map((item) => {
      const stage = typeof item.stage === 'string' && isBandApplicationStage(item.stage) ? item.stage : 'nuevo';
      return {
        id: typeof item.id === 'string' && item.id.trim() ? item.id : makeId(),
        name: typeof item.name === 'string' ? item.name : '',
        city: typeof item.city === 'string' ? item.city : '',
        genre: typeof item.genre === 'string' ? item.genre : '',
        source: typeof item.source === 'string' ? item.source : '',
        contact: typeof item.contact === 'string' ? item.contact : '',
        stage,
        scores: normalizeBandScores(item.scores),
        notes: typeof item.notes === 'string' ? item.notes : '',
      };
    })
    .filter((band) => band.name.trim().length > 0);
};

const readStoredState = (): CampaignTrackerState => {
  const defaults = makeDefaultTrackerState();
  if (typeof window === 'undefined') return defaults;

  try {
    const raw = window.localStorage.getItem(STORAGE_KEY);
    if (!raw) return defaults;
    const parsed = JSON.parse(raw) as Partial<CampaignTrackerState>;
    return {
      ...defaults,
      startDate: typeof parsed.startDate === 'string' && parsed.startDate.trim()
        ? parsed.startDate
        : defaults.startDate,
      totalBudgetUsd: coerceNumber(parsed.totalBudgetUsd),
      metrics: normalizeMetrics(parsed.metrics),
      completedCalendarDays: normalizeCompletedDays(parsed.completedCalendarDays),
      assetStatuses: normalizeAssetStatuses(parsed.assetStatuses),
      bands: normalizeBands(parsed.bands),
      operatorNotes: typeof parsed.operatorNotes === 'string' ? parsed.operatorNotes : '',
    };
  } catch {
    return defaults;
  }
};

function Section({
  title,
  subtitle,
  icon,
  children,
}: {
  title: string;
  subtitle?: string;
  icon?: ReactNode;
  children: ReactNode;
}) {
  return (
    <Paper variant="outlined" sx={{ p: { xs: 2, md: 2.5 }, borderRadius: 2 }}>
      <Stack spacing={2}>
        <Stack direction="row" spacing={1.5} alignItems="flex-start">
          {icon && (
            <Box sx={{ color: 'primary.main', display: 'flex', pt: 0.25 }}>
              {icon}
            </Box>
          )}
          <Box>
            <Typography variant="h6" fontWeight={850}>{title}</Typography>
            {subtitle && <Typography color="text.secondary">{subtitle}</Typography>}
          </Box>
        </Stack>
        {children}
      </Stack>
    </Paper>
  );
}

function MetricBox({
  icon,
  label,
  value,
  helper,
}: {
  icon: ReactNode;
  label: string;
  value: string;
  helper?: string;
}) {
  return (
    <Box
      sx={{
        height: '100%',
        minHeight: 118,
        p: 2,
        border: 1,
        borderColor: 'divider',
        borderRadius: 2,
        bgcolor: 'background.paper',
      }}
    >
      <Stack spacing={1}>
        <Box sx={{ color: 'primary.main', display: 'flex' }}>{icon}</Box>
        <Typography variant="body2" color="text.secondary">{label}</Typography>
        <Typography variant="h5" fontWeight={900}>{value}</Typography>
        {helper && <Typography variant="caption" color="text.secondary">{helper}</Typography>}
      </Stack>
    </Box>
  );
}

function TabPanel({
  active,
  value,
  children,
}: {
  active: TrackerTab;
  value: TrackerTab;
  children: ReactNode;
}) {
  if (active !== value) return null;
  return <Box sx={{ pt: 3 }}>{children}</Box>;
}

function CopyCard({
  item,
  onCopy,
}: {
  item: { id: string; title: string; intent: string; body: string };
  onCopy: (body: string, label: string) => void;
}) {
  return (
    <Paper variant="outlined" sx={{ p: 2, borderRadius: 2, height: '100%' }}>
      <Stack spacing={1.5} sx={{ height: '100%' }}>
        <Stack direction="row" justifyContent="space-between" spacing={1} alignItems="flex-start">
          <Box>
            <Typography fontWeight={850}>{item.title}</Typography>
            <Typography variant="body2" color="text.secondary">{item.intent}</Typography>
          </Box>
          <Tooltip title="Copiar">
            <IconButton onClick={() => onCopy(item.body, item.title)} aria-label={`Copiar ${item.title}`}>
              <ContentCopyIcon fontSize="small" />
            </IconButton>
          </Tooltip>
        </Stack>
        <Typography
          component="pre"
          variant="body2"
          sx={{
            whiteSpace: 'pre-wrap',
            fontFamily: 'inherit',
            m: 0,
            color: 'text.secondary',
            flex: 1,
          }}
        >
          {item.body}
        </Typography>
      </Stack>
    </Paper>
  );
}

export default function TdfDomoCampaignPage() {
  const [tracker, setTracker] = useState<CampaignTrackerState>(() => readStoredState());
  const [tab, setTab] = useState<TrackerTab>('control');
  const [notice, setNotice] = useState<{ severity: 'success' | 'error' | 'info'; message: string } | null>(null);
  const [newBand, setNewBand] = useState<NewBandDraft>(newBandInitialState);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(tracker));
  }, [tracker]);

  const todayInput = toLocalDateInputValue();
  const currentDay = useMemo(
    () => getCampaignDayFromDates(tracker.startDate, todayInput),
    [todayInput, tracker.startDate],
  );
  const currentPhaseId = getCurrentPhaseId(currentDay);
  const currentPhase = CAMPAIGN_PHASES.find((phase) => phase.id === currentPhaseId);
  const summary = useMemo(() => calculateCampaignSummary(tracker.metrics), [tracker.metrics]);
  const budgetRows = useMemo(() => buildBudgetRows(tracker.totalBudgetUsd), [tracker.totalBudgetUsd]);
  const completedCalendarCount = tracker.completedCalendarDays.length;
  const completedAssetCount = VIDEO_ASSETS.filter(
    (asset) => tracker.assetStatuses[String(asset.id)]?.status === 'publicado',
  ).length;
  const trackedSelectedBands = tracker.bands.filter((band) => band.stage === 'seleccionado').length;
  const sortedBands = useMemo(
    () =>
      [...tracker.bands].sort((a, b) => calculateBandScore(b.scores) - calculateBandScore(a.scores)),
    [tracker.bands],
  );

  const setMetric = (key: keyof CampaignMetricState, value: string, integer?: boolean) => {
    const parsed = coerceNumber(value);
    setTracker((prev) => ({
      ...prev,
      metrics: {
        ...prev.metrics,
        [key]: integer ? Math.round(parsed) : parsed,
      },
    }));
  };

  const updateAsset = (assetId: number, patch: Partial<AssetTrackingState>) => {
    setTracker((prev) => ({
      ...prev,
      assetStatuses: {
        ...prev.assetStatuses,
        [String(assetId)]: {
          status: prev.assetStatuses[String(assetId)]?.status ?? 'pendiente',
          notes: prev.assetStatuses[String(assetId)]?.notes ?? '',
          ...patch,
        },
      },
    }));
  };

  const toggleCalendarDay = (day: number, checked: boolean) => {
    setTracker((prev) => ({
      ...prev,
      completedCalendarDays: checked
        ? Array.from(new Set([...prev.completedCalendarDays, day])).sort((a, b) => a - b)
        : prev.completedCalendarDays.filter((item) => item !== day),
    }));
  };

  const addBand = () => {
    const name = newBand.name.trim();
    if (!name) {
      setNotice({ severity: 'error', message: 'Ingresa el nombre de la banda antes de agregarla.' });
      return;
    }

    setTracker((prev) => ({
      ...prev,
      bands: [
        ...prev.bands,
        {
          id: makeId(),
          name,
          city: newBand.city.trim(),
          genre: newBand.genre.trim(),
          source: newBand.source.trim() || 'Formulario',
          contact: newBand.contact.trim(),
          stage: 'nuevo',
          scores: { ...EMPTY_BAND_SCORE },
          notes: '',
        },
      ],
    }));
    setNewBand(newBandInitialState);
    setNotice({ severity: 'success', message: 'Banda agregada al tracker.' });
  };

  const updateBand = (bandId: string, patch: Partial<TrackedBand>) => {
    setTracker((prev) => ({
      ...prev,
      bands: prev.bands.map((band) => (band.id === bandId ? { ...band, ...patch } : band)),
    }));
  };

  const updateBandScore = (bandId: string, criterion: keyof BandScoreBreakdown, value: string) => {
    setTracker((prev) => ({
      ...prev,
      bands: prev.bands.map((band) =>
        band.id === bandId
          ? {
            ...band,
            scores: {
              ...band.scores,
              [criterion]: clampScoreInput(value),
            },
          }
          : band,
      ),
    }));
  };

  const deleteBand = (bandId: string) => {
    setTracker((prev) => ({
      ...prev,
      bands: prev.bands.filter((band) => band.id !== bandId),
    }));
  };

  const copyText = async (body: string, label: string) => {
    try {
      await navigator.clipboard.writeText(body);
      setNotice({ severity: 'success', message: `${label} copiado.` });
    } catch {
      setNotice({ severity: 'error', message: 'No se pudo copiar al portapapeles.' });
    }
  };

  const downloadJson = () => {
    const blob = new Blob([JSON.stringify(tracker, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'tdf-sessions-domo-campaign-tracker.json';
    link.click();
    URL.revokeObjectURL(url);
  };

  const downloadBandsCsv = () => {
    const header = ['Banda', 'Ciudad', 'Género', 'Fuente', 'Contacto', 'Etapa', 'Score', 'Notas'];
    const rows = sortedBands.map((band) => [
      band.name,
      band.city,
      band.genre,
      band.source,
      band.contact,
      BAND_APPLICATION_STAGE_LABELS[band.stage],
      String(calculateBandScore(band.scores)),
      band.notes,
    ]);
    const csv = [header, ...rows]
      .map((row) => row.map((cell) => `"${cell.replace(/"/g, '""')}"`).join(','))
      .join('\n');
    const blob = new Blob([csv], { type: 'text/csv;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'tdf-sessions-domo-bandas.csv';
    link.click();
    URL.revokeObjectURL(url);
  };

  const copyTextFromClick = (body: string, label: string) => {
    void copyText(body, label);
  };

  const heroActions = (
    <>
      <Button component={RouterLink} to="/live-sessions/registro" startIcon={<PublicIcon />} variant="outlined">
        Formulario público
      </Button>
      <Button component={RouterLink} to="/estudio/live-sessions" startIcon={<LinkIcon />} variant="contained">
        Intake interno
      </Button>
    </>
  );

  return (
    <PageShell
      title="TDF Sessions x Domo Pululahua"
      subtitle="Centro operativo para convocatoria nacional, seguimiento de creativos, métricas, postulaciones y selección."
      actions={heroActions}
    >
      <Stack spacing={3} sx={{ pb: 8 }}>
        {notice && (
          <Alert severity={notice.severity} onClose={() => setNotice(null)}>
            {notice.message}
          </Alert>
        )}

        <Paper
          sx={{
            overflow: 'hidden',
            borderRadius: 2,
            minHeight: { xs: 360, md: 420 },
            position: 'relative',
            color: '#fff',
            backgroundImage: `linear-gradient(90deg, rgba(6,10,20,0.94), rgba(6,10,20,0.72) 48%, rgba(6,10,20,0.34)), url(${HERO_IMAGE_URL})`,
            backgroundSize: 'cover',
            backgroundPosition: 'center',
          }}
        >
          <Stack
            spacing={3}
            sx={{
              p: { xs: 3, md: 5 },
              maxWidth: 780,
              minHeight: { xs: 360, md: 420 },
              justifyContent: 'center',
            }}
          >
            <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
              <Chip label="Convocatoria nacional" sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff' }} />
              <Chip label="Solo 10 bandas" sx={{ bgcolor: 'rgba(244,196,48,0.22)', color: '#ffe8a3' }} />
              <Chip label="Domo Pululahua" sx={{ bgcolor: 'rgba(52,211,153,0.20)', color: '#c7f9df' }} />
            </Stack>
            <Box>
              <Typography variant="h2" sx={{ fontSize: { xs: '2rem', md: '3.6rem' }, fontWeight: 950, lineHeight: 0.98 }}>
                30 minutos. 4 cámaras. Un cráter. Tu banda.
              </Typography>
              <Typography sx={{ mt: 2, color: 'rgba(255,255,255,0.84)', maxWidth: 660, fontSize: { xs: '1rem', md: '1.12rem' } }}>
                No es un videoclip. Es tu banda tocando de verdad en un lugar imposible de ignorar.
              </Typography>
            </Box>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.25}>
              <Button component={RouterLink} to="/live-sessions/registro" variant="contained" color="secondary" startIcon={<SendIcon />}>
                Postula ahora
              </Button>
              <Button component={RouterLink} to="/domo-del-pululahua" variant="outlined" color="inherit" startIcon={<PublicIcon />}>
                Ver Domo
              </Button>
            </Stack>
          </Stack>
        </Paper>

        <Grid container spacing={2}>
          <Grid item xs={12} sm={6} md={3}>
            <MetricBox
              icon={<GroupsIcon />}
              label="Postulaciones"
              value={`${tracker.metrics.applications}`}
              helper={`${tracker.metrics.qualifiedApplications} calificadas`}
            />
          </Grid>
          <Grid item xs={12} sm={6} md={3}>
            <MetricBox
              icon={<ScoreboardIcon />}
              label="Selección"
              value={`${tracker.metrics.selectedBands}/${TARGET_SELECTED_BANDS}`}
              helper={`${summary.remainingSlots} cupos restantes`}
            />
          </Grid>
          <Grid item xs={12} sm={6} md={3}>
            <MetricBox
              icon={<PaidIcon />}
              label="Costo por lead"
              value={formatMoney(summary.costPerApplication)}
              helper={`${formatMoney(summary.costPerQualified)} por calificada`}
            />
          </Grid>
          <Grid item xs={12} sm={6} md={3}>
            <MetricBox
              icon={<CalendarMonthIcon />}
              label="Día de campaña"
              value={currentDay == null ? '—' : String(currentDay)}
              helper={currentPhase ? currentPhase.title : 'Fuera del rango activo'}
            />
          </Grid>
        </Grid>

        <Paper variant="outlined" sx={{ borderRadius: 2 }}>
          <Tabs
            value={tab}
            onChange={(_, value: TrackerTab) => setTab(value)}
            variant="scrollable"
            scrollButtons="auto"
            sx={{ px: 1, borderBottom: 1, borderColor: 'divider' }}
          >
            <Tab value="control" icon={<InsightsIcon />} iconPosition="start" label="Control" />
            <Tab value="calendar" icon={<ChecklistRtlIcon />} iconPosition="start" label="Calendario" />
            <Tab value="assets" icon={<MovieCreationIcon />} iconPosition="start" label="Creativos" />
            <Tab value="bands" icon={<GroupsIcon />} iconPosition="start" label="Bandas" />
            <Tab value="copies" icon={<CampaignIcon />} iconPosition="start" label="Copys" />
          </Tabs>

          <Box sx={{ p: { xs: 2, md: 3 } }}>
            <TabPanel active={tab} value="control">
              <Stack spacing={3}>
                <Section
                  title="Fase activa y configuración"
                  subtitle="La fecha de inicio corresponde al Día 1 del lanzamiento público."
                  icon={<CalendarMonthIcon />}
                >
                  <Grid container spacing={2}>
                    <Grid item xs={12} md={3}>
                      <TextField
                        label="Inicio de convocatoria"
                        type="date"
                        value={tracker.startDate}
                        onChange={(event) => setTracker((prev) => ({ ...prev, startDate: event.target.value }))}
                        fullWidth
                        InputLabelProps={{ shrink: true }}
                      />
                    </Grid>
                    <Grid item xs={12} md={3}>
                      <TextField
                        label="Presupuesto total"
                        type="number"
                        value={tracker.totalBudgetUsd}
                        onChange={(event) => setTracker((prev) => ({ ...prev, totalBudgetUsd: coerceNumber(event.target.value) }))}
                        fullWidth
                        InputProps={{ startAdornment: <InputAdornment position="start">$</InputAdornment> }}
                      />
                    </Grid>
                    <Grid item xs={12} md={6}>
                      <Alert severity={currentPhase ? 'info' : 'warning'}>
                        {currentPhase
                          ? `${currentPhase.title}: ${currentPhase.objective}`
                          : 'Ajusta la fecha de inicio para ubicar la campaña dentro del calendario operativo.'}
                      </Alert>
                    </Grid>
                  </Grid>

                  <Grid container spacing={2}>
                    {CAMPAIGN_PHASES.map((phase) => {
                      const isActive = phase.id === currentPhaseId;
                      return (
                        <Grid item xs={12} md={3} key={phase.id}>
                          <Box
                            sx={{
                              height: '100%',
                              p: 2,
                              border: 1,
                              borderColor: isActive ? 'primary.main' : 'divider',
                              borderRadius: 2,
                              bgcolor: isActive ? 'primary.main' : 'background.paper',
                              color: isActive ? 'primary.contrastText' : 'text.primary',
                            }}
                          >
                            <Stack spacing={1}>
                              <Typography fontWeight={900}>{phase.title}</Typography>
                              <Typography variant="body2" sx={{ opacity: 0.82 }}>{phase.timing}</Typography>
                              <Typography variant="body2">{phase.cta}</Typography>
                            </Stack>
                          </Box>
                        </Grid>
                      );
                    })}
                  </Grid>
                </Section>

                <Section title="Métricas del embudo" subtitle="Actualiza estas cifras con Meta, TikTok, YouTube y formularios." icon={<InsightsIcon />}>
                  <Grid container spacing={2}>
                    {metricFields.map((field) => (
                      <Grid item xs={12} sm={6} md={3} key={field.key}>
                        <TextField
                          label={field.label}
                          type="number"
                          value={tracker.metrics[field.key]}
                          onChange={(event) => setMetric(field.key, event.target.value, field.integer)}
                          helperText={field.helper}
                          fullWidth
                          InputProps={field.adornment ? { startAdornment: <InputAdornment position="start">{field.adornment}</InputAdornment> } : undefined}
                          inputProps={{ min: 0, step: field.integer ? 1 : 0.01 }}
                        />
                      </Grid>
                    ))}
                  </Grid>
                  <Grid container spacing={2}>
                    <Grid item xs={12} md={4}>
                      <MetricBox icon={<SendIcon />} label="Clic a postulación" value={formatPercent(summary.applicationRate)} helper="Postulaciones / clics formulario" />
                    </Grid>
                    <Grid item xs={12} md={4}>
                      <MetricBox icon={<TaskAltIcon />} label="Calificación" value={formatPercent(summary.qualifiedRate)} helper="Calificadas / postulaciones" />
                    </Grid>
                    <Grid item xs={12} md={4}>
                      <MetricBox icon={<ScoreboardIcon />} label="Cupos ocupados" value={formatPercent(summary.selectionProgress)} helper={`${trackedSelectedBands} seleccionadas en tracker`} />
                    </Grid>
                  </Grid>
                </Section>

                <Section title="Distribución de presupuesto" subtitle="Recomendación base: emoción visual, explicación técnica y cierre con urgencia." icon={<PaidIcon />}>
                  <TableContainer>
                    <Table size="small">
                      <TableHead>
                        <TableRow>
                          <TableCell>Campaña</TableCell>
                          <TableCell>Objetivo</TableCell>
                          <TableCell>Creativos</TableCell>
                          <TableCell align="right">%</TableCell>
                          <TableCell align="right">USD</TableCell>
                        </TableRow>
                      </TableHead>
                      <TableBody>
                        {budgetRows.map((row) => (
                          <TableRow key={row.id}>
                            <TableCell sx={{ fontWeight: 800 }}>{row.label}</TableCell>
                            <TableCell>{row.objective}</TableCell>
                            <TableCell>{row.creatives.map(getAssetLabel).join(' · ')}</TableCell>
                            <TableCell align="right">{row.percent}%</TableCell>
                            <TableCell align="right">{formatMoney(row.amountUsd)}</TableCell>
                          </TableRow>
                        ))}
                      </TableBody>
                    </Table>
                  </TableContainer>
                </Section>

                <Section title="Notas de operación" subtitle="Resumen libre para acuerdos, pendientes, links UTM o decisiones de pauta." icon={<CampaignIcon />}>
                  <TextField
                    label="Notas internas"
                    value={tracker.operatorNotes}
                    onChange={(event) => setTracker((prev) => ({ ...prev, operatorNotes: event.target.value }))}
                    multiline
                    minRows={4}
                    fullWidth
                  />
                  <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                    <Button onClick={downloadJson} startIcon={<DownloadIcon />} variant="outlined">
                      Exportar JSON
                    </Button>
                    <Button onClick={() => copyTextFromClick(JSON.stringify(tracker, null, 2), 'Resumen de campaña')} startIcon={<ContentCopyIcon />} variant="outlined">
                      Copiar estado
                    </Button>
                  </Stack>
                </Section>
              </Stack>
            </TabPanel>

            <TabPanel active={tab} value="calendar">
              <Stack spacing={3}>
                <Section
                  title="Calendario de 21 días"
                  subtitle={`${completedCalendarCount}/${CONTENT_CALENDAR.length} publicaciones marcadas como listas o publicadas.`}
                  icon={<ChecklistRtlIcon />}
                >
                  <LinearProgress
                    variant="determinate"
                    value={(completedCalendarCount / CONTENT_CALENDAR.length) * 100}
                    sx={{ height: 8, borderRadius: 999 }}
                  />
                  <Grid container spacing={2}>
                    {CONTENT_CALENDAR.map((item) => {
                      const checked = tracker.completedCalendarDays.includes(item.day);
                      const calendarDate = getContentCalendarDate(item.day, tracker.startDate);
                      return (
                        <Grid item xs={12} md={4} key={item.day}>
                          <Paper
                            variant="outlined"
                            sx={{
                              p: 2,
                              borderRadius: 2,
                              height: '100%',
                              borderColor: checked ? 'success.main' : 'divider',
                            }}
                          >
                            <Stack spacing={1.25}>
                              <Stack direction="row" spacing={1} alignItems="flex-start">
                                <Checkbox
                                  checked={checked}
                                  onChange={(event) => toggleCalendarDay(item.day, event.target.checked)}
                                  inputProps={{ 'aria-label': `Completar día ${item.day}` }}
                                />
                                <Box sx={{ minWidth: 0 }}>
                                  <Typography fontWeight={900}>Día {item.day}: {item.title}</Typography>
                                  <Typography variant="body2" color="text.secondary">
                                    {calendarDate ? formatDateLabel(calendarDate) : 'Fecha pendiente'} · {item.format}
                                  </Typography>
                                </Box>
                              </Stack>
                              <Chip
                                size="small"
                                label={PHASE_LABELS[item.phaseId]}
                                color={item.phaseId === currentPhaseId ? 'primary' : 'default'}
                                sx={{ alignSelf: 'flex-start' }}
                              />
                              <Typography variant="body2">{item.message}</Typography>
                              <Typography variant="caption" color="text.secondary">
                                {item.assetIds.map(getAssetLabel).join(' · ')}
                              </Typography>
                              <Typography variant="body2" fontWeight={800}>{item.cta}</Typography>
                            </Stack>
                          </Paper>
                        </Grid>
                      );
                    })}
                  </Grid>
                </Section>
              </Stack>
            </TabPanel>

            <TabPanel active={tab} value="assets">
              <Stack spacing={3}>
                <Section
                  title="Mapa de videos base"
                  subtitle={`${completedAssetCount}/${VIDEO_ASSETS.length} videos marcados como publicados.`}
                  icon={<MovieCreationIcon />}
                >
                  <LinearProgress
                    variant="determinate"
                    value={(completedAssetCount / VIDEO_ASSETS.length) * 100}
                    sx={{ height: 8, borderRadius: 999 }}
                  />
                  <TableContainer>
                    <Table size="small">
                      <TableHead>
                        <TableRow>
                          <TableCell>Video</TableCell>
                          <TableCell>Rol</TableCell>
                          <TableCell>Uso</TableCell>
                          <TableCell>Fases</TableCell>
                          <TableCell sx={{ minWidth: 160 }}>Estado</TableCell>
                          <TableCell sx={{ minWidth: 220 }}>Notas</TableCell>
                        </TableRow>
                      </TableHead>
                      <TableBody>
                        {VIDEO_ASSETS.map((asset) => {
                          const tracking = tracker.assetStatuses[String(asset.id)] ?? { status: 'pendiente', notes: '' };
                          return (
                            <TableRow key={asset.id}>
                              <TableCell>
                                <Typography fontWeight={850}>Video {asset.id}</Typography>
                                <Typography variant="body2" color="text.secondary">{asset.title}</Typography>
                              </TableCell>
                              <TableCell>{asset.role}</TableCell>
                              <TableCell>{asset.bestUse}</TableCell>
                              <TableCell>
                                <Stack direction="row" spacing={0.5} useFlexGap flexWrap="wrap">
                                  {asset.phaseIds.map((phaseId) => (
                                    <Chip key={phaseId} size="small" label={PHASE_LABELS[phaseId]} />
                                  ))}
                                </Stack>
                              </TableCell>
                              <TableCell>
                                <TextField
                                  select
                                  size="small"
                                  value={tracking.status}
                                  onChange={(event) => {
                                    const status = event.target.value;
                                    updateAsset(asset.id, { status: isCreativeAssetStatus(status) ? status : 'pendiente' });
                                  }}
                                  fullWidth
                                >
                                  {CREATIVE_ASSET_STATUS_OPTIONS.map((status) => (
                                    <MenuItem key={status} value={status}>
                                      {ASSET_STATUS_LABELS[status]}
                                    </MenuItem>
                                  ))}
                                </TextField>
                              </TableCell>
                              <TableCell>
                                <TextField
                                  size="small"
                                  placeholder="Hook, corte, pauta, pendiente"
                                  value={tracking.notes}
                                  onChange={(event) => updateAsset(asset.id, { notes: event.target.value })}
                                  fullWidth
                                />
                              </TableCell>
                            </TableRow>
                          );
                        })}
                      </TableBody>
                    </Table>
                  </TableContainer>
                </Section>

                <Grid container spacing={2}>
                  <Grid item xs={12} md={6}>
                    <Section title="Sí hacer" icon={<TaskAltIcon />}>
                      <Stack spacing={1}>
                        {CREATIVE_RULES.do.map((item) => (
                          <Typography key={item} variant="body2">• {item}</Typography>
                        ))}
                      </Stack>
                    </Section>
                  </Grid>
                  <Grid item xs={12} md={6}>
                    <Section title="Evitar" icon={<ChecklistRtlIcon />}>
                      <Stack spacing={1}>
                        {CREATIVE_RULES.avoid.map((item) => (
                          <Typography key={item} variant="body2">• {item}</Typography>
                        ))}
                      </Stack>
                    </Section>
                  </Grid>
                </Grid>
              </Stack>
            </TabPanel>

            <TabPanel active={tab} value="bands">
              <Stack spacing={3}>
                <Section
                  title="Tracker de postulaciones"
                  subtitle={`${tracker.bands.length} bandas en seguimiento · ${trackedSelectedBands} seleccionadas en el tablero.`}
                  icon={<GroupsIcon />}
                >
                  <Grid container spacing={2}>
                    <Grid item xs={12} md={3}>
                      <TextField
                        label="Banda"
                        value={newBand.name}
                        onChange={(event) => setNewBand((prev) => ({ ...prev, name: event.target.value }))}
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} md={2}>
                      <TextField
                        label="Ciudad"
                        value={newBand.city}
                        onChange={(event) => setNewBand((prev) => ({ ...prev, city: event.target.value }))}
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} md={2}>
                      <TextField
                        label="Género"
                        value={newBand.genre}
                        onChange={(event) => setNewBand((prev) => ({ ...prev, genre: event.target.value }))}
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} md={2}>
                      <TextField
                        label="Fuente"
                        value={newBand.source}
                        onChange={(event) => setNewBand((prev) => ({ ...prev, source: event.target.value }))}
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} md={3}>
                      <TextField
                        label="Contacto / link"
                        value={newBand.contact}
                        onChange={(event) => setNewBand((prev) => ({ ...prev, contact: event.target.value }))}
                        fullWidth
                      />
                    </Grid>
                  </Grid>
                  <Button startIcon={<AddIcon />} variant="contained" onClick={addBand} sx={{ alignSelf: 'flex-start' }}>
                    Agregar banda
                  </Button>
                </Section>

                <Section title="Criterios de selección" subtitle="Score ponderado sobre 100 puntos." icon={<ScoreboardIcon />}>
                  <Grid container spacing={1.5}>
                    {SELECTION_CRITERIA.map((criterion) => (
                      <Grid item xs={12} sm={6} md={4} key={criterion.key}>
                        <Box sx={{ p: 1.5, border: 1, borderColor: 'divider', borderRadius: 2 }}>
                          <Typography fontWeight={850}>{criterion.label}</Typography>
                          <Typography variant="body2" color="text.secondary">{criterion.weight}%</Typography>
                        </Box>
                      </Grid>
                    ))}
                  </Grid>
                </Section>

                {sortedBands.length === 0 ? (
                  <Alert severity="info">
                    Todavía no hay bandas cargadas en el tracker. Agrega postulantes manualmente o usa el formulario público para levantar datos.
                  </Alert>
                ) : (
                  <Stack spacing={2}>
                    {sortedBands.map((band, index) => {
                      const score = calculateBandScore(band.scores);
                      return (
                        <Paper key={band.id} variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                          <Stack spacing={2}>
                            <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.5} justifyContent="space-between">
                              <Box>
                                <Stack direction="row" spacing={1} alignItems="center" useFlexGap flexWrap="wrap">
                                  <Typography variant="h6" fontWeight={900}>#{index + 1} {band.name}</Typography>
                                  <Chip label={`${score}/100`} color={score >= 75 ? 'success' : score >= 55 ? 'warning' : 'default'} />
                                  <Chip label={BAND_APPLICATION_STAGE_LABELS[band.stage]} color={STAGE_COLOR[band.stage]} />
                                </Stack>
                                <Typography color="text.secondary">
                                  {[band.city, band.genre, band.source].filter(Boolean).join(' · ') || 'Sin ciudad/género/fuente'}
                                </Typography>
                              </Box>
                              <Stack direction="row" spacing={1} alignItems="center">
                                <TextField
                                  select
                                  label="Etapa"
                                  size="small"
                                  value={band.stage}
                                  onChange={(event) => {
                                    const stage = event.target.value;
                                    updateBand(band.id, { stage: isBandApplicationStage(stage) ? stage : 'nuevo' });
                                  }}
                                  sx={{ minWidth: 170 }}
                                >
                                  {BAND_APPLICATION_STAGE_OPTIONS.map((stage) => (
                                    <MenuItem key={stage} value={stage}>{BAND_APPLICATION_STAGE_LABELS[stage]}</MenuItem>
                                  ))}
                                </TextField>
                                <Tooltip title="Eliminar banda">
                                  <IconButton onClick={() => deleteBand(band.id)} aria-label={`Eliminar ${band.name}`}>
                                    <DeleteOutlineIcon />
                                  </IconButton>
                                </Tooltip>
                              </Stack>
                            </Stack>

                            <Grid container spacing={1.5}>
                              {SELECTION_CRITERIA.map((criterion) => (
                                <Grid item xs={6} md={2} key={criterion.key}>
                                  <TextField
                                    label={criterion.label}
                                    type="number"
                                    size="small"
                                    value={band.scores[criterion.key]}
                                    onChange={(event) => updateBandScore(band.id, criterion.key, event.target.value)}
                                    inputProps={{ min: 0, max: 10, step: 0.5 }}
                                    fullWidth
                                  />
                                </Grid>
                              ))}
                            </Grid>

                            <Grid container spacing={1.5}>
                              <Grid item xs={12} md={4}>
                                <TextField
                                  label="Contacto / link"
                                  value={band.contact}
                                  onChange={(event) => updateBand(band.id, { contact: event.target.value })}
                                  fullWidth
                                />
                              </Grid>
                              <Grid item xs={12} md={8}>
                                <TextField
                                  label="Notas"
                                  value={band.notes}
                                  onChange={(event) => updateBand(band.id, { notes: event.target.value })}
                                  fullWidth
                                />
                              </Grid>
                            </Grid>
                          </Stack>
                        </Paper>
                      );
                    })}
                    <Button onClick={downloadBandsCsv} startIcon={<DownloadIcon />} variant="outlined" sx={{ alignSelf: 'flex-start' }}>
                      Exportar bandas CSV
                    </Button>
                  </Stack>
                )}
              </Stack>
            </TabPanel>

            <TabPanel active={tab} value="copies">
              <Stack spacing={3}>
                <Section title="Copys listos para anuncios" subtitle="Piezas base para pauta, posts y retargeting." icon={<CampaignIcon />}>
                  <Grid container spacing={2}>
                    {COPY_BANK.map((item) => (
                      <Grid item xs={12} md={6} key={item.id}>
                        <CopyCard item={item} onCopy={copyTextFromClick} />
                      </Grid>
                    ))}
                  </Grid>
                </Section>

                <Section title="Voiceovers" subtitle="Guiones cortos para versiones con voz IA o locución propia." icon={<MovieCreationIcon />}>
                  <Grid container spacing={2}>
                    {VOICEOVER_SCRIPTS.map((item) => (
                      <Grid item xs={12} md={4} key={item.id}>
                        <CopyCard item={item} onCopy={copyTextFromClick} />
                      </Grid>
                    ))}
                  </Grid>
                </Section>

                <Section title="WhatsApp" subtitle="Secuencia mínima para respuesta y seguimiento." icon={<WhatsAppIcon />}>
                  <Grid container spacing={2}>
                    {WHATSAPP_TEMPLATES.map((item) => (
                      <Grid item xs={12} md={6} key={item.id}>
                        <CopyCard item={item} onCopy={copyTextFromClick} />
                      </Grid>
                    ))}
                  </Grid>
                </Section>

                <Grid container spacing={2}>
                  <Grid item xs={12} md={6}>
                    <Section title="Preguntas del formulario" icon={<TaskAltIcon />}>
                      <Stack spacing={1}>
                        {FORM_QUESTIONS.map((question, index) => (
                          <Typography key={question} variant="body2">
                            {index + 1}. {question}
                          </Typography>
                        ))}
                      </Stack>
                    </Section>
                  </Grid>
                  <Grid item xs={12} md={6}>
                    <Section title="Manifiesto de campaña" icon={<PublicIcon />}>
                      <Typography variant="body2" color="text.secondary">
                        Ecuador tiene bandas con canciones, energía e identidad. Pero muchas veces no tienen una producción audiovisual
                        que esté al nivel de lo que hacen en vivo. TDF Sessions nace para cambiar eso.
                      </Typography>
                      <Divider />
                      <Typography variant="body2" fontWeight={800}>
                        Una live session profesional, en un escenario único, con audio multipista, cuatro cámaras y una producción pensada
                        para que tu música se vea y suene como merece.
                      </Typography>
                      <Button
                        onClick={() => copyTextFromClick(
                          'Ecuador tiene bandas con canciones, energía e identidad. Pero muchas veces no tienen una producción audiovisual que esté al nivel de lo que hacen en vivo. TDF Sessions nace para cambiar eso. Una live session profesional, en un escenario único, con audio multipista, cuatro cámaras y una producción pensada para que tu música se vea y suene como merece. Solo 10 bandas serán seleccionadas. Convocatoria abierta.',
                          'Manifiesto',
                        )}
                        startIcon={<ContentCopyIcon />}
                        variant="outlined"
                        sx={{ alignSelf: 'flex-start' }}
                      >
                        Copiar manifiesto
                      </Button>
                    </Section>
                  </Grid>
                </Grid>
              </Stack>
            </TabPanel>
          </Box>
        </Paper>
      </Stack>
    </PageShell>
  );
}
