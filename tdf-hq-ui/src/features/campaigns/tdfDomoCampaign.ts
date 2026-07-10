export type CampaignPhaseId = 'teaser' | 'launch' | 'desire' | 'closing';

export type CreativeAssetStatus = 'pendiente' | 'guion' | 'edicion' | 'aprobado' | 'publicado';

export type BandApplicationStage =
  | 'nuevo'
  | 'contactado'
  | 'postulacion'
  | 'calificado'
  | 'preseleccion'
  | 'seleccionado'
  | 'descartado';

export interface CampaignPhase {
  id: CampaignPhaseId;
  title: string;
  timing: string;
  dayRange: readonly [number, number];
  objective: string;
  messages: readonly string[];
  assetIds: readonly number[];
  cta: string;
}

export interface CampaignVideoAsset {
  id: number;
  title: string;
  role: string;
  bestUse: string;
  phaseIds: readonly CampaignPhaseId[];
}

export interface CampaignMetricState {
  reach: number;
  videoViews: number;
  video50Views: number;
  saves: number;
  shares: number;
  profileVisits: number;
  formClicks: number;
  whatsappMessages: number;
  applications: number;
  qualifiedApplications: number;
  selectedBands: number;
  adSpendUsd: number;
}

export interface CampaignSummary {
  costPerApplication: number | null;
  costPerQualified: number | null;
  applicationRate: number | null;
  qualifiedRate: number | null;
  selectionProgress: number;
  remainingSlots: number;
}

export interface BudgetSplit {
  id: string;
  label: string;
  percent: number;
  objective: string;
  creatives: readonly number[];
}

export interface BudgetRow extends BudgetSplit {
  amountUsd: number;
}

export interface ContentCalendarItem {
  day: number;
  title: string;
  format: string;
  phaseId: CampaignPhaseId;
  message: string;
  assetIds: readonly number[];
  cta: string;
}

export interface SelectionCriterion {
  key: keyof BandScoreBreakdown;
  label: string;
  weight: number;
}

export interface BandScoreBreakdown {
  musicQuality: number;
  livePerformance: number;
  artisticIdentity: number;
  stageVisuals: number;
  professionalism: number;
  diversity: number;
}

export interface TrackedBand {
  id: string;
  name: string;
  city: string;
  genre: string;
  source: string;
  contact: string;
  stage: BandApplicationStage;
  scores: BandScoreBreakdown;
  notes: string;
}

export interface CopyBankItem {
  id: string;
  title: string;
  intent: string;
  body: string;
}

const TARGET_SELECTED_BANDS = 10;
const ONE_DAY_MS = 24 * 60 * 60 * 1000;

export const CREATIVE_ASSET_STATUS_OPTIONS: readonly CreativeAssetStatus[] = [
  'pendiente',
  'guion',
  'edicion',
  'aprobado',
  'publicado',
] as const;

export const BAND_APPLICATION_STAGE_OPTIONS: readonly BandApplicationStage[] = [
  'nuevo',
  'contactado',
  'postulacion',
  'calificado',
  'preseleccion',
  'seleccionado',
  'descartado',
] as const;

export const BAND_APPLICATION_STAGE_LABELS: Record<BandApplicationStage, string> = {
  nuevo: 'Nuevo',
  contactado: 'Contactado',
  postulacion: 'Postulación',
  calificado: 'Calificado',
  preseleccion: 'Preselección',
  seleccionado: 'Seleccionado',
  descartado: 'Descartado',
};

export const DEFAULT_CAMPAIGN_METRICS: CampaignMetricState = {
  reach: 0,
  videoViews: 0,
  video50Views: 0,
  saves: 0,
  shares: 0,
  profileVisits: 0,
  formClicks: 0,
  whatsappMessages: 0,
  applications: 0,
  qualifiedApplications: 0,
  selectedBands: 0,
  adSpendUsd: 0,
};

export const CAMPAIGN_PHASES: readonly CampaignPhase[] = [
  {
    id: 'teaser',
    title: 'Teaser',
    timing: '3 a 5 días antes',
    dayRange: [-4, 0],
    objective: 'Generar intriga antes de abrir postulaciones.',
    messages: [
      'Algo grande viene para las bandas de Ecuador.',
      'Un escenario único. Solo 10 bandas.',
      'Muy pronto: TDF Sessions.',
    ],
    assetIds: [4, 1],
    cta: 'Mantente atento',
  },
  {
    id: 'launch',
    title: 'Lanzamiento',
    timing: 'Días 1 al 7',
    dayRange: [1, 7],
    objective: 'Conseguir las primeras postulaciones calificadas.',
    messages: [
      'Convocatoria abierta.',
      'Buscamos solo 10 bandas de todo el Ecuador.',
      'Postula a TDF Sessions x Domo Pululahua.',
    ],
    assetIds: [10, 1, 6],
    cta: 'Postula ahora',
  },
  {
    id: 'desire',
    title: 'Deseo y credibilidad',
    timing: 'Días 8 al 14',
    dayRange: [8, 14],
    objective: 'Demostrar el valor real del paquete técnico y creativo.',
    messages: [
      'No es un videoclip. Es una live session profesional.',
      '4 cámaras. Audio multipista. Edición profesional.',
      'Tu música merece sonar y verse así.',
    ],
    assetIds: [2, 5, 7, 9],
    cta: 'Envía tu postulación',
  },
  {
    id: 'closing',
    title: 'Urgencia y cierre',
    timing: 'Días 15 al 21',
    dayRange: [15, 21],
    objective: 'Convertir a bandas indecisas antes del cierre.',
    messages: [
      'Últimos días para postular.',
      'Solo 10 bandas serán seleccionadas.',
      '30 minutos en vivo para demostrar quién eres.',
    ],
    assetIds: [3, 8, 10],
    cta: 'Cierra tu postulación hoy',
  },
] as const;

export const VIDEO_ASSETS: readonly CampaignVideoAsset[] = [
  { id: 1, title: 'La Convocatoria', role: 'Anuncio principal', bestUse: 'Lanzamiento', phaseIds: ['teaser', 'launch'] },
  { id: 2, title: 'Tu música merece esto', role: 'Aspiracional', bestUse: 'Reels + retargeting', phaseIds: ['desire'] },
  { id: 3, title: 'Tienes 30 minutos', role: 'Urgencia', bestUse: 'Últimos días', phaseIds: ['closing'] },
  { id: 4, title: 'El escenario perfecto', role: 'Emoción / branding', bestUse: 'Teaser', phaseIds: ['teaser'] },
  { id: 5, title: 'Detrás de cámaras', role: 'Credibilidad técnica', bestUse: 'Consideración', phaseIds: ['desire'] },
  { id: 6, title: 'Desde cualquier rincón del Ecuador', role: 'Alcance nacional', bestUse: 'Lanzamiento', phaseIds: ['launch'] },
  { id: 7, title: 'No es un videoclip', role: 'Diferenciación', bestUse: 'Anuncio educativo', phaseIds: ['desire'] },
  { id: 8, title: 'Haz que te descubran', role: 'Resultado / oportunidad', bestUse: 'Retargeting', phaseIds: ['closing'] },
  { id: 9, title: 'Todo incluido', role: 'Oferta clara', bestUse: 'Conversión', phaseIds: ['desire'] },
  { id: 10, title: 'Convocatoria abierta', role: 'Trailer hero', bestUse: 'Pauta principal', phaseIds: ['launch', 'closing'] },
] as const;

export const BUDGET_SPLIT: readonly BudgetSplit[] = [
  {
    id: 'awareness',
    label: 'Awareness',
    percent: 45,
    objective: 'Alcance y reproducciones de video.',
    creatives: [1, 4, 10],
  },
  {
    id: 'consideration',
    label: 'Consideración',
    percent: 25,
    objective: 'Visitas al perfil, clics al formulario o mensajes.',
    creatives: [2, 5, 7, 9],
  },
  {
    id: 'conversion',
    label: 'Conversión / leads',
    percent: 25,
    objective: 'Formularios, WhatsApp y postulaciones calificadas.',
    creatives: [3, 8, 10],
  },
  {
    id: 'testing',
    label: 'Testing creativo',
    percent: 5,
    objective: 'Pruebas rápidas de hooks, thumbnails y audiencias.',
    creatives: [1, 2, 4],
  },
] as const;

export const CONTENT_CALENDAR: readonly ContentCalendarItem[] = [
  { day: 1, title: 'Trailer principal', format: 'Reel / Shorts', phaseId: 'launch', message: 'Convocatoria abierta.', assetIds: [10], cta: 'Postula ahora' },
  { day: 2, title: 'Buscamos solo 10 bandas', format: 'Reel', phaseId: 'launch', message: 'Solo 10 bandas de todo el Ecuador.', assetIds: [1], cta: 'Postula ahora' },
  { day: 3, title: 'Qué incluye TDF Sessions', format: 'Carrusel', phaseId: 'launch', message: '30 minutos, 4 cámaras, audio multipista y reels.', assetIds: [9], cta: 'Revisa los detalles' },
  { day: 4, title: 'Tu música merece sonar y verse así', format: 'Reel', phaseId: 'launch', message: 'Tu banda ya tiene la energía; ahora necesita producción.', assetIds: [2], cta: 'Postula' },
  { day: 5, title: 'Encuesta Pululahua', format: 'Stories', phaseId: 'launch', message: '¿Tu banda tocaría en vivo en el Pululahua?', assetIds: [4], cta: 'Responde' },
  { day: 6, title: 'Desde cualquier rincón del Ecuador', format: 'Reel', phaseId: 'launch', message: 'Quito, Guayaquil, Cuenca, Loja, Manta y más.', assetIds: [6], cta: 'Postula desde tu ciudad' },
  { day: 7, title: 'Live corto de convocatoria', format: 'Instagram Live', phaseId: 'launch', message: 'Explicar requisitos, beneficios y fechas.', assetIds: [10], cta: 'Haz tus preguntas' },
  { day: 8, title: 'Cámaras, audio y luces', format: 'Reel técnico', phaseId: 'desire', message: '4 cámaras, audio multipista y producción real.', assetIds: [5], cta: 'Envía tu postulación' },
  { day: 9, title: 'No es videoclip vs Live Session', format: 'Carrusel', phaseId: 'desire', message: 'Tocar de verdad cambia el resultado.', assetIds: [7], cta: 'Conoce el formato' },
  { day: 10, title: '30 minutos en vivo', format: 'Reel', phaseId: 'desire', message: '30 minutos para demostrar quién eres.', assetIds: [3], cta: 'Postula' },
  { day: 11, title: 'FAQ de postulación', format: 'Stories FAQ', phaseId: 'desire', message: 'Costos, fechas, selección, derechos y logística.', assetIds: [9], cta: 'Pregunta por DM' },
  { day: 12, title: 'Cada detalle cuenta', format: 'Reel', phaseId: 'desire', message: 'Edición, color, audio y reels para redes.', assetIds: [5, 9], cta: 'Envía tu material' },
  { day: 13, title: 'Criterios de selección', format: 'Post fijo', phaseId: 'desire', message: 'Calidad musical, vivo, identidad y profesionalismo.', assetIds: [2], cta: 'Prepara tu postulación' },
  { day: 14, title: 'Recordatorio de cupos', format: 'Reel / Story', phaseId: 'desire', message: 'Solo 10 bandas serán parte de esta experiencia.', assetIds: [1, 10], cta: 'Postula hoy' },
  { day: 15, title: 'Tu próxima oportunidad', format: 'Reel', phaseId: 'closing', message: 'Haz que te descubran con una sesión profesional.', assetIds: [8], cta: 'Cierra tu postulación' },
  { day: 16, title: 'Story con contador', format: 'Stories', phaseId: 'closing', message: 'Cuenta regresiva al cierre.', assetIds: [3], cta: 'Activa recordatorio' },
  { day: 17, title: 'Últimos días', format: 'Reel', phaseId: 'closing', message: 'Últimos días para postular.', assetIds: [3, 10], cta: 'Postula ahora' },
  { day: 18, title: 'Qué recibe cada banda', format: 'Carrusel', phaseId: 'closing', message: 'Live session completa, reels, publicación y experiencia.', assetIds: [9], cta: 'Completa el formulario' },
  { day: 19, title: 'Domo + CTA', format: 'Video corto', phaseId: 'closing', message: 'Un escenario imposible de ignorar.', assetIds: [4, 10], cta: 'Postula antes del cierre' },
  { day: 20, title: 'Último llamado', format: 'Reel / Stories', phaseId: 'closing', message: 'Mañana cierran postulaciones.', assetIds: [3, 8], cta: 'Entra al formulario' },
  { day: 21, title: 'Cierre de convocatoria', format: 'Reel / Stories', phaseId: 'closing', message: 'Postulaciones cierran hoy.', assetIds: [10], cta: 'Cierra tu postulación hoy' },
] as const;

export const SELECTION_CRITERIA: readonly SelectionCriterion[] = [
  { key: 'musicQuality', label: 'Calidad musical', weight: 30 },
  { key: 'livePerformance', label: 'Interpretación en vivo', weight: 25 },
  { key: 'artisticIdentity', label: 'Identidad artística', weight: 20 },
  { key: 'stageVisuals', label: 'Propuesta visual / escénica', weight: 10 },
  { key: 'professionalism', label: 'Compromiso y profesionalismo', weight: 10 },
  { key: 'diversity', label: 'Diversidad territorial o de género musical', weight: 5 },
] as const;

export const EMPTY_BAND_SCORE: BandScoreBreakdown = {
  musicQuality: 0,
  livePerformance: 0,
  artisticIdentity: 0,
  stageVisuals: 0,
  professionalism: 0,
  diversity: 0,
};

export const COPY_BANK: readonly CopyBankItem[] = [
  {
    id: 'directo',
    title: 'Copy directo',
    intent: 'Anuncio principal',
    body: 'Convocatoria abierta.\nBuscamos solo 10 bandas de todo el Ecuador para vivir una experiencia única: una live session profesional en el Domo Pululahua, producida por TDF Studio.\n\n30 minutos en vivo. 4 cámaras. Audio multipista. Edición profesional.\nPostula ahora.',
  },
  {
    id: 'emocional',
    title: 'Copy emocional',
    intent: 'Awareness / aspiracional',
    body: 'Tu banda no necesita otro video cualquiera.\nNecesita una sesión que capture su energía real.\n\nTDF Sessions x Domo Pululahua abre convocatoria para 10 bandas ecuatorianas.\n\nTu música merece sonar y verse así.',
  },
  {
    id: 'tecnico',
    title: 'Copy técnico',
    intent: 'Consideración',
    body: 'Esto no es un videoclip.\nEs una Live Session profesional:\n\n- 30 minutos en vivo\n- 4 cámaras profesionales\n- Audio multipista\n- Edición profesional\n- Corrección de color\n- Reels para redes sociales\n\nProducido por TDF Studio.',
  },
  {
    id: 'urgencia',
    title: 'Copy de urgencia',
    intent: 'Cierre',
    body: 'Solo 10 bandas serán seleccionadas.\nSolo 30 minutos para demostrar quién eres.\nSolo una experiencia como esta en el Pululahua.\n\nPostula a TDF Sessions.',
  },
  {
    id: 'nacional',
    title: 'Copy nacional',
    intent: 'Alcance territorial',
    body: 'Desde cualquier rincón del Ecuador, queremos escuchar a las bandas que están moviendo la escena.\n\nQuito, Guayaquil, Cuenca, Ambato, Loja, Manta, Ibarra, Riobamba y más.\n\nLa convocatoria está abierta.',
  },
] as const;

export const VOICEOVER_SCRIPTS: readonly CopyBankItem[] = [
  {
    id: 'hero',
    title: 'Voiceover trailer hero',
    intent: 'Trailer principal',
    body: 'Ecuador tiene bandas que merecen sonar más grande. Por eso abrimos una convocatoria única: TDF Sessions x Domo Pululahua. Treinta minutos en vivo, cuatro cámaras, audio multipista y una producción profesional. Solo diez bandas serán seleccionadas. Postula ahora.',
  },
  {
    id: 'aspiracional',
    title: 'Voiceover aspiracional',
    intent: 'Deseo',
    body: 'Tu banda ya tiene la música. Ya tiene la energía. Ahora necesita una sesión que la muestre como merece. No es un videoclip. Es una live session profesional, producida por TDF Studio.',
  },
  {
    id: 'urgencia',
    title: 'Voiceover urgencia',
    intent: 'Cierre',
    body: 'Tienes treinta minutos. Cuatro cámaras grabando. Audio multipista corriendo. Un escenario único. Y una oportunidad para demostrar quién eres. TDF Sessions: convocatoria abierta.',
  },
] as const;

export const WHATSAPP_TEMPLATES: readonly CopyBankItem[] = [
  {
    id: 'auto',
    title: 'Respuesta automática',
    intent: 'Primer contacto',
    body: '¡Gracias por tu interés en TDF Sessions x Domo Pululahua!\nEstamos seleccionando solo 10 bandas de todo el Ecuador para una live session profesional producida por TDF Studio.\n\nPara postular, envíanos:\n\n1. Nombre de la banda\n2. Ciudad\n3. Género\n4. Instagram\n5. Link de música o video en vivo\n6. Nombre y teléfono de contacto',
  },
  {
    id: 'seguimiento',
    title: 'Seguimiento recibido',
    intent: 'Confirmación',
    body: '¡Recibido! El equipo de TDF revisará la postulación considerando calidad musical, interpretación en vivo, identidad artística y disponibilidad. Las bandas preseleccionadas serán contactadas directamente.',
  },
] as const;

export const FORM_QUESTIONS: readonly string[] = [
  'Nombre de la banda',
  'Ciudad',
  'Género musical',
  'Número de integrantes',
  'Nombre de contacto',
  'WhatsApp',
  'Instagram',
  'YouTube / Spotify / SoundCloud',
  'Link a una presentación en vivo o ensayo',
  '¿Por qué tu banda debería ser seleccionada?',
  'Disponibilidad de fechas',
  '¿Cuentan con canciones originales?',
  '¿Autorizan el uso de imagen, audio y video para promoción de TDF Sessions?',
] as const;

export const CREATIVE_RULES = {
  do: [
    'Usar IA para atmósfera, transiciones, fondos y storyboards.',
    'Agregar textos y logos reales en edición.',
    'Usar los videos IA como teaser, no como prueba falsa del evento.',
    'Mantener estética cinematográfica, sobria y premium.',
    'Usar música propia de TDF.',
    'Mostrar claramente el valor técnico del paquete.',
  ],
  avoid: [
    'Textos generados dentro del video por IA.',
    'Logos inventados.',
    'Manos deformadas tocando instrumentos.',
    'Prometer exposición garantizada o fama.',
    'Usar voces clonadas sin consentimiento.',
    'Crear comentarios falsos o métricas falsas.',
  ],
} as const;

const clampScore = (value: number): number => {
  if (!Number.isFinite(value)) return 0;
  return Math.min(10, Math.max(0, value));
};

const nonNegativeNumber = (value: number): number => {
  if (!Number.isFinite(value) || value < 0) return 0;
  return value;
};

const safeDivide = (numerator: number, denominator: number): number | null => {
  if (!Number.isFinite(numerator) || !Number.isFinite(denominator) || denominator <= 0) return null;
  return numerator / denominator;
};

const parseDateInput = (value: string): Date | null => {
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value.trim());
  if (!match) return null;
  const year = Number(match[1]);
  const month = Number(match[2]);
  const day = Number(match[3]);
  const parsed = new Date(year, month - 1, day, 12, 0, 0, 0);
  if (parsed.getFullYear() !== year || parsed.getMonth() !== month - 1 || parsed.getDate() !== day) return null;
  return parsed;
};

const formatDateInput = (date: Date): string => {
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  return `${date.getFullYear()}-${month}-${day}`;
};

export function calculateCampaignSummary(metrics: CampaignMetricState): CampaignSummary {
  const applications = nonNegativeNumber(metrics.applications);
  const qualifiedApplications = nonNegativeNumber(metrics.qualifiedApplications);
  const selectedBands = nonNegativeNumber(metrics.selectedBands);
  const spend = nonNegativeNumber(metrics.adSpendUsd);

  return {
    costPerApplication: safeDivide(spend, applications),
    costPerQualified: safeDivide(spend, qualifiedApplications),
    applicationRate: safeDivide(applications, nonNegativeNumber(metrics.formClicks)),
    qualifiedRate: safeDivide(qualifiedApplications, applications),
    selectionProgress: Math.min(1, safeDivide(selectedBands, TARGET_SELECTED_BANDS) ?? 0),
    remainingSlots: Math.max(0, TARGET_SELECTED_BANDS - Math.round(selectedBands)),
  };
}

export function calculateBandScore(scores: BandScoreBreakdown): number {
  const weightedScore = SELECTION_CRITERIA.reduce(
    (sum, criterion) => sum + (clampScore(scores[criterion.key]) / 10) * criterion.weight,
    0,
  );
  return Math.round(weightedScore * 10) / 10;
}

export function buildBudgetRows(totalBudgetUsd: number): BudgetRow[] {
  const total = nonNegativeNumber(totalBudgetUsd);
  return BUDGET_SPLIT.map((item) => ({
    ...item,
    amountUsd: Math.round(total * item.percent) / 100,
  }));
}

export function getCampaignDayFromDates(startDateInput: string, todayInput: string): number | null {
  const startDate = parseDateInput(startDateInput);
  const todayDate = parseDateInput(todayInput);
  if (!startDate || !todayDate) return null;
  return Math.floor((todayDate.getTime() - startDate.getTime()) / ONE_DAY_MS) + 1;
}

export function getCurrentPhaseId(campaignDay: number | null): CampaignPhaseId | null {
  if (campaignDay == null) return null;
  return CAMPAIGN_PHASES.find((phase) => campaignDay >= phase.dayRange[0] && campaignDay <= phase.dayRange[1])?.id ?? null;
}

export function getContentCalendarDate(day: number, startDateInput: string): string {
  const startDate = parseDateInput(startDateInput);
  if (!startDate) return '';
  const date = new Date(startDate);
  date.setDate(startDate.getDate() + day - 1);
  return formatDateInput(date);
}

export function getAssetLabel(assetId: number): string {
  const asset = VIDEO_ASSETS.find((item) => item.id === assetId);
  return asset ? `Video ${asset.id}: ${asset.title}` : `Video ${assetId}`;
}

