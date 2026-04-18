import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Collapse,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  Grid,
  IconButton,
  InputAdornment,
  Link,
  Menu,
  MenuItem,
  Paper,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import ArrowDropDownIcon from '@mui/icons-material/ArrowDropDown';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import SaveIcon from '@mui/icons-material/Save';
import MoreVertIcon from '@mui/icons-material/MoreVert';
import SearchIcon from '@mui/icons-material/Search';
import ClearIcon from '@mui/icons-material/Clear';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Link as RouterLink, useSearchParams } from 'react-router-dom';
import {
  Courses,
  type CourseCohortOptionDTO,
  type CourseEmailEventDTO,
  type CourseRegistrationDTO,
  type CourseRegistrationDossierDTO,
  type CourseRegistrationFollowUpDTO,
  type CourseRegistrationReceiptDTO,
} from '../api/courses';
import GoogleDriveUploadWidget from '../components/GoogleDriveUploadWidget';
import type { DriveFileInfo } from '../services/googleDrive';
import { formatTimestampForDisplay, parseTimestamp } from '../utils/dateTime';

type StatusFilter = 'all' | 'pending_payment' | 'paid' | 'cancelled';
type DossierIntent = 'review' | 'markPaid';
type FlashSeverity = 'success' | 'error' | 'info' | 'warning';
const DEFAULT_LIMIT = 200;
const markPaidReceiptSectionHelpText = 'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y luego podrás marcar la inscripción como pagada.';
const emptyReceiptAlertMessage = 'Agrega el primer comprobante para documentar el pago y habilitar Marcar pagado. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const emptyReceiptEvidenceAlertMessage = 'Agrega el primer comprobante solo si necesitas documentar evidencia de pago. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const firstReceiptComposerHelpText = 'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y aparecerá aquí con enlace y acciones para revisarlo después.';
const receiptComposerHelpText = 'Este formulario ya está abierto para guardar otro comprobante o pegar un enlace existente.';
const editingReceiptComposerHelpText = 'Edita el comprobante y guarda los cambios para actualizar el registro.';
const initialEmptyStateConfigMessage = 'Todavía no hay inscripciones. Configura el curso inicial; cuando llegue la primera inscripción podrás revisar pago, seguimiento y correos aquí.';
const initialEmptyStateMultiCohortMessage = 'Todavía no hay inscripciones. Ya hay formularios configurados; abre Cursos para decidir cuál compartir primero.';
const initialEmptyStateConfigActionLabel = 'Configurar cursos';
const initialEmptyStateMultiCohortActionLabel = 'Abrir Cursos';
const initialEmptyStateFormActionLabel = 'Abrir formulario público';
const initialCohortResolutionMessage = 'Revisando cohortes configuradas para mostrar el siguiente paso correcto.';
const initialCohortErrorMessage = 'No se pudieron cargar las cohortes para elegir qué formulario compartir. Reintenta cohortes antes de filtrar o revisar la lista.';
const cohortFilterUnavailableMessage = 'No se pudieron cargar cohortes. La lista sigue disponible; reintenta cohortes para recuperar el filtro por curso.';
const buildSingleCohortInitialEmptyStateMessage = (cohortLabel: string) =>
  `Todavía no hay inscripciones para ${cohortLabel}. Cuando llegue la primera podrás revisar pago, seguimiento y correos aquí.`;
type RegistrationIdentityKind = 'name' | 'contact' | 'record';
const buildCompactDossierScopeHint = (targetLabel: string) =>
  `Abre el expediente desde ${targetLabel}; usa Cambiar estado para acciones rápidas.`;
const buildDossierOnlyScopeHint = (targetLabel: string) =>
  `Abre el expediente desde ${targetLabel}.`;
const emptyNotesHelperText = 'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.';
const markPaidEmptyNotesHelperText = 'Agrega una nota solo si necesitas dejar contexto extra sobre este pago.';
const showSystemEmailsLabel = 'Ver correos del sistema';
const hideSystemEmailsLabel = 'Ocultar correos del sistema';
const systemEmailHistoryHelperText = 'Historial persistente de correos del sistema para esta inscripción. Usa el refresco del expediente para volver a consultarlo.';
const emptySystemEmailHistoryMessage = 'Todavía no hay correos del sistema registrados para esta inscripción. Cuando se envíe el primero, aparecerá aquí.';
const emptyFollowUpAlertMessage = 'Aún no hay seguimiento manual. Documenta llamadas, mensajes o próximos pasos desde aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.';
const markPaidEmptyFollowUpHelperText = 'Agrega seguimiento solo si necesitas dejar contexto manual aparte del comprobante o del cambio de estado.';
const firstFollowUpComposerHelpText = 'Este formulario ya está abierto para registrar el primer seguimiento. Guárdalo y aparecerá aquí para revisarlo después.';
const followUpComposerHelpText = 'Este formulario ya está abierto para registrar seguimiento. Guárdalo y aparecerá en el historial para revisarlo después.';
const editingFollowUpComposerHelpText = 'Edita el seguimiento y guarda los cambios para actualizar el historial.';
const openPaymentWorkflowLabel = 'Registrar pago';
const markPaidSuccessMessage = 'Inscripción marcada como pagada.';
const activeStatusFilterHelperText = 'Esta vista ya está filtrada por ese estado. Tócalo otra vez para volver a ver todos.';
const customStatusFilterUnavailableMessage = 'Los estados visibles no coinciden con los filtros estándar. Usa el menú de estado de cada inscripción para normalizarlos.';
const defaultPublicFormSource = 'landing';
const MIN_DEFAULT_CSV_EXPORT_ROWS = 3;
const MIN_LOCAL_SEARCH_REGISTRATIONS = 8;
const LOCAL_SEARCH_LABEL = 'Buscar inscripciones';

interface FlashState {
  severity: FlashSeverity;
  message: string;
}

interface DossierTarget {
  reg: CourseRegistrationDTO;
  intent: DossierIntent;
}

interface ReceiptFormState {
  editingId: number | null;
  fileUrl: string;
  fileName: string;
  notes: string;
}

interface FollowUpFormState {
  editingId: number | null;
  entryType: string;
  subject: string;
  notes: string;
  attachmentUrl: string;
  attachmentName: string;
  nextFollowUpAt: string;
}

const statusFilters: readonly StatusFilter[] = ['all', 'pending_payment', 'paid', 'cancelled'];
const statusFilterLabels: Record<StatusFilter, string> = {
  all: 'Todos',
  pending_payment: 'Pendiente de pago',
  paid: 'Pagado',
  cancelled: 'Cancelado',
};
const followUpTypeOptions = ['note', 'call', 'whatsapp', 'email', 'payment_receipt', 'status_change', 'registration'];

const emptyReceiptForm = (): ReceiptFormState => ({
  editingId: null,
  fileUrl: '',
  fileName: '',
  notes: '',
});

const emptyFollowUpForm = (): FollowUpFormState => ({
  editingId: null,
  entryType: 'note',
  subject: '',
  notes: '',
  attachmentUrl: '',
  attachmentName: '',
  nextFollowUpAt: '',
});

const isStatusFilter = (value: string): value is StatusFilter =>
  statusFilters.some((status) => status === value);

const parseStatusFilter = (value: string | null): StatusFilter => {
  const trimmed = value?.trim() ?? '';
  return isStatusFilter(trimmed) ? trimmed : 'all';
};

const parsePositiveLimit = (value: string | null, fallback = DEFAULT_LIMIT): number => {
  const trimmed = value?.trim() ?? '';
  if (!/^\d+$/.test(trimmed)) return fallback;
  const parsed = Number(trimmed);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : fallback;
};

const summarizeActiveFilters = ({
  cohortLabel,
  status,
  limit,
}: {
  cohortLabel: string;
  status: StatusFilter;
  limit: number;
}) => {
  const parts: string[] = [];
  const trimmedCohortLabel = cohortLabel.trim();
  if (trimmedCohortLabel) parts.push(`cohorte ${trimmedCohortLabel}`);
  if (status !== 'all') parts.push(`estado ${statusFilterLabels[status].toLowerCase()}`);
  if (limit !== DEFAULT_LIMIT) parts.push(`límite ${limit}`);
  return parts.join(' · ');
};

const buildAutomaticFilterHelpText = ({
  combinedSingleChoiceSummary,
  hasVisibleRegistrations,
  showAdvancedLimitControl,
  showSingleStatusSummary,
  singleAvailableCohortLabel,
}: {
  combinedSingleChoiceSummary: string;
  hasVisibleRegistrations: boolean;
  showAdvancedLimitControl: boolean;
  showSingleStatusSummary: boolean;
  singleAvailableCohortLabel: string;
}) => {
  if (combinedSingleChoiceSummary) return '';

  if (singleAvailableCohortLabel || showSingleStatusSummary) {
    return '';
  }

  const filterStartingPoint = singleAvailableCohortLabel
    ? 'Usa Estado.'
    : showSingleStatusSummary
      ? 'Usa cohorte.'
      : 'Empieza por cohorte y estado.';
  const limitGuidance = showAdvancedLimitControl
    ? 'Usa Ajustar límite solo cuando necesites revisar un lote distinto.'
    : 'Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.';
  const emptySuffix = hasVisibleRegistrations
    ? ''
    : ' Ajusta la vista o usa refrescar si esperabas resultados.';

  return `Los filtros se aplican automáticamente al cambiar. ${filterStartingPoint} ${limitGuidance}${emptySuffix}`;
};

const getResetViewLabel = ({
  hasCustomLimit,
  hasSlugFilter,
  hasStatusFilter,
}: {
  hasCustomLimit: boolean;
  hasSlugFilter: boolean;
  hasStatusFilter: boolean;
}) => {
  if (hasCustomLimit && (hasSlugFilter || hasStatusFilter)) return 'Restablecer vista';
  if (hasCustomLimit) return 'Restablecer límite';
  if (hasSlugFilter && !hasStatusFilter) return 'Mostrar todas las cohortes';
  if (!hasSlugFilter && hasStatusFilter) return 'Mostrar todos los estados';
  if (hasSlugFilter && hasStatusFilter) return 'Restablecer filtros';
  return 'Restablecer vista';
};

const formatRowCountLabel = (count: number) => `${count} fila${count === 1 ? '' : 's'}`;
const formatRegistrationCountLabel = (count: number) => `${count} inscripci${count === 1 ? 'ón' : 'ones'}`;
const formatLoadedRegistrationCountLabel = (count: number) =>
  `${count} inscripci${count === 1 ? 'ón cargada' : 'ones cargadas'}`;
const formatLocalSearchResultSummary = (visibleCount: number, loadedCount: number) =>
  `Mostrando ${visibleCount} de ${formatLoadedRegistrationCountLabel(loadedCount)}.`;
const buildReachedListLimitSummary = (limit: number) =>
  `Se cargó el límite de ${limit} inscripciones; usa Ajustar límite si necesitas revisar más registros.`;

const formatDate = (iso: string | null | undefined) => formatTimestampForDisplay(iso, '-');
const formatOptionalDate = (iso: string | null | undefined) => {
  const formatted = formatDate(iso);
  return formatted === '-' ? '' : formatted;
};

const isRegistrationStatus = (
  status: string,
): status is Exclude<StatusFilter, 'all'> => (
  status === 'pending_payment' || status === 'paid' || status === 'cancelled'
);

const customRegistrationStatusLabel = (status: string) => {
  const normalized = status.trim().toLowerCase().replace(/[_-]+/g, ' ').replace(/\s+/g, ' ');
  if (!normalized) return 'Estado desconocido';
  return normalized.replace(/\b\w/g, (match) => match.toUpperCase());
};

const registrationStatusLabel = (status: string) =>
  isRegistrationStatus(status) ? statusFilterLabels[status] : customRegistrationStatusLabel(status);

const registrationStatusButtonLabel = (
  status: string,
  useCompactActionLabel: boolean,
) => (useCompactActionLabel ? 'Cambiar estado' : registrationStatusLabel(status));

const registrationStatusChipColor = (
  status: string,
): 'default' | 'success' | 'warning' | 'error' => {
  if (status === 'paid') return 'success';
  if (status === 'cancelled') return 'error';
  if (status === 'pending_payment') return 'warning';
  return 'default';
};

const registrationStatusButtonColor = (
  status: string,
): 'inherit' | 'success' | 'warning' | 'error' => {
  if (status === 'paid') return 'success';
  if (status === 'cancelled') return 'error';
  if (status === 'pending_payment') return 'warning';
  return 'inherit';
};

const statusFilterChipLabel = (
  status: StatusFilter,
  counts: { total: number; pending_payment: number; paid: number; cancelled: number },
  includeCounts: boolean,
) => {
  const label = statusFilterLabels[status];
  if (!includeCounts) return label;
  if (status === 'all') return `${label} (${counts.total})`;
  return `${label} (${counts[status]})`;
};

const statusChip = (status: string) => {
  return (
    <Chip
      label={registrationStatusLabel(status)}
      color={registrationStatusChipColor(status)}
      size="small"
    />
  );
};

const canTransitionToStatus = (
  currentStatus: string,
  nextStatus: Exclude<StatusFilter, 'all'>,
) => {
  if (!isRegistrationStatus(currentStatus)) return true;
  return currentStatus !== nextStatus;
};

const canOpenPaymentWorkflowFromStatus = (currentStatus: string) =>
  currentStatus !== 'cancelled' && canTransitionToStatus(currentStatus, 'paid');

const pendingStatusMenuLabel = (currentStatus: string) =>
  currentStatus === 'cancelled' ? 'Reabrir como pendiente' : 'Marcar pendiente';

const pendingStatusMenuTargetLabel = (currentStatus: string) =>
  currentStatus === 'cancelled' ? 'reabrir la inscripción como pendiente' : 'marcarla pendiente';

const eventStatusColor = (
  status: string,
): 'default' | 'success' | 'warning' | 'error' | 'info' => {
  const normalized = status.toLowerCase();
  if (normalized === 'sent') return 'success';
  if (normalized === 'failed') return 'error';
  if (normalized === 'skipped') return 'warning';
  if (normalized === 'queued') return 'info';
  return 'default';
};

const eventStatusLabels: Record<string, string> = {
  sent: 'Enviado',
  failed: 'Falló',
  skipped: 'Omitido',
  queued: 'En cola',
};

const eventStatusLabel = (status: string) => {
  const normalized = status.trim().toLowerCase();
  const trimmed = status.trim();
  return eventStatusLabels[normalized] ?? (trimmed || 'Estado desconocido');
};

const eventTypeLabels: Record<string, string> = {
  note: 'Nota',
  call: 'Llamada',
  whatsapp: 'WhatsApp',
  email: 'Correo',
  payment_receipt: 'Comprobante de pago',
  status_change: 'Cambio de estado',
  registration: 'Inscripción',
  payment_reminder: 'Recordatorio de pago',
  registration_confirmation: 'Confirmación de inscripción',
  welcome_credentials: 'Credenciales de bienvenida',
};

const eventTypeLabel = (eventType: string) =>
  eventTypeLabels[eventType.trim().toLowerCase()]
  ?? eventType
    .trim()
    .toLowerCase()
    .replace(/_/g, ' ')
    .replace(/\b\w/g, (m) => m.toUpperCase());

const followUpActionTargetLabel = (entry: CourseRegistrationFollowUpDTO) =>
  entry.crfSubject ?? `${eventTypeLabel(entry.crfEntryType)} del ${formatDate(entry.crfCreatedAt)}`;

const cohortOptionLabel = (cohort: CourseCohortOptionDTO) => {
  const slug = cohort.ccSlug.trim();
  const title = cohort.ccTitle?.trim();
  if (!title) return slug;
  return `${title} (${slug})`;
};

const registrationSourceLabel = (source: string | null | undefined) => {
  const trimmed = source?.trim() ?? '';
  return trimmed === '' ? 'Sin fuente' : trimmed;
};

const normalizeRegistrationSourceKey = (sourceLabel: string) =>
  registrationSourceLabel(sourceLabel).toLocaleLowerCase('es');

const isDefaultPublicFormSource = (sourceLabel: string) =>
  sourceLabel.trim().toLowerCase() === defaultPublicFormSource;

const registrationIdentityDisplay = (
  fullName: string | null | undefined,
  email: string | null | undefined,
  phone: string | null | undefined,
  registrationId?: number | null,
) => {
  const trimmedName = fullName?.trim() ?? '';
  const trimmedEmail = email?.trim() ?? '';
  const trimmedPhone = phone?.trim() ?? '';
  const fallbackIdentity = registrationId == null ? 'Sin nombre' : `Registro #${registrationId}`;

  if (trimmedName) {
    return {
      primary: trimmedName,
      secondary: registrationContactSummary(trimmedEmail, trimmedPhone),
    };
  }

  if (trimmedEmail) {
    return {
      primary: trimmedEmail,
      secondary: trimmedPhone,
    };
  }

  if (trimmedPhone) {
    return {
      primary: trimmedPhone,
      secondary: '',
    };
  }

  return {
    primary: fallbackIdentity,
    secondary: 'Sin correo ni teléfono',
  };
};

const registrationIdentityKind = (
  reg: Pick<CourseRegistrationDTO, 'crFullName' | 'crEmail' | 'crPhoneE164'>,
): RegistrationIdentityKind => {
  if (reg.crFullName?.trim()) return 'name';
  if (reg.crEmail?.trim() || reg.crPhoneE164?.trim()) return 'contact';
  return 'record';
};

const registrationIdentityTargetLabel = (registrations: readonly CourseRegistrationDTO[]) => {
  const identityKinds = new Set(registrations.map(registrationIdentityKind));
  if (identityKinds.size === 1) {
    const [kind] = Array.from(identityKinds);
    if (kind === 'contact') return 'el contacto';
    if (kind === 'record') return 'el registro';
  }
  if (identityKinds.size > 1 && !identityKinds.has('name')) return 'la identidad principal';
  return 'el nombre';
};

const registrationContactSummary = (email: string | null | undefined, phone: string | null | undefined) => {
  const trimmedEmail = email?.trim() ?? '';
  const trimmedPhone = phone?.trim() ?? '';
  const parts = [trimmedEmail, trimmedPhone].filter((value) => value !== '');
  if (parts.length === 0) return 'Sin correo ni teléfono';
  return parts.join(' · ');
};

const registrationActionTargetLabel = (
  reg: Pick<CourseRegistrationDTO, 'crId' | 'crFullName' | 'crEmail' | 'crPhoneE164'>,
) => {
  const trimmedName = reg.crFullName?.trim() ?? '';
  if (trimmedName) return trimmedName;
  const trimmedEmail = reg.crEmail?.trim() ?? '';
  if (trimmedEmail) return trimmedEmail;
  const trimmedPhone = reg.crPhoneE164?.trim() ?? '';
  if (trimmedPhone) return trimmedPhone;
  return `registro #${reg.crId}`;
};

const normalizeRegistrationActionTargetKey = (
  reg: Pick<CourseRegistrationDTO, 'crId' | 'crFullName' | 'crEmail' | 'crPhoneE164'>,
) => registrationActionTargetLabel(reg).trim().toLocaleLowerCase('es');

const getRegistrationIdsRequiringActionDisambiguator = (
  registrations: readonly CourseRegistrationDTO[],
) => {
  const targetCounts = new Map<string, number>();

  registrations.forEach((reg) => {
    const targetKey = normalizeRegistrationActionTargetKey(reg);
    targetCounts.set(targetKey, (targetCounts.get(targetKey) ?? 0) + 1);
  });

  return new Set(
    registrations
      .filter((reg) => (targetCounts.get(normalizeRegistrationActionTargetKey(reg)) ?? 0) > 1)
      .map((reg) => reg.crId),
  );
};

const registrationActionTargetLabelWithContext = (
  reg: Pick<CourseRegistrationDTO, 'crId' | 'crFullName' | 'crEmail' | 'crPhoneE164'>,
  needsDisambiguator: boolean,
) => {
  const baseLabel = registrationActionTargetLabel(reg);
  if (!needsDisambiguator) return baseLabel;

  const identity = registrationIdentityDisplay(reg.crFullName, reg.crEmail, reg.crPhoneE164, reg.crId);
  const secondary = identity.secondary.trim();
  if (secondary && secondary !== 'Sin correo ni teléfono') {
    return `${baseLabel} (${secondary})`;
  }

  return `${baseLabel} (registro #${reg.crId})`;
};

const registrationListContextSummary = ({
  cohortLabel,
  createdAt,
  hasNotes,
  showCreatedAt = true,
  showCohort,
  showSource,
  source,
}: {
  cohortLabel: string;
  createdAt: string | null | undefined;
  hasNotes: boolean;
  showCreatedAt?: boolean;
  showCohort: boolean;
  showSource: boolean;
  source: string | null | undefined;
}) => {
  const parts: string[] = [];
  const trimmedCohortLabel = cohortLabel.trim();
  const trimmedSource = source?.trim() ?? '';
  if (showCohort && trimmedCohortLabel) parts.push(`Cohorte: ${trimmedCohortLabel}`);
  if (showSource && trimmedSource && !isDefaultPublicFormSource(trimmedSource)) {
    parts.push(`Fuente: ${registrationSourceLabel(trimmedSource)}`);
  }
  const createdLabel = showCreatedAt ? formatOptionalDate(createdAt) : '';
  if (createdLabel) parts.push(`Creado: ${createdLabel}`);
  if (hasNotes) parts.push('Notas internas');
  return parts.join(' · ');
};

const registrationDossierContextSummary = ({
  courseLabel,
  createdAt,
  source,
}: {
  courseLabel: string;
  createdAt: string | null | undefined;
  source: string | null | undefined;
}) => {
  const trimmedCourseLabel = courseLabel.trim();
  const parts = trimmedCourseLabel ? [`Curso: ${trimmedCourseLabel}`] : [];
  const trimmedSource = source?.trim() ?? '';
  if (trimmedSource && !isDefaultPublicFormSource(trimmedSource)) {
    parts.push(`Fuente: ${trimmedSource}`);
  }
  const createdLabel = formatOptionalDate(createdAt);
  if (createdLabel) parts.push(`Creado: ${createdLabel}`);
  return parts.join(' · ');
};

const trimToNull = (value: string): string | null => {
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
};

const preferNonEmptyText = (primary?: string | null, fallback?: string | null) => {
  const trimmedPrimary = primary?.trim();
  if (trimmedPrimary) return trimmedPrimary;
  const trimmedFallback = fallback?.trim();
  return trimmedFallback || null;
};

const preferPositiveId = (primary?: number | null, fallback?: number | null) => {
  if (typeof primary === 'number' && Number.isInteger(primary) && primary > 0) return primary;
  if (typeof fallback === 'number' && Number.isInteger(fallback) && fallback > 0) return fallback;
  return primary ?? fallback ?? null;
};

const looksLikeImageResource = (url?: string | null, fileName?: string | null) => {
  const candidate = `${url ?? ''} ${fileName ?? ''}`;
  return /\.(png|jpe?g|gif|webp|bmp|svg)(?:$|[?#])/i.test(candidate);
};

const toLocalDateTimeInputValue = (value?: string | null) => {
  const parsed = parseTimestamp(value);
  if (!parsed) return '';
  const tzOffsetMs = parsed.getTimezoneOffset() * 60_000;
  return new Date(parsed.getTime() - tzOffsetMs).toISOString().slice(0, 16);
};

const toIsoStringFromLocalDateTime = (value: string): string | null => {
  const trimmed = value.trim();
  if (!trimmed) return null;
  const parsed = new Date(trimmed);
  return Number.isNaN(parsed.getTime()) ? null : parsed.toISOString();
};

const mergeCourseRegistrationRecords = (
  primary: CourseRegistrationDTO,
  fallback: CourseRegistrationDTO,
): CourseRegistrationDTO => ({
  ...primary,
  crPartyId: preferPositiveId(primary.crPartyId, fallback.crPartyId),
  crFullName: preferNonEmptyText(primary.crFullName, fallback.crFullName),
  crEmail: preferNonEmptyText(primary.crEmail, fallback.crEmail),
  crPhoneE164: preferNonEmptyText(primary.crPhoneE164, fallback.crPhoneE164),
  crSource: preferNonEmptyText(primary.crSource, fallback.crSource),
  crAdminNotes: preferNonEmptyText(primary.crAdminNotes, fallback.crAdminNotes),
  crHowHeard: preferNonEmptyText(primary.crHowHeard, fallback.crHowHeard),
  crUtmSource: preferNonEmptyText(primary.crUtmSource, fallback.crUtmSource),
  crUtmMedium: preferNonEmptyText(primary.crUtmMedium, fallback.crUtmMedium),
  crUtmCampaign: preferNonEmptyText(primary.crUtmCampaign, fallback.crUtmCampaign),
  crUtmContent: preferNonEmptyText(primary.crUtmContent, fallback.crUtmContent),
});

const dedupeCourseRegistrations = (registrations: readonly CourseRegistrationDTO[]) => {
  const registrationsById = new Map<number, CourseRegistrationDTO>();

  registrations.forEach((registration) => {
    const existingRegistration = registrationsById.get(registration.crId);
    if (!existingRegistration) {
      registrationsById.set(registration.crId, registration);
      return;
    }

    registrationsById.set(
      registration.crId,
      mergeCourseRegistrationRecords(existingRegistration, registration),
    );
  });

  return [...registrationsById.values()];
};

const dedupeCourseRegistrationReceipts = (receipts: readonly CourseRegistrationReceiptDTO[]) => {
  const seenReceiptIds = new Set<number>();

  return receipts.filter((receipt) => {
    if (seenReceiptIds.has(receipt.crrId)) {
      return false;
    }

    seenReceiptIds.add(receipt.crrId);
    return true;
  });
};

const dedupeCourseRegistrationFollowUps = (followUps: readonly CourseRegistrationFollowUpDTO[]) => {
  const seenFollowUpIds = new Set<number>();

  return followUps.filter((entry) => {
    if (seenFollowUpIds.has(entry.crfId)) {
      return false;
    }

    seenFollowUpIds.add(entry.crfId);
    return true;
  });
};

export default function CourseRegistrationsAdminPage() {
  const qc = useQueryClient();
  const [searchParams, setSearchParams] = useSearchParams();
  const initialSlug = searchParams.get('slug') ?? '';
  const initialStatus = parseStatusFilter(searchParams.get('status'));
  const initialLimit = parsePositiveLimit(searchParams.get('limit'));
  const [slug, setSlug] = useState(initialSlug);
  const [status, setStatus] = useState<StatusFilter>(initialStatus);
  const [limit, setLimit] = useState(initialLimit);
  const [showAdvancedFilters, setShowAdvancedFilters] = useState(false);
  const [localSearch, setLocalSearch] = useState('');
  const [copyMessage, setCopyMessage] = useState<string | null>(null);
  const [pageFlash, setPageFlash] = useState<FlashState | null>(null);
  const [dossierFlash, setDossierFlash] = useState<FlashState | null>(null);
  const [selectedDossier, setSelectedDossier] = useState<DossierTarget | null>(null);
  const [hasUsedRowAction, setHasUsedRowAction] = useState(false);
  const [hasUsedFilterControl, setHasUsedFilterControl] = useState(false);
  const [showEmailHistory, setShowEmailHistory] = useState(false);
  const [statusMenuTarget, setStatusMenuTarget] = useState<{
    anchorEl: HTMLElement;
    reg: CourseRegistrationDTO;
  } | null>(null);
  const [receiptMenuTarget, setReceiptMenuTarget] = useState<{
    anchorEl: HTMLElement;
    receipt: CourseRegistrationReceiptDTO;
  } | null>(null);
  const [followUpMenuTarget, setFollowUpMenuTarget] = useState<{
    anchorEl: HTMLElement;
    entry: CourseRegistrationFollowUpDTO;
  } | null>(null);
  const [notesDraft, setNotesDraft] = useState('');
  const [showNotesComposer, setShowNotesComposer] = useState(false);
  const [receiptForm, setReceiptForm] = useState<ReceiptFormState>(emptyReceiptForm);
  const [followUpForm, setFollowUpForm] = useState<FollowUpFormState>(emptyFollowUpForm);
  const [showReceiptComposer, setShowReceiptComposer] = useState(false);
  const [showReceiptUrlField, setShowReceiptUrlField] = useState(false);
  const [showFollowUpUrlField, setShowFollowUpUrlField] = useState(false);
  const [showFollowUpDetails, setShowFollowUpDetails] = useState(false);
  const [showFollowUpComposer, setShowFollowUpComposer] = useState(false);
  const [markedPaidRegistrationId, setMarkedPaidRegistrationId] = useState<number | null>(null);
  const selectedSlug = slug.trim();

  const listQueryKey = useMemo(
    () => ['admin', 'course-registrations', { slug, status, limit }],
    [slug, status, limit],
  );

  const selectedDossierSlug = selectedDossier?.reg.crCourseSlug ?? '';
  const selectedDossierId = selectedDossier?.reg.crId ?? null;
  const dossierQueryKey = useMemo(
    () => ['admin', 'course-registration-dossier', selectedDossierSlug, selectedDossierId],
    [selectedDossierSlug, selectedDossierId],
  );

  const invalidateRegistrationViews = async (courseSlug: string, registrationId: number) => {
    await Promise.all([
      qc.invalidateQueries({ queryKey: ['admin', 'course-registrations'] }),
      qc.invalidateQueries({
        queryKey: ['admin', 'course-registration-dossier', courseSlug, registrationId],
      }),
      qc.invalidateQueries({ queryKey: ['admin', 'course-registration-email-events', registrationId] }),
    ]);
  };

  const cohortsQuery = useQuery({
    queryKey: ['admin', 'course-cohorts'],
    queryFn: () => Courses.listCohorts(),
    staleTime: 60_000,
  });

  const cohortLabelsBySlug = useMemo(() => {
    const bySlug = new Map<string, string>();
    for (const cohort of cohortsQuery.data ?? []) {
      const cohortSlug = cohort.ccSlug.trim();
      if (!cohortSlug || bySlug.has(cohortSlug)) continue;
      bySlug.set(cohortSlug, cohortOptionLabel(cohort));
    }
    if (selectedSlug && !bySlug.has(selectedSlug)) {
      bySlug.set(selectedSlug, selectedSlug);
    }
    return bySlug;
  }, [cohortsQuery.data, selectedSlug]);

  const cohortOptions = useMemo(
    () => Array.from(cohortLabelsBySlug.entries()).map(([value, label]) => ({ value, label })),
    [cohortLabelsBySlug],
  );
  const configuredCohortOptions = useMemo(() => {
    const options: { value: string; label: string }[] = [];
    const seenSlugs = new Set<string>();

    for (const cohort of cohortsQuery.data ?? []) {
      const cohortSlug = cohort.ccSlug.trim();
      if (!cohortSlug || seenSlugs.has(cohortSlug)) continue;
      seenSlugs.add(cohortSlug);
      options.push({ value: cohortSlug, label: cohortOptionLabel(cohort) });
    }

    return options;
  }, [cohortsQuery.data]);
  const singleAvailableCohort = useMemo(() => {
    if (cohortsQuery.isError || configuredCohortOptions.length !== 1) return null;
    const [onlyConfiguredCohort] = configuredCohortOptions;
    if (!onlyConfiguredCohort) return null;
    return !selectedSlug || selectedSlug === onlyConfiguredCohort.value ? onlyConfiguredCohort : null;
  }, [cohortsQuery.isError, configuredCohortOptions, selectedSlug]);
  const singleAvailableCohortLabel = singleAvailableCohort?.label ?? '';

  const activeCohortLabel = selectedSlug ? (cohortLabelsBySlug.get(selectedSlug) ?? selectedSlug) : '';

  const regsQuery = useQuery({
    queryKey: listQueryKey,
    queryFn: () =>
      Courses.listRegistrations({
        slug: slug.trim() || undefined,
        status: status === 'all' ? undefined : status,
        limit,
      }),
  });
  const registrations = useMemo(
    () => dedupeCourseRegistrations(regsQuery.data ?? []),
    [regsQuery.data],
  );

  const dossierQuery = useQuery<CourseRegistrationDossierDTO>({
    queryKey: dossierQueryKey,
    enabled: Boolean(selectedDossier && selectedDossierId != null),
    queryFn: () => {
      if (!selectedDossier || selectedDossierId == null) {
        return Promise.reject(new Error('No hay una inscripción seleccionada.'));
      }
      return Courses.getRegistrationDossier(selectedDossier.reg.crCourseSlug, selectedDossierId);
    },
    staleTime: 0,
  });
  const selectedDossierEmail = (
    dossierQuery.data?.crdRegistration.crId === selectedDossierId
      ? dossierQuery.data.crdRegistration.crEmail
      : selectedDossier?.reg.crEmail
  )?.trim() ?? '';

  const emailEventsQuery = useQuery<CourseEmailEventDTO[]>({
    queryKey: ['admin', 'course-registration-email-events', selectedDossierId],
    enabled: selectedDossierId != null && selectedDossier?.intent !== 'markPaid' && selectedDossierEmail !== '',
    queryFn: () => {
      if (selectedDossierId == null) return Promise.resolve([]);
      return Courses.listRegistrationEmails(selectedDossierId, 200);
    },
    staleTime: 30_000,
  });

  const statusCounts = useMemo(() => {
    const base = { total: 0, pending_payment: 0, paid: 0, cancelled: 0 };
    return registrations.reduce(
      (acc, reg) => {
        acc.total += 1;
        if (
          reg.crStatus === 'pending_payment'
          || reg.crStatus === 'paid'
          || reg.crStatus === 'cancelled'
        ) {
          acc[reg.crStatus] += 1;
        }
        return acc;
      },
      { ...base },
    );
  }, [registrations]);
  const hasVisibleRegistrations = registrations.length > 0;
  const visibleStatusFilters = useMemo<readonly StatusFilter[]>(() => {
    if (!hasVisibleRegistrations) return statusFilters;
    return statusFilters.filter((value) => value === 'all' || status === value || statusCounts[value] > 0);
  }, [hasVisibleRegistrations, status, statusCounts]);
  const actionableStatusFilters = useMemo(
    () => visibleStatusFilters.filter((value): value is Exclude<StatusFilter, 'all'> => value !== 'all'),
    [visibleStatusFilters],
  );
  const hasHiddenStatusFilters = visibleStatusFilters.length < statusFilters.length;
  const singleVisibleStatus = useMemo<Exclude<StatusFilter, 'all'> | null>(() => {
    if (!hasVisibleRegistrations) return null;
    const realStatuses = visibleStatusFilters.filter((value): value is Exclude<StatusFilter, 'all'> => value !== 'all');
    return realStatuses.length === 1 ? (realStatuses[0] ?? null) : null;
  }, [hasVisibleRegistrations, visibleStatusFilters]);
  const singleVisibleCustomStatus = useMemo(() => {
    if (!hasVisibleRegistrations || status !== 'all') return '';
    const statusesByKey = new Map<string, string>();

    registrations.forEach((reg) => {
      const trimmedStatus = reg.crStatus.trim();
      const statusKey = trimmedStatus.toLowerCase();
      if (!statusesByKey.has(statusKey)) {
        statusesByKey.set(statusKey, trimmedStatus);
      }
    });

    const [onlyStatus] = Array.from(statusesByKey.values());
    return statusesByKey.size === 1 && onlyStatus && !isRegistrationStatus(onlyStatus) ? onlyStatus : '';
  }, [hasVisibleRegistrations, registrations, status]);

  const showSingleStatusSummary = Boolean(singleVisibleStatus && status === 'all');
  const hasSlugFilter = slug.trim() !== '';
  const hasStatusFilter = status !== 'all';
  const hasRedundantSingleCohortFilter = Boolean(
    selectedSlug
    && singleAvailableCohort?.value === selectedSlug,
  );
  const hasEffectiveSlugFilter = hasSlugFilter && !hasRedundantSingleCohortFilter;
  const hasManualFilters = hasEffectiveSlugFilter || hasStatusFilter;
  const hasCustomLimit = limit !== DEFAULT_LIMIT;
  const hasCustomFilters = hasManualFilters || hasCustomLimit;
  const showCohortFilterUnavailableSummary = cohortsQuery.isError && hasVisibleRegistrations && !hasSlugFilter;
  const activeFilterSummary = useMemo(
    () => summarizeActiveFilters({ cohortLabel: activeCohortLabel, status, limit }),
    [activeCohortLabel, status, limit],
  );
  const combinedSingleChoiceSummary = singleAvailableCohortLabel && showSingleStatusSummary && singleVisibleStatus
    ? `${singleAvailableCohortLabel} · ${statusFilterLabels[singleVisibleStatus]}`
    : '';
  const loadedRegistrationCount = registrations.length;
  const localSearchTerm = localSearch.trim();
  const localSearchKey = localSearchTerm.toLocaleLowerCase('es');
  const hasLocalSearch = Boolean(localSearchKey);
  const searchedRegistrations = useMemo(() => {
    if (!localSearchKey) return registrations;
    return registrations.filter((reg) => {
      const courseSlug = reg.crCourseSlug.trim();
      const haystack = [
        reg.crFullName,
        reg.crEmail,
        reg.crPhoneE164,
        courseSlug,
        cohortLabelsBySlug.get(courseSlug),
        registrationStatusLabel(reg.crStatus),
        reg.crSource,
      ].join(' ').toLocaleLowerCase('es');
      return haystack.includes(localSearchKey);
    });
  }, [cohortLabelsBySlug, localSearchKey, registrations]);
  const registrationIdsRequiringActionDisambiguator = useMemo(
    () => getRegistrationIdsRequiringActionDisambiguator(searchedRegistrations),
    [searchedRegistrations],
  );
  const actionTargetLabelsByRegistrationId = useMemo(
    () => new Map(
      searchedRegistrations.map((reg) => [
        reg.crId,
        registrationActionTargetLabelWithContext(
          reg,
          registrationIdsRequiringActionDisambiguator.has(reg.crId),
        ),
      ]),
    ),
    [registrationIdsRequiringActionDisambiguator, searchedRegistrations],
  );
  const getActionTargetLabelForRegistration = (reg: CourseRegistrationDTO) =>
    actionTargetLabelsByRegistrationId.get(reg.crId) ?? registrationActionTargetLabel(reg);
  const singleVisibleCohortLabel = useMemo(() => {
    if (selectedSlug || searchedRegistrations.length < 2) return '';
    const uniqueCohortSlugs = Array.from(
      new Set(
        searchedRegistrations
          .map((reg) => reg.crCourseSlug.trim())
          .filter((value) => value !== ''),
      ),
    );
    if (uniqueCohortSlugs.length !== 1) return '';
    const cohortSlug = uniqueCohortSlugs[0];
    if (!cohortSlug) return '';
    return cohortLabelsBySlug.get(cohortSlug) ?? cohortSlug;
  }, [cohortLabelsBySlug, searchedRegistrations, selectedSlug]);
  const singleVisibleSourceLabel = useMemo(() => {
    if (searchedRegistrations.length === 0) return '';
    const sourceLabelsByKey = new Map<string, string>();

    searchedRegistrations.forEach((reg) => {
      const sourceLabel = registrationSourceLabel(reg.crSource);
      const sourceKey = normalizeRegistrationSourceKey(sourceLabel);
      if (!sourceLabelsByKey.has(sourceKey)) {
        sourceLabelsByKey.set(sourceKey, sourceLabel);
      }
    });

    const [onlySourceLabel] = Array.from(sourceLabelsByKey.values());
    return sourceLabelsByKey.size === 1 && onlySourceLabel ? onlySourceLabel : '';
  }, [searchedRegistrations]);
  const hasNamedVisibleSource = Boolean(
    singleVisibleSourceLabel
    && singleVisibleSourceLabel !== 'Sin fuente'
    && !isDefaultPublicFormSource(singleVisibleSourceLabel),
  );
  const sharedVisibleSourceSummary = hasNamedVisibleSource
    ? `Mostrando una sola fuente: ${singleVisibleSourceLabel}.`
    : '';
  const showEmptyLocalSearchResults = hasLocalSearch
    && loadedRegistrationCount > 0
    && searchedRegistrations.length === 0;
  const showLocalSearchControl = loadedRegistrationCount >= MIN_LOCAL_SEARCH_REGISTRATIONS || Boolean(localSearchKey);
  const localSearchHelperText = localSearchKey
    ? showEmptyLocalSearchResults
      ? `Búsqueda local en el lote cargado (${formatRegistrationCountLabel(loadedRegistrationCount)}).`
      : formatLocalSearchResultSummary(searchedRegistrations.length, loadedRegistrationCount)
    : 'Busca dentro de este lote sin cambiar los filtros de cohorte o estado.';
  const visibleRegistrationsSummary = hasCustomFilters
    ? `Mostrando ${formatRegistrationCountLabel(loadedRegistrationCount)}.`
    : `Mostrando ${formatRegistrationCountLabel(loadedRegistrationCount)} en esta vista.`;
  const combinedSingleChoiceLimitSummary = combinedSingleChoiceSummary && limit !== DEFAULT_LIMIT
    ? `Límite actual: hasta ${limit} inscripci${limit === 1 ? 'ón' : 'ones'}.`
    : '';
  const summarizedVisibleSourceLabel = hasNamedVisibleSource
    ? `Fuente visible: ${singleVisibleSourceLabel}.`
    : '';
  const combinedSingleChoiceSourceSummary = combinedSingleChoiceSummary
    ? summarizedVisibleSourceLabel
    : '';
  const showTinyDefaultCountInCurrentView = Boolean(combinedSingleChoiceSummary)
    && !hasCustomFilters
    && loadedRegistrationCount > 1
    && loadedRegistrationCount < MIN_DEFAULT_CSV_EXPORT_ROWS
    && !copyMessage;
  const combinedSingleChoiceCountSummary = showTinyDefaultCountInCurrentView
    ? visibleRegistrationsSummary
    : '';
  const combinedSingleChoiceContextSummary = [
    combinedSingleChoiceSourceSummary,
    combinedSingleChoiceLimitSummary,
    combinedSingleChoiceCountSummary,
  ].filter(Boolean).join(' ');
  const standaloneSingleChoiceSourceSummary = !combinedSingleChoiceSummary && (singleAvailableCohortLabel || showSingleStatusSummary)
    ? summarizedVisibleSourceLabel
    : '';
  const resetViewLabel = getResetViewLabel({
    hasCustomLimit,
    hasSlugFilter: hasEffectiveSlugFilter,
    hasStatusFilter,
  });
  const showCohortSelect = !combinedSingleChoiceSummary && !singleAvailableCohortLabel;
  const cohortFilterCanSelfReset = showCohortSelect && hasSlugFilter && !hasStatusFilter && !hasCustomLimit;
  const filteredEmptyStateRecoveryHint = hasManualFilters
    ? 'Revisa los filtros o restablece la vista si esperabas resultados.'
    : 'Usa refrescar si esperabas resultados.';
  const filteredEmptyStateScope = hasManualFilters
    ? hasCustomLimit
      ? 'en la vista actual'
      : 'con los filtros actuales'
    : 'con el límite actual';
  const filteredEmptyStateMessage = activeFilterSummary
    ? `No hay inscripciones ${filteredEmptyStateScope}: ${activeFilterSummary}. ${filteredEmptyStateRecoveryHint}`
    : `No hay inscripciones ${filteredEmptyStateScope}. ${filteredEmptyStateRecoveryHint}`;
  const canCopyCsv = searchedRegistrations.length > 1
    && (hasCustomFilters || loadedRegistrationCount >= MIN_DEFAULT_CSV_EXPORT_ROWS);
  const hideTinyDefaultListRowDates = !hasCustomFilters && loadedRegistrationCount < MIN_DEFAULT_CSV_EXPORT_ROWS;
  const shouldShowSharedCohortSummary = !hasCustomFilters && Boolean(singleVisibleCohortLabel) && !singleAvailableCohortLabel;
  const hasSharedVisibleSource = Boolean(singleVisibleSourceLabel);
  const shouldShowSharedSourceSummary = hasNamedVisibleSource
    && !combinedSingleChoiceSourceSummary
    && !standaloneSingleChoiceSourceSummary;
  const sharedVisibleCreatedAtLabel = useMemo(() => {
    if (registrations.length < 2) return '';
    const createdLabels = registrations.map((reg) => formatOptionalDate(reg.crCreatedAt));
    if (createdLabels.some((label) => label === '')) return '';
    const [firstLabel] = createdLabels;
    return firstLabel && createdLabels.every((label) => label === firstLabel) ? firstLabel : '';
  }, [registrations]);
  const shouldHideSharedCreatedAtContext = Boolean(sharedVisibleCreatedAtLabel) && hasCustomFilters;
  const allVisibleRegistrationsHaveNotes = loadedRegistrationCount > 1
    && registrations.every((reg) => Boolean(reg.crAdminNotes?.trim()));
  const sharedVisibleNotesSummary = allVisibleRegistrationsHaveNotes
    ? 'Notas internas en todas las inscripciones visibles.'
    : '';
  const sharedListContextSummaries = [
    shouldShowSharedCohortSummary ? `Mostrando una sola cohorte: ${singleVisibleCohortLabel}.` : '',
    shouldShowSharedSourceSummary ? `Fuente visible: ${singleVisibleSourceLabel}.` : '',
    sharedVisibleNotesSummary,
  ].filter(Boolean);
  const combinedSharedListContextSummary = sharedListContextSummaries.length > 1
    ? sharedListContextSummaries.join(' ')
    : '';
  const statusAlreadyVisibleInFilterStrip = hasStatusFilter && !showSingleStatusSummary;
  const showSingleCustomStatusSummary = Boolean(singleVisibleCustomStatus) && actionableStatusFilters.length === 0;
  const showFilterOnboardingCopy = !hasUsedRowAction && !hasUsedFilterControl;
  const copyCsvButtonLabel = `Copiar CSV (${formatRowCountLabel(searchedRegistrations.length)})`;
  const showUtilityCountSummary = !hasLocalSearch
    && !canCopyCsv
    && !showTinyDefaultCountInCurrentView
    && (loadedRegistrationCount > 1 || Boolean(copyMessage));
  const viewHitsCurrentLimit = hasVisibleRegistrations && loadedRegistrationCount >= limit;
  const standaloneReachedListLimitSummary = !hasCustomFilters && viewHitsCurrentLimit
    ? buildReachedListLimitSummary(limit)
    : '';
  const showAdvancedLimitControl = viewHitsCurrentLimit || limit !== DEFAULT_LIMIT;
  const showSingleResultWithoutHiddenLimit = loadedRegistrationCount === 1 && !showAdvancedLimitControl;
  const showSingleResultWithOnlyPassiveFilterContext = showSingleResultWithoutHiddenLimit
    && !hasCustomFilters
    && !hasSlugFilter
    && Boolean(combinedSingleChoiceSummary);
  const showSingleStatusSummaryInPageChrome = showSingleStatusSummary && !showSingleResultWithOnlyPassiveFilterContext;
  const useCompactStatusActionLabel = showSingleStatusSummaryInPageChrome
    || statusAlreadyVisibleInFilterStrip
    || showSingleCustomStatusSummary;
  const dossierIdentityTargetLabel = registrationIdentityTargetLabel(registrations);
  const dossierScopeHint = useCompactStatusActionLabel
    ? buildCompactDossierScopeHint(dossierIdentityTargetLabel)
    : buildDossierOnlyScopeHint(dossierIdentityTargetLabel);
  const showDossierScopeHint = loadedRegistrationCount > 0 && !hasUsedRowAction && !hasUsedFilterControl;
  const showFirstRunFilterHelper = showFilterOnboardingCopy && !showSingleResultWithoutHiddenLimit;
  const visibleActiveFilterSummary = useMemo(() => {
    const parts: string[] = [];
    const cohortAlreadyExplained = Boolean(combinedSingleChoiceSummary || singleAvailableCohortLabel);
    const statusAlreadyExplained = Boolean(
      combinedSingleChoiceSummary || showSingleStatusSummary || statusAlreadyVisibleInFilterStrip,
    );
    const limitAlreadyExplained = Boolean(combinedSingleChoiceLimitSummary);
    if (activeCohortLabel && !cohortAlreadyExplained) parts.push(`cohorte ${activeCohortLabel}`);
    if (status !== 'all' && !statusAlreadyExplained) parts.push(`estado ${statusFilterLabels[status].toLowerCase()}`);
    if (limit !== DEFAULT_LIMIT && !limitAlreadyExplained) parts.push(`límite ${limit}`);
    return parts.join(' · ');
  }, [
    activeCohortLabel,
    combinedSingleChoiceSummary,
    combinedSingleChoiceLimitSummary,
    limit,
    showSingleStatusSummary,
    singleAvailableCohortLabel,
    statusAlreadyVisibleInFilterStrip,
    status,
  ]);
  const activeViewSummaryMessage = useMemo(() => {
    if (!hasCustomFilters || !hasVisibleRegistrations) return '';
    if (cohortFilterCanSelfReset) return '';
    if (!hasManualFilters && hasCustomLimit) {
      return combinedSingleChoiceLimitSummary
        ? ''
        : `Límite actual: hasta ${limit} inscripci${limit === 1 ? 'ón' : 'ones'}.`;
    }
    if (!visibleActiveFilterSummary) return '';
    return `Vista filtrada: ${visibleActiveFilterSummary}.`;
  }, [
    combinedSingleChoiceLimitSummary,
    cohortFilterCanSelfReset,
    hasCustomFilters,
    hasCustomLimit,
    hasManualFilters,
    hasVisibleRegistrations,
    limit,
    visibleActiveFilterSummary,
  ]);
  const showInlineSummaryResetAction = Boolean(
    combinedSingleChoiceSummary
    && hasCustomFilters
    && hasVisibleRegistrations,
  );
  const showInlineSingleChoiceLimitToggle = showAdvancedLimitControl
    && Boolean(combinedSingleChoiceSummary || singleAvailableCohortLabel || showSingleStatusSummary);
  const statusFilterCanSelfReset = statusAlreadyVisibleInFilterStrip && !hasEffectiveSlugFilter && !hasCustomLimit;
  const showFilteredResetAction = !showInlineSummaryResetAction && !cohortFilterCanSelfReset && !statusFilterCanSelfReset;
  const showFilteredEmptyStateResetAction = hasManualFilters;
  const showFilteredEmptyStateRefreshAction = !hasManualFilters;
  const filteredUtilitySummaryMessage = useMemo(
    () => [
      activeViewSummaryMessage,
      showUtilityCountSummary ? visibleRegistrationsSummary : '',
    ].filter(Boolean).join(' '),
    [activeViewSummaryMessage, showUtilityCountSummary, visibleRegistrationsSummary],
  );
  const standaloneUtilitySummaryMessage = useMemo(
    () => [
      showUtilityCountSummary ? visibleRegistrationsSummary : '',
      standaloneReachedListLimitSummary,
    ].filter(Boolean).join(' '),
    [showUtilityCountSummary, standaloneReachedListLimitSummary, visibleRegistrationsSummary],
  );
  const showFilteredEmptyState = !regsQuery.isLoading
    && !regsQuery.isError
    && !cohortsQuery.isError
    && hasCustomFilters
    && !hasVisibleRegistrations;
  const showInitialCohortErrorState = !regsQuery.isLoading
    && !regsQuery.isError
    && cohortsQuery.isError
    && !hasCustomFilters
    && !hasVisibleRegistrations;
  const showRegistrationErrorInlineRetry = regsQuery.isError && !hasVisibleRegistrations;
  const showHeaderRefreshAction = !showInitialCohortErrorState
    && !showRegistrationErrorInlineRetry
    && (regsQuery.isError || cohortsQuery.isError);
  const headerRefreshLabel = cohortsQuery.isError
    ? regsQuery.isError
      ? 'Reintentar datos'
      : 'Reintentar cohortes'
    : regsQuery.isError
      ? 'Reintentar inscripciones'
      : 'Refrescar lista';
  const registrationErrorRetryLabel = cohortsQuery.isError
    ? 'Reintentar datos'
    : 'Reintentar inscripciones';
  const showFilteredUtilityRow = hasCustomFilters
    && hasVisibleRegistrations
    && (
      Boolean(activeViewSummaryMessage)
      || showUtilityCountSummary
      || canCopyCsv
      || Boolean(copyMessage)
      || showFilteredResetAction
    );
  const showStandaloneListUtilityRow = !hasCustomFilters
    && hasVisibleRegistrations
    && (
      Boolean(standaloneUtilitySummaryMessage)
      || canCopyCsv
      || Boolean(copyMessage)
    );
  const showInitialFilterGuidance = !regsQuery.isLoading
    && !regsQuery.isError
    && !cohortsQuery.isError
    && !cohortsQuery.isLoading
    && !hasCustomFilters
    && !hasVisibleRegistrations;
  const showInitialCohortResolutionState = !regsQuery.isLoading
    && !regsQuery.isError
    && cohortsQuery.isLoading
    && !hasCustomFilters
    && !hasVisibleRegistrations;
  const showInitialRegistrationLoading = regsQuery.isLoading && !regsQuery.data;
  const showRegistrationFilterPanel = !showInitialRegistrationLoading
    && !showInitialCohortResolutionState
    && !showInitialCohortErrorState
    && !showFilteredEmptyState
    && !showSingleResultWithOnlyPassiveFilterContext
    && (!regsQuery.isError || hasCustomFilters);
  const limitToggleLabel = showAdvancedFilters
    ? 'Ocultar límite'
    : limit !== DEFAULT_LIMIT
      ? `Ajustar límite (${limit})`
      : 'Ajustar límite';
  const singleAvailableCohortHelperText = showAdvancedLimitControl
    ? 'Cohorte única por ahora. Usa Estado o Ajustar límite para cambiar la vista.'
    : 'Cohorte única por ahora. Usa Estado para cambiar la vista.';
  const singleVisibleStatusHelperText = showAdvancedLimitControl
    ? 'Estado único en esta vista. Usa cohorte o Ajustar límite para cambiar la vista.'
    : 'Estado único en esta vista. Usa cohorte para cambiar la vista.';
  const filtersHelpText = buildAutomaticFilterHelpText({
    combinedSingleChoiceSummary,
    hasVisibleRegistrations,
    showAdvancedLimitControl,
    showSingleStatusSummary,
    singleAvailableCohortLabel,
  });
  const statusFilterHelperText = statusFilterCanSelfReset
    ? activeStatusFilterHelperText
    : hasHiddenStatusFilters
      ? 'Solo aparecen estados con inscripciones en esta vista.'
      : '';
  const showStatusFilterCaption = !(statusFilterCanSelfReset && actionableStatusFilters.length === 1);
  const statusFilterGroupLabel = statusFilterCanSelfReset
    ? `Filtro de estado activo: ${statusFilterLabels[status]}`
    : 'Filtros de estado de inscripciones';
  const showCustomStatusFilterUnavailableSummary = hasVisibleRegistrations
    && !showSingleStatusSummary
    && actionableStatusFilters.length === 0;
  const combinedSingleChoiceHelperText = showAdvancedLimitControl
    ? 'Vista única por ahora: una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.'
    : 'Vista única por ahora: una cohorte y un estado.';
  const canReviewSystemEmails = selectedDossier?.intent !== 'markPaid';
  const hasSystemEmailHistory = canReviewSystemEmails && (emailEventsQuery.data?.length ?? 0) > 0;
  const showSystemEmailHistoryAction = canReviewSystemEmails
    && (showEmailHistory || hasSystemEmailHistory || emailEventsQuery.isError);
  const hasMultipleAvailableCohorts = !cohortsQuery.isError && configuredCohortOptions.length > 1;
  const initialEmptyStateMessage = singleAvailableCohort
    ? buildSingleCohortInitialEmptyStateMessage(singleAvailableCohort.label)
    : hasMultipleAvailableCohorts
      ? initialEmptyStateMultiCohortMessage
    : initialEmptyStateConfigMessage;
  const initialEmptyStateAction = singleAvailableCohort
    ? {
      label: initialEmptyStateFormActionLabel,
      to: `/inscripcion/${encodeURIComponent(singleAvailableCohort.value)}`,
    }
    : {
      label: hasMultipleAvailableCohorts
        ? initialEmptyStateMultiCohortActionLabel
        : initialEmptyStateConfigActionLabel,
      to: '/configuracion/cursos',
    };

  const resetReceiptComposer = (open = false) => {
    setReceiptForm(emptyReceiptForm());
    setShowReceiptUrlField(false);
    setShowReceiptComposer(open);
  };

  const resetFollowUpComposer = () => {
    setFollowUpForm(emptyFollowUpForm());
    setShowFollowUpUrlField(false);
    setShowFollowUpDetails(false);
    setShowFollowUpComposer(false);
  };

  const updateStatusMutation = useMutation({
    mutationFn: (args: { id: number; courseSlug: string; newStatus: Exclude<StatusFilter, 'all'> }) =>
      Courses.updateStatus(args.courseSlug, args.id, { status: args.newStatus }),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  const updateNotesMutation = useMutation({
    mutationFn: (args: { id: number; courseSlug: string; notes: string | null }) =>
      Courses.updateRegistrationNotes(args.courseSlug, args.id, { notes: args.notes }),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  const createReceiptMutation = useMutation({
    mutationFn: (args: { id: number; courseSlug: string; payload: { fileUrl: string; fileName?: string | null; notes?: string | null } }) =>
      Courses.createReceipt(args.courseSlug, args.id, args.payload),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  const updateReceiptMutation = useMutation({
    mutationFn: (args: {
      id: number;
      courseSlug: string;
      receiptId: number;
      payload: { fileUrl?: string | null; fileName?: string | null; notes?: string | null };
    }) => Courses.updateReceipt(args.courseSlug, args.id, args.receiptId, args.payload),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  const deleteReceiptMutation = useMutation({
    mutationFn: (args: { id: number; courseSlug: string; receiptId: number }) =>
      Courses.deleteReceipt(args.courseSlug, args.id, args.receiptId),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  const createFollowUpMutation = useMutation({
    mutationFn: (args: {
      id: number;
      courseSlug: string;
      payload: {
        entryType?: string | null;
        subject?: string | null;
        notes: string;
        attachmentUrl?: string | null;
        attachmentName?: string | null;
        nextFollowUpAt?: string | null;
      };
    }) => Courses.createFollowUp(args.courseSlug, args.id, args.payload),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  const updateFollowUpMutation = useMutation({
    mutationFn: (args: {
      id: number;
      courseSlug: string;
      followUpId: number;
      payload: {
        entryType?: string | null;
        subject?: string | null;
        notes?: string | null;
        attachmentUrl?: string | null;
        attachmentName?: string | null;
        nextFollowUpAt?: string | null;
      };
    }) => Courses.updateFollowUp(args.courseSlug, args.id, args.followUpId, args.payload),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  const deleteFollowUpMutation = useMutation({
    mutationFn: (args: { id: number; courseSlug: string; followUpId: number }) =>
      Courses.deleteFollowUp(args.courseSlug, args.id, args.followUpId),
    onSuccess: async (_, variables) => {
      await invalidateRegistrationViews(variables.courseSlug, variables.id);
    },
  });

  useEffect(() => {
    const params = new URLSearchParams();
    if (slug.trim()) params.set('slug', slug.trim());
    if (status !== 'all') params.set('status', status);
    if (limit && limit !== DEFAULT_LIMIT) params.set('limit', String(limit));
    setSearchParams(params, { replace: true });
  }, [slug, status, limit, setSearchParams]);

  useEffect(() => {
    if (!selectedDossier) {
      setDossierFlash(null);
      setMarkedPaidRegistrationId(null);
      setShowEmailHistory(false);
      setReceiptMenuTarget(null);
      setFollowUpMenuTarget(null);
      setNotesDraft('');
      setShowNotesComposer(false);
      setReceiptForm(emptyReceiptForm());
      setShowReceiptUrlField(false);
      setShowReceiptComposer(false);
      setFollowUpForm(emptyFollowUpForm());
      setShowFollowUpUrlField(false);
      setShowFollowUpDetails(false);
      setShowFollowUpComposer(false);
      return;
    }
    setMarkedPaidRegistrationId(null);
    setShowEmailHistory(false);
    setReceiptMenuTarget(null);
    setNotesDraft(selectedDossier.reg.crAdminNotes ?? '');
    setShowNotesComposer(false);
    setReceiptForm(emptyReceiptForm());
    setShowReceiptUrlField(false);
    setShowReceiptComposer(selectedDossier.intent === 'markPaid');
    setFollowUpForm(emptyFollowUpForm());
    setShowFollowUpUrlField(false);
    setShowFollowUpDetails(false);
    setShowFollowUpComposer(false);
    setDossierFlash(null);
  }, [selectedDossier]);

  useEffect(() => {
    setNotesDraft(dossierQuery.data?.crdRegistration.crAdminNotes ?? '');
  }, [dossierQuery.data?.crdRegistration.crId, dossierQuery.data?.crdRegistration.crAdminNotes]);

  const handleRefresh = () => {
    const shouldRefreshCohorts = cohortsQuery.isError;
    const shouldRefreshRegistrations = regsQuery.isError || !shouldRefreshCohorts;
    const requests: Promise<unknown>[] = [];

    if (shouldRefreshRegistrations) {
      requests.push(qc.invalidateQueries({ queryKey: ['admin', 'course-registrations'] }));
    }

    if (shouldRefreshCohorts) {
      requests.push(qc.invalidateQueries({ queryKey: ['admin', 'course-cohorts'] }));
    }

    void Promise.all(requests);
  };

  const handleRefreshDossier = () => {
    if (!selectedDossier) return;
    const requests: Promise<unknown>[] = [dossierQuery.refetch()];
    if (selectedDossierId != null && selectedDossier.intent !== 'markPaid' && showEmailHistory) {
      requests.push(emailEventsQuery.refetch());
    }
    void Promise.all(requests);
  };

  const handleCopyCsv = async () => {
    if (searchedRegistrations.length < 2) return;
    const header = ['id', 'slug', 'nombre', 'email', 'estado', 'creado'];
    const rows = searchedRegistrations.map((reg) => [
      reg.crId,
      reg.crCourseSlug,
      reg.crFullName ?? '',
      reg.crEmail ?? '',
      reg.crStatus,
      reg.crCreatedAt,
    ]);
    const csv = [header, ...rows]
      .map((r) => r.map((cell) => `"${String(cell ?? '').replace(/"/g, '""')}"`).join(','))
      .join('\n');
    try {
      await navigator.clipboard.writeText(csv);
      setCopyMessage(`Copiado CSV (${formatRowCountLabel(rows.length)})`);
      setTimeout(() => setCopyMessage(null), 2000);
    } catch {
      setCopyMessage('No se pudo copiar el CSV');
      setTimeout(() => setCopyMessage(null), 2000);
    }
  };

  const handleResetFilters = () => {
    setHasUsedFilterControl(true);
    setLocalSearch('');
    setSlug('');
    setStatus('all');
    setLimit(DEFAULT_LIMIT);
    setShowAdvancedFilters(false);
  };

  const handleToggleAdvancedFilters = () => {
    setHasUsedFilterControl(true);
    setShowAdvancedFilters((current) => !current);
  };

  const handleOpenStatusMenu = (anchorEl: HTMLElement, reg: CourseRegistrationDTO) => {
    setHasUsedRowAction(true);
    setStatusMenuTarget({ anchorEl, reg });
  };

  const handleCloseStatusMenu = () => {
    setStatusMenuTarget(null);
  };

  const handleOpenReceiptMenu = (anchorEl: HTMLElement, receipt: CourseRegistrationReceiptDTO) => {
    setReceiptMenuTarget({ anchorEl, receipt });
  };

  const handleCloseReceiptMenu = () => {
    setReceiptMenuTarget(null);
  };

  const handleOpenFollowUpMenu = (anchorEl: HTMLElement, entry: CourseRegistrationFollowUpDTO) => {
    setFollowUpMenuTarget({ anchorEl, entry });
  };

  const handleCloseFollowUpMenu = () => {
    setFollowUpMenuTarget(null);
  };

  const handleQuickStatus = (reg: CourseRegistrationDTO, newStatus: Exclude<StatusFilter, 'all'>) => {
    setPageFlash(null);
    void updateStatusMutation
      .mutateAsync({ id: reg.crId, courseSlug: reg.crCourseSlug, newStatus })
      .then(() => {
        setPageFlash({
          severity: 'success',
          message: `Estado actualizado para ${getActionTargetLabelForRegistration(reg)}.`,
        });
      })
      .catch((err: Error) => {
        setPageFlash({ severity: 'error', message: err.message });
      });
  };

  const handleOpenDossier = (reg: CourseRegistrationDTO, intent: DossierIntent) => {
    setHasUsedRowAction(true);
    setSelectedDossier({ reg, intent });
  };

  const getPersistedNotesValue = () =>
    dossierQuery.data?.crdRegistration.crAdminNotes ?? selectedDossier?.reg.crAdminNotes ?? '';

  const handleSaveNotes = () => {
    if (!selectedDossier || selectedDossierId == null) return;
    setDossierFlash(null);
    void updateNotesMutation
      .mutateAsync({
        id: selectedDossierId,
        courseSlug: selectedDossier.reg.crCourseSlug,
        notes: trimToNull(notesDraft),
      })
      .then(() => {
        setDossierFlash({ severity: 'success', message: 'Notas guardadas.' });
      })
      .catch((err: Error) => {
        setDossierFlash({ severity: 'error', message: err.message });
      });
  };

  const handleOpenNotesComposer = () => {
    setNotesDraft(getPersistedNotesValue());
    setShowNotesComposer(true);
    setDossierFlash(null);
  };

  const handleHideNotesComposer = () => {
    setNotesDraft(getPersistedNotesValue());
    setShowNotesComposer(false);
    setDossierFlash(null);
  };

  const handleReceiptUpload = (files: DriveFileInfo[]) => {
    const file = files[0];
    if (!file) return;
    setReceiptForm((prev) => ({
      ...prev,
      fileUrl: file.publicUrl ?? file.webContentLink ?? file.webViewLink ?? '',
      fileName: file.name,
    }));
    setDossierFlash(null);
  };

  const handleFollowUpUpload = (files: DriveFileInfo[]) => {
    const file = files[0];
    if (!file) return;
    setFollowUpForm((prev) => ({
      ...prev,
      attachmentUrl: file.publicUrl ?? file.webContentLink ?? file.webViewLink ?? '',
      attachmentName: file.name,
    }));
    setDossierFlash(null);
  };

  const handleSubmitReceipt = () => {
    if (!selectedDossier || selectedDossierId == null) return;
    const fileUrl = trimToNull(receiptForm.fileUrl);
    if (!fileUrl) {
      setDossierFlash({ severity: 'error', message: 'Sube un comprobante o pega una URL válida.' });
      return;
    }
    const payload = {
      fileUrl,
      fileName: trimToNull(receiptForm.fileName),
      notes: trimToNull(receiptForm.notes),
    };
    const action =
      receiptForm.editingId == null
        ? createReceiptMutation.mutateAsync({
            id: selectedDossierId,
            courseSlug: selectedDossier.reg.crCourseSlug,
            payload,
          })
        : updateReceiptMutation.mutateAsync({
            id: selectedDossierId,
            courseSlug: selectedDossier.reg.crCourseSlug,
            receiptId: receiptForm.editingId,
            payload,
          });
    setDossierFlash(null);
    void action
      .then(() => {
        resetReceiptComposer();
        setDossierFlash({
          severity: 'success',
          message: receiptForm.editingId == null ? 'Comprobante guardado.' : 'Comprobante actualizado.',
        });
      })
      .catch((err: Error) => {
        setDossierFlash({ severity: 'error', message: err.message });
      });
  };

  const handleEditReceipt = (receipt: CourseRegistrationReceiptDTO) => {
    handleCloseReceiptMenu();
    setShowReceiptComposer(true);
    setReceiptForm({
      editingId: receipt.crrId,
      fileUrl: receipt.crrFileUrl,
      fileName: receipt.crrFileName ?? '',
      notes: receipt.crrNotes ?? '',
    });
    setShowReceiptUrlField(true);
  };

  const handleDeleteReceipt = (receipt: CourseRegistrationReceiptDTO) => {
    if (!selectedDossier || selectedDossierId == null) return;
    handleCloseReceiptMenu();
    if (!window.confirm('¿Eliminar este comprobante?')) return;
    setDossierFlash(null);
    void deleteReceiptMutation
      .mutateAsync({
        id: selectedDossierId,
        courseSlug: selectedDossier.reg.crCourseSlug,
        receiptId: receipt.crrId,
      })
      .then(() => {
        if (receiptForm.editingId === receipt.crrId) {
          resetReceiptComposer();
        }
        setDossierFlash({ severity: 'success', message: 'Comprobante eliminado.' });
      })
      .catch((err: Error) => {
        setDossierFlash({ severity: 'error', message: err.message });
      });
  };

  const handleSubmitFollowUp = () => {
    if (!selectedDossier || selectedDossierId == null) return;
    const notes = trimToNull(followUpForm.notes);
    if (!notes) {
      setDossierFlash({ severity: 'error', message: 'Agrega una nota para registrar el seguimiento.' });
      return;
    }
    const payload = {
      entryType: trimToNull(followUpForm.entryType),
      subject: trimToNull(followUpForm.subject),
      notes,
      attachmentUrl: trimToNull(followUpForm.attachmentUrl),
      attachmentName: trimToNull(followUpForm.attachmentName),
      nextFollowUpAt: toIsoStringFromLocalDateTime(followUpForm.nextFollowUpAt),
    };
    const action =
      followUpForm.editingId == null
        ? createFollowUpMutation.mutateAsync({
            id: selectedDossierId,
            courseSlug: selectedDossier.reg.crCourseSlug,
            payload,
          })
        : updateFollowUpMutation.mutateAsync({
            id: selectedDossierId,
            courseSlug: selectedDossier.reg.crCourseSlug,
            followUpId: followUpForm.editingId,
            payload,
          });
    setDossierFlash(null);
    void action
      .then(() => {
        resetFollowUpComposer();
        setDossierFlash({
          severity: 'success',
          message: followUpForm.editingId == null ? 'Seguimiento guardado.' : 'Seguimiento actualizado.',
        });
      })
      .catch((err: Error) => {
        setDossierFlash({ severity: 'error', message: err.message });
      });
  };

  const handleEditFollowUp = (entry: CourseRegistrationFollowUpDTO) => {
    handleCloseFollowUpMenu();
    setShowFollowUpComposer(true);
    setShowFollowUpDetails(true);
    setFollowUpForm({
      editingId: entry.crfId,
      entryType: entry.crfEntryType,
      subject: entry.crfSubject ?? '',
      notes: entry.crfNotes,
      attachmentUrl: entry.crfAttachmentUrl ?? '',
      attachmentName: entry.crfAttachmentName ?? '',
      nextFollowUpAt: toLocalDateTimeInputValue(entry.crfNextFollowUpAt),
    });
    setShowFollowUpUrlField(Boolean(entry.crfAttachmentUrl));
  };

  const handleDeleteFollowUp = (entry: CourseRegistrationFollowUpDTO) => {
    if (!selectedDossier || selectedDossierId == null) return;
    handleCloseFollowUpMenu();
    if (!window.confirm('¿Eliminar esta entrada de seguimiento?')) return;
    setDossierFlash(null);
    void deleteFollowUpMutation
      .mutateAsync({
        id: selectedDossierId,
        courseSlug: selectedDossier.reg.crCourseSlug,
        followUpId: entry.crfId,
      })
      .then(() => {
        if (followUpForm.editingId === entry.crfId) {
          resetFollowUpComposer();
        }
        setDossierFlash({ severity: 'success', message: 'Seguimiento eliminado.' });
      })
      .catch((err: Error) => {
        setDossierFlash({ severity: 'error', message: err.message });
      });
  };

  const handleMarkPaidFromDossier = () => {
    if (!selectedDossier || selectedDossierId == null) return;
    setDossierFlash(null);
    void updateStatusMutation
      .mutateAsync({
        id: selectedDossierId,
        courseSlug: selectedDossier.reg.crCourseSlug,
        newStatus: 'paid',
      })
      .then(() => {
        setMarkedPaidRegistrationId(selectedDossierId);
        setDossierFlash({ severity: 'success', message: markPaidSuccessMessage });
      })
      .catch((err: Error) => {
        setDossierFlash({ severity: 'error', message: err.message });
      });
  };

  const dossierData = dossierQuery.data;
  const activeRegistration = dossierData?.crdRegistration ?? selectedDossier?.reg ?? null;
  const receipts = dedupeCourseRegistrationReceipts(dossierData?.crdReceipts ?? []);
  const followUps = dedupeCourseRegistrationFollowUps(dossierData?.crdFollowUps ?? []);
  const persistedNotes = trimToNull(getPersistedNotesValue());
  const hasSavedNotes = Boolean(persistedNotes);
  const hasNotesDraftChanges = trimToNull(notesDraft) !== persistedNotes;
  const canMarkPaid = dossierData?.crdCanMarkPaid ?? false;
  const isMarkPaidIntent = selectedDossier?.intent === 'markPaid';
  const hasMarkedPaidInCurrentDossier =
    isMarkPaidIntent && selectedDossierId != null && markedPaidRegistrationId === selectedDossierId;
  const showMarkPaidAction = canMarkPaid && !hasMarkedPaidInCurrentDossier;
  const showInlineEmptyNotesAction = !isMarkPaidIntent && !showNotesComposer && !hasSavedNotes;
  const showInlineEmptyFollowUpAction = !isMarkPaidIntent && !showFollowUpComposer && followUps.length === 0;
  const hasPrimaryDossierAction = showMarkPaidAction || showSystemEmailHistoryAction;
  const showDossierActionRow = hasPrimaryDossierAction || showInlineEmptyNotesAction || showInlineEmptyFollowUpAction;
  const hasReceipts = receipts.length > 0;
  const showEvidenceOnlyEmptyReceiptCopy = activeRegistration?.crStatus === 'paid'
    || activeRegistration?.crStatus === 'cancelled';
  const emptyReceiptReviewMessage = showEvidenceOnlyEmptyReceiptCopy
    ? emptyReceiptEvidenceAlertMessage
    : emptyReceiptAlertMessage;
  const showCompactMarkPaidNotesState = selectedDossier?.intent === 'markPaid'
    && !showNotesComposer
    && !hasSavedNotes;
  const showEmptyNotesState = !showNotesComposer && !hasSavedNotes;
  const emptyNotesSectionHelperText = showCompactMarkPaidNotesState
    ? markPaidEmptyNotesHelperText
    : emptyNotesHelperText;
  const showReceiptCountChip = receipts.length > 1;
  const canSubmitReceipt = Boolean(trimToNull(receiptForm.fileUrl));
  const hasReceiptMetadataDraft = Boolean(trimToNull(receiptForm.fileName)) || Boolean(trimToNull(receiptForm.notes));
  const receiptSectionHelpText = (
    selectedDossier?.intent === 'markPaid'
    && showReceiptComposer
    && !hasReceipts
  )
    ? markPaidReceiptSectionHelpText
    : showReceiptComposer && receiptForm.editingId != null
      ? editingReceiptComposerHelpText
    : showReceiptComposer && !hasReceipts
      ? firstReceiptComposerHelpText
      : showReceiptComposer && hasReceipts
        ? receiptComposerHelpText
        : '';
  const showAddReceiptAction = !showReceiptComposer && hasReceipts && selectedDossier?.intent !== 'markPaid';
  const canHideReceiptUrlField = showReceiptUrlField
    && receiptForm.editingId == null
    && !canSubmitReceipt
    && !hasReceiptMetadataDraft;
  const showReceiptReviewPane = hasReceipts || !showReceiptComposer;
  const showReceiptMetadataFields = (
    receiptForm.editingId != null
    || showReceiptUrlField
    || Boolean(trimToNull(receiptForm.fileName))
    || canSubmitReceipt
  );
  const isMarkPaidFirstReceiptFlow = selectedDossier?.intent === 'markPaid'
    && !dossierQuery.isError
    && !hasReceipts
    && receiptForm.editingId == null;
  const receiptCancelLabel = isMarkPaidFirstReceiptFlow
    ? 'Cerrar pago'
    : receiptForm.editingId == null
      ? 'Cancelar comprobante'
      : 'Cancelar edición de comprobante';
  const handleCancelReceiptComposer = () => {
    if (isMarkPaidFirstReceiptFlow) {
      setSelectedDossier(null);
      return;
    }
    resetReceiptComposer();
  };
  const hasFollowUpOptionalDraft = (
    followUpForm.editingId != null
    || followUpForm.entryType !== 'note'
    || Boolean(trimToNull(followUpForm.subject))
    || Boolean(trimToNull(followUpForm.attachmentName))
    || Boolean(trimToNull(followUpForm.attachmentUrl))
    || followUpForm.nextFollowUpAt.trim() !== ''
  );
  const showFollowUpOptionalFields = showFollowUpDetails || showFollowUpUrlField || hasFollowUpOptionalDraft;
  const canHideFollowUpOptionalFields = showFollowUpOptionalFields && !hasFollowUpOptionalDraft;
  const canHideFollowUpUrlField = showFollowUpUrlField && !trimToNull(followUpForm.attachmentUrl);
  const showFollowUpCountChip = followUps.length > 1;
  const showFollowUpHistoryPane = followUps.length > 0 || !showFollowUpComposer;
  const isCreatingFirstFollowUp = showFollowUpComposer && followUpForm.editingId == null && followUps.length === 0;
  const canSubmitFollowUp = Boolean(trimToNull(followUpForm.notes));
  const showCompactMarkPaidFollowUpState = selectedDossier?.intent === 'markPaid'
    && followUps.length === 0
    && !showFollowUpComposer;
  const followUpSectionTitle = showCompactMarkPaidFollowUpState
    ? 'Seguimiento (opcional)'
    : followUps.length > 0
    ? 'Historial de seguimiento'
    : isCreatingFirstFollowUp
      ? 'Primer seguimiento'
      : 'Seguimiento';
  const followUpComposerTitle = followUpForm.editingId == null ? 'Registrar seguimiento' : 'Editar seguimiento';
  const followUpComposerSummary = followUpForm.editingId != null
    ? editingFollowUpComposerHelpText
    : isCreatingFirstFollowUp
      ? firstFollowUpComposerHelpText
      : followUpComposerHelpText;
  const currentMutationRegistrationId = updateStatusMutation.variables?.id ?? null;
  const statusMenuReg = statusMenuTarget?.reg ?? null;
  const receiptMenuReceipt = receiptMenuTarget?.receipt ?? null;
  const followUpMenuEntry = followUpMenuTarget?.entry ?? null;
  const activeRegistrationCourseSlug = activeRegistration?.crCourseSlug.trim() ?? '';
  const activeRegistrationCourseLabel = activeRegistrationCourseSlug
    ? (cohortLabelsBySlug.get(activeRegistrationCourseSlug) ?? activeRegistrationCourseSlug)
    : '';
  const activeRegistrationIdentity = activeRegistration
    ? registrationIdentityDisplay(
      activeRegistration.crFullName,
      activeRegistration.crEmail,
      activeRegistration.crPhoneE164,
      activeRegistration.crId,
    )
    : { primary: 'Sin nombre', secondary: 'Sin correo ni teléfono' };
  const activeRegistrationSummary = activeRegistration
    ? registrationDossierContextSummary({
      courseLabel: activeRegistrationCourseLabel,
      createdAt: activeRegistration.crCreatedAt,
      source: activeRegistration.crSource,
    })
    : '';
  const showInternalRegistrationReference = Boolean(
    activeRegistration?.crPartyId
    && !activeRegistration?.crFullName?.trim()
    && !activeRegistration?.crEmail?.trim()
    && !activeRegistration?.crPhoneE164?.trim(),
  );
  const activeRegistrationSecondaryLine = showInternalRegistrationReference && activeRegistration?.crPartyId
    ? `Sin datos de contacto. Referencia interna: Party #${activeRegistration.crPartyId}.`
    : activeRegistrationIdentity.secondary;
  const isRefreshingDossier = dossierQuery.isFetching || (showSystemEmailHistoryAction && showEmailHistory && emailEventsQuery.isFetching);
  const hasDossierRefreshContext = dossierQuery.isError
    || canMarkPaid
    || hasReceipts
    || followUps.length > 0
    || hasSavedNotes
    || showSystemEmailHistoryAction;
  const showDossierRefreshAction = Boolean(selectedDossier)
    && !dossierQuery.isLoading
    && hasDossierRefreshContext;
  const dossierRefreshLabel = showSystemEmailHistoryAction && showEmailHistory
    ? 'Refrescar expediente y correos'
    : 'Refrescar expediente';

  useEffect(() => {
    if (selectedDossier?.intent !== 'markPaid' || !canMarkPaid) return;
    setShowReceiptComposer(false);
  }, [canMarkPaid, selectedDossier?.intent]);

  const isConfirmMarkPaidFlow = isMarkPaidIntent && (showMarkPaidAction || hasMarkedPaidInCurrentDossier);
  const showNotesSection = isMarkPaidIntent
    ? (!isConfirmMarkPaidFlow || hasSavedNotes || showNotesComposer)
    : (hasSavedNotes || showNotesComposer);
  const showFollowUpSection = isMarkPaidIntent
    ? (!isConfirmMarkPaidFlow || followUps.length > 0 || showFollowUpComposer)
    : (followUps.length > 0 || showFollowUpComposer);
  const prioritizePaymentSection = isMarkPaidIntent;
  const showDossierFooterCloseAction = !isMarkPaidFirstReceiptFlow;
  const dossierDialogTitle = isMarkPaidIntent
    ? canMarkPaid
      ? 'Confirmar pago de inscripción'
      : 'Registrar pago de inscripción'
    : 'Expediente de inscripción';

  useEffect(() => {
    setShowEmailHistory(false);
  }, [selectedDossierId, selectedDossier?.intent]);

  const notesSection = (
    <Card variant="outlined">
      <CardContent>
        <Stack spacing={1.5}>
          <Stack direction="row" alignItems="center" justifyContent="space-between" flexWrap="wrap" useFlexGap>
            <Typography variant="h6">
              {showEmptyNotesState ? 'Notas internas (opcional)' : 'Notas internas'}
            </Typography>
            {!showNotesComposer && hasSavedNotes ? (
              <Button
                variant="contained"
                size="small"
                onClick={handleOpenNotesComposer}
              >
                Editar notas
              </Button>
            ) : null}
          </Stack>
          {showNotesComposer ? (
            <>
              <TextField
                label="Notas internas"
                multiline
                minRows={4}
                value={notesDraft}
                onChange={(e) => setNotesDraft(e.target.value)}
                placeholder="Contexto interno, acuerdos, bloqueos o próximos pasos."
                fullWidth
              />
              <Stack spacing={0.75} alignItems="flex-start">
                <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                  <Button
                    variant="contained"
                    size="small"
                    startIcon={<SaveIcon />}
                    onClick={handleSaveNotes}
                    disabled={updateNotesMutation.isPending || !hasNotesDraftChanges}
                  >
                    Guardar notas
                  </Button>
                  <Button variant="text" size="small" onClick={handleHideNotesComposer}>
                    Cancelar notas
                  </Button>
                </Stack>
                {!hasNotesDraftChanges && (
                  <Typography variant="caption" color="text.secondary">
                    Edita el contenido para habilitar Guardar.
                  </Typography>
                )}
              </Stack>
            </>
          ) : hasSavedNotes ? (
            <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap' }}>
              {persistedNotes}
            </Typography>
          ) : (
            <Stack spacing={0.75} alignItems="flex-start">
              <Typography variant="body2" color="text.secondary">
                {emptyNotesSectionHelperText}
              </Typography>
              <Button size="small" variant="text" onClick={handleOpenNotesComposer}>
                Agregar nota
              </Button>
            </Stack>
          )}
        </Stack>
      </CardContent>
    </Card>
  );
  const receiptsSection = (
    <Card variant="outlined">
      <CardContent>
        <Stack spacing={2}>
          <Stack
            direction="row"
            alignItems="flex-start"
            justifyContent="space-between"
            flexWrap="wrap"
            useFlexGap
            spacing={1}
          >
            <Box sx={{ minWidth: 240, flexGrow: 1 }}>
              <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                <Typography variant="h6">Comprobantes de pago</Typography>
                {showReceiptCountChip && (
                  <Chip size="small" label={`${receipts.length} guardado${receipts.length === 1 ? '' : 's'}`} />
                )}
              </Stack>
              {receiptSectionHelpText && (
                <Typography variant="body2" color="text.secondary">
                  {receiptSectionHelpText}
                </Typography>
              )}
            </Box>
            {showAddReceiptAction && (
              <Button
                size="small"
                variant="contained"
                onClick={() => setShowReceiptComposer(true)}
              >
                Agregar comprobante
              </Button>
            )}
          </Stack>

          <Grid container spacing={2}>
            {showReceiptComposer && (
              <Grid item xs={12} md={showReceiptReviewPane ? 6 : 12} data-testid="course-registration-receipt-composer-pane">
                <Stack spacing={1.5}>
                  <GoogleDriveUploadWidget
                    label={
                      receiptForm.fileName
                        ? `Archivo listo: ${receiptForm.fileName}`
                        : 'Subir comprobante (imagen/PDF)'
                    }
                    helperText="Se guardará en Drive y quedará disponible desde esta inscripción."
                    accept="application/pdf,image/*"
                    multiple={false}
                    onComplete={handleReceiptUpload}
                    dense
                  />
                  {!showReceiptUrlField && (
                    <Button
                      size="small"
                      variant="text"
                      sx={{ alignSelf: 'flex-start' }}
                      aria-expanded={showReceiptUrlField}
                      onClick={() => setShowReceiptUrlField(true)}
                    >
                      Usar enlace existente en lugar de subir archivo
                    </Button>
                  )}
                  <Collapse in={showReceiptUrlField} unmountOnExit>
                    <Stack spacing={1} sx={{ pt: 0.5 }}>
                      <TextField
                        label="URL del comprobante"
                        value={receiptForm.fileUrl}
                        onChange={(e) => setReceiptForm((prev) => ({ ...prev, fileUrl: e.target.value }))}
                        placeholder="Pega un enlace existente si el archivo ya está cargado"
                        fullWidth
                      />
                      {canHideReceiptUrlField && (
                        <Button
                          size="small"
                          variant="text"
                          sx={{ alignSelf: 'flex-start' }}
                          aria-expanded={showReceiptUrlField}
                          onClick={() => setShowReceiptUrlField(false)}
                        >
                          Ocultar enlace existente
                        </Button>
                      )}
                    </Stack>
                  </Collapse>
                  {!showReceiptMetadataFields && (
                    <Typography variant="caption" color="text.secondary">
                      Primero elige el archivo o pega un enlace; luego podrás ajustar el nombre visible y
                      {' '}
                      las notas.
                    </Typography>
                  )}
                  <Collapse in={showReceiptMetadataFields} unmountOnExit>
                    <Stack spacing={1.5}>
                      <TextField
                        label="Nombre visible"
                        value={receiptForm.fileName}
                        onChange={(e) => setReceiptForm((prev) => ({ ...prev, fileName: e.target.value }))}
                        placeholder="Ej. transferencia-produbanco-marzo.png"
                        fullWidth
                      />
                      <TextField
                        label="Notas del comprobante"
                        value={receiptForm.notes}
                        onChange={(e) => setReceiptForm((prev) => ({ ...prev, notes: e.target.value }))}
                        placeholder="Referencia, banco, monto o aclaraciones"
                        fullWidth
                        multiline
                        minRows={2}
                      />
                    </Stack>
                  </Collapse>
                  <Stack direction="row" spacing={1}>
                    <Button
                      variant="contained"
                      onClick={handleSubmitReceipt}
                      disabled={createReceiptMutation.isPending || updateReceiptMutation.isPending || !canSubmitReceipt}
                    >
                      {receiptForm.editingId == null ? 'Guardar comprobante' : 'Actualizar comprobante'}
                    </Button>
                    <Button variant="text" onClick={handleCancelReceiptComposer}>
                      {receiptCancelLabel}
                    </Button>
                  </Stack>
                </Stack>
              </Grid>
            )}
            {showReceiptReviewPane && (
              <Grid item xs={12} md={showReceiptComposer ? 6 : 12} data-testid="course-registration-receipt-list-pane">
                <Stack spacing={1.5}>
                  {receipts.length === 0 && !showReceiptComposer && (
                    <Alert
                      severity="info"
                      action={(
                        <Button color="inherit" size="small" onClick={() => setShowReceiptComposer(true)}>
                          Agregar primer comprobante
                        </Button>
                      )}
                    >
                      {emptyReceiptReviewMessage}
                    </Alert>
                  )}
                  {receipts.map((receipt) => (
                    <Paper key={receipt.crrId} variant="outlined" sx={{ p: 1.5 }}>
                      <Stack spacing={1}>
                        {looksLikeImageResource(receipt.crrFileUrl, receipt.crrFileName) && (
                          <Box
                            component="img"
                            src={receipt.crrFileUrl}
                            alt={receipt.crrFileName ?? `Comprobante ${receipt.crrId}`}
                            sx={{
                              width: '100%',
                              maxHeight: 220,
                              objectFit: 'cover',
                              borderRadius: 1.5,
                              bgcolor: 'grey.100',
                            }}
                          />
                        )}
                        <Stack direction="row" justifyContent="space-between" alignItems="center" flexWrap="wrap" useFlexGap>
                          <Box>
                            <Link
                              href={receipt.crrFileUrl}
                              target="_blank"
                              rel="noreferrer"
                              underline="hover"
                              color="text.primary"
                              variant="subtitle2"
                              sx={{ display: 'inline-flex', alignItems: 'center', gap: 0.75, fontWeight: 600 }}
                            >
                              {receipt.crrFileName ?? `Comprobante #${receipt.crrId}`}
                              <OpenInNewIcon sx={{ fontSize: 16 }} />
                            </Link>
                            <Typography variant="caption" color="text.secondary">
                              Subido: {formatDate(receipt.crrCreatedAt)}
                            </Typography>
                          </Box>
                          <IconButton
                            size="small"
                            title="Opciones del comprobante"
                            aria-label={`Abrir acciones para comprobante ${receipt.crrFileName ?? `comprobante ${receipt.crrId}`}`}
                            aria-haspopup="menu"
                            onClick={(event) => handleOpenReceiptMenu(event.currentTarget, receipt)}
                          >
                            <MoreVertIcon fontSize="small" />
                          </IconButton>
                        </Stack>
                        {receipt.crrNotes && (
                          <Typography variant="body2" color="text.secondary">
                            {receipt.crrNotes}
                          </Typography>
                        )}
                      </Stack>
                    </Paper>
                  ))}
                </Stack>
              </Grid>
            )}
          </Grid>
        </Stack>
      </CardContent>
    </Card>
  );

  return (
    <Stack spacing={3}>
      <Stack
        direction={{ xs: 'column', sm: 'row' }}
        justifyContent="space-between"
        alignItems={{ xs: 'flex-start', sm: 'center' }}
        spacing={2}
      >
        <Stack spacing={0.5} sx={{ minWidth: 0, flex: '1 1 320px' }}>
          <Typography variant="h4" fontWeight={700}>
            Inscripciones de cursos
          </Typography>
          {showDossierScopeHint && (
            <Typography
              variant="body2"
              color="text.secondary"
              data-testid="course-registration-page-intro"
            >
              {dossierScopeHint}
            </Typography>
          )}
        </Stack>
        {showHeaderRefreshAction && (
          <Stack
            direction="row"
            spacing={1}
            flexWrap="wrap"
            useFlexGap
            data-testid="course-registration-header-actions"
          >
            <Button
              size="small"
              variant="outlined"
              startIcon={<RefreshIcon />}
              onClick={handleRefresh}
              disabled={regsQuery.isFetching || cohortsQuery.isFetching}
            >
              {headerRefreshLabel}
            </Button>
          </Stack>
        )}
      </Stack>

      {pageFlash && <Alert severity={pageFlash.severity}>{pageFlash.message}</Alert>}

      {showInitialCohortResolutionState && (
        <Alert
          severity="info"
          variant="outlined"
          icon={<CircularProgress size={18} />}
          data-testid="course-registration-initial-cohort-loading"
        >
          {initialCohortResolutionMessage}
        </Alert>
      )}

      {showInitialCohortErrorState && (
        <Alert
          severity="warning"
          variant="outlined"
          data-testid="course-registration-initial-cohort-error"
          action={(
            <Button
              color="inherit"
              size="small"
              onClick={handleRefresh}
              disabled={cohortsQuery.isFetching}
            >
              {headerRefreshLabel}
            </Button>
          )}
        >
          {initialCohortErrorMessage}
        </Alert>
      )}

      {showRegistrationFilterPanel && showInitialFilterGuidance && (
        <Alert
          severity="info"
          variant="outlined"
          data-testid="course-registration-initial-empty-state"
          action={(
            <Button
              color="inherit"
              size="small"
              component={RouterLink}
              to={initialEmptyStateAction.to}
            >
              {initialEmptyStateAction.label}
            </Button>
          )}
        >
          {initialEmptyStateMessage}
        </Alert>
      )}

      {showRegistrationFilterPanel && !showInitialFilterGuidance && (
        <Paper sx={{ p: 3, borderRadius: 3 }}>
          <>
            <Grid container spacing={2}>
              {combinedSingleChoiceSummary ? (
                <Grid item xs={12}>
                  <Stack
                    data-testid="course-registration-current-view-summary"
                    spacing={0.5}
                    sx={{
                      minHeight: 40,
                      justifyContent: 'center',
                      px: 1.5,
                      py: 1.25,
                      border: '1px solid',
                      borderColor: 'divider',
                      borderRadius: 1,
                    }}
                  >
                    <Typography variant="caption" color="text.secondary">
                      Vista actual
                    </Typography>
                    <Typography variant="body2" fontWeight={600}>
                      {combinedSingleChoiceSummary}
                    </Typography>
                    {combinedSingleChoiceContextSummary && (
                      <Typography
                        variant="caption"
                        color="text.secondary"
                        data-testid="course-registration-single-choice-context"
                      >
                        {combinedSingleChoiceContextSummary}
                      </Typography>
                    )}
                    {showFirstRunFilterHelper && (
                      <Typography variant="caption" color="text.secondary">
                        {combinedSingleChoiceHelperText}
                      </Typography>
                    )}
                    {showInlineSummaryResetAction && (
                      <Button
                        size="small"
                        variant="text"
                        sx={{ alignSelf: 'flex-start', mt: 0.5 }}
                        onClick={handleResetFilters}
                        data-testid="course-registration-inline-reset"
                      >
                        {resetViewLabel}
                      </Button>
                    )}
                    {showInlineSingleChoiceLimitToggle && (
                      <Button
                        size="small"
                        variant="text"
                        sx={{ alignSelf: 'flex-start', mt: 0.5 }}
                        onClick={handleToggleAdvancedFilters}
                        aria-expanded={showAdvancedFilters}
                      >
                        {limitToggleLabel}
                      </Button>
                    )}
                  </Stack>
                </Grid>
              ) : (
                <>
                  <Grid item xs={12} md={6}>
                    {showCohortFilterUnavailableSummary ? (
                      <Stack
                        data-testid="course-registration-cohort-filter-unavailable"
                        spacing={0.5}
                        sx={{
                          minHeight: 40,
                          justifyContent: 'center',
                          px: 1.5,
                          py: 1.25,
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1,
                        }}
                      >
                        <Typography variant="caption" color="text.secondary">
                          Cohortes no disponibles
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {cohortFilterUnavailableMessage}
                        </Typography>
                      </Stack>
                    ) : singleAvailableCohortLabel ? (
                      <Stack
                        data-testid="course-registration-single-cohort-summary"
                        spacing={0.5}
                        sx={{
                          minHeight: 40,
                          justifyContent: 'center',
                          px: 1.5,
                          py: 1.25,
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1,
                        }}
                      >
                        <Typography variant="caption" color="text.secondary">
                          Cohorte disponible
                        </Typography>
                        <Typography variant="body2" fontWeight={600}>
                          {singleAvailableCohortLabel}
                        </Typography>
                        {standaloneSingleChoiceSourceSummary && (
                          <Typography variant="caption" color="text.secondary">
                            {standaloneSingleChoiceSourceSummary}
                          </Typography>
                        )}
                        {showFirstRunFilterHelper && (
                          <Typography variant="caption" color="text.secondary">
                            {singleAvailableCohortHelperText}
                          </Typography>
                        )}
                        {showInlineSingleChoiceLimitToggle && (
                          <Button
                            size="small"
                            variant="text"
                            sx={{ alignSelf: 'flex-start', mt: 0.5 }}
                            onClick={handleToggleAdvancedFilters}
                            aria-expanded={showAdvancedFilters}
                          >
                            {limitToggleLabel}
                          </Button>
                        )}
                      </Stack>
                    ) : (
                      <TextField
                        select
                        label="Curso / cohorte"
                        value={slug}
                        onChange={(e) => {
                          setHasUsedFilterControl(true);
                          setLocalSearch('');
                          setSlug(e.target.value);
                        }}
                        fullWidth
                        size="small"
                        error={cohortsQuery.isError}
                        helperText={
                          cohortsQuery.isError
                            ? 'No se pudieron cargar cohortes.'
                            : cohortsQuery.isLoading
                              ? 'Cargando cohortes…'
                              : undefined
                        }
                      >
                        <MenuItem value="">Todos</MenuItem>
                        {cohortOptions.map((option) => (
                          <MenuItem key={option.value} value={option.value}>
                            {option.label}
                          </MenuItem>
                        ))}
                      </TextField>
                    )}
                  </Grid>
                  <Grid item xs={12} md={6}>
                    {showSingleStatusSummary && singleVisibleStatus ? (
                      <Stack
                        data-testid="course-registration-single-status-summary"
                        spacing={0.5}
                        sx={{
                          minHeight: 40,
                          justifyContent: 'center',
                          px: 1.5,
                          py: 1.25,
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1,
                        }}
                      >
                        <Typography variant="caption" color="text.secondary">
                          Estado disponible
                        </Typography>
                        <Typography variant="body2" fontWeight={600}>
                          {statusFilterLabels[singleVisibleStatus]}
                        </Typography>
                        {standaloneSingleChoiceSourceSummary && (
                          <Typography variant="caption" color="text.secondary">
                            {standaloneSingleChoiceSourceSummary}
                          </Typography>
                        )}
                        {showFirstRunFilterHelper && (
                          <Typography variant="caption" color="text.secondary">
                            {singleVisibleStatusHelperText}
                          </Typography>
                        )}
                        {showInlineSingleChoiceLimitToggle && (
                          <Button
                            size="small"
                            variant="text"
                            sx={{ alignSelf: 'flex-start', mt: 0.5 }}
                            onClick={handleToggleAdvancedFilters}
                            aria-expanded={showAdvancedFilters}
                          >
                            {limitToggleLabel}
                          </Button>
                        )}
                      </Stack>
                    ) : showSingleCustomStatusSummary ? (
                      <Stack
                        data-testid="course-registration-single-custom-status-summary"
                        spacing={0.5}
                        sx={{
                          minHeight: 40,
                          justifyContent: 'center',
                          px: 1.5,
                          py: 1.25,
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1,
                        }}
                      >
                        <Typography variant="caption" color="text.secondary">
                          Estado no estándar
                        </Typography>
                        <Typography variant="body2" fontWeight={600}>
                          {customRegistrationStatusLabel(singleVisibleCustomStatus)}
                        </Typography>
                        <Typography variant="caption" color="text.secondary">
                          {customStatusFilterUnavailableMessage}
                        </Typography>
                      </Stack>
                    ) : showCustomStatusFilterUnavailableSummary ? (
                      <Stack
                        data-testid="course-registration-status-filter-unavailable"
                        spacing={0.5}
                        sx={{
                          minHeight: 40,
                          justifyContent: 'center',
                          px: 1.5,
                          py: 1.25,
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1,
                        }}
                      >
                        <Typography variant="caption" color="text.secondary">
                          Sin filtros de estado
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {customStatusFilterUnavailableMessage}
                        </Typography>
                      </Stack>
                    ) : (
                      <Stack spacing={1}>
                        {showStatusFilterCaption && (
                          <Typography variant="caption" color="text.secondary">
                            Filtrar por estado
                          </Typography>
                        )}
                        <Stack
                          direction="row"
                          spacing={1}
                          flexWrap="wrap"
                          useFlexGap
                          role="group"
                          aria-label={statusFilterGroupLabel}
                        >
                          {actionableStatusFilters.map((value) => (
                            <Chip
                              key={value}
                              clickable
                              component="button"
                              type="button"
                              color={registrationStatusChipColor(value)}
                              label={statusFilterChipLabel(value, statusCounts, hasVisibleRegistrations)}
                              variant={status === value ? 'filled' : 'outlined'}
                              aria-label={`Filtrar inscripciones por estado ${statusFilterLabels[value]}`}
                              aria-pressed={status === value}
                              onClick={() => {
                                setHasUsedFilterControl(true);
                                setLocalSearch('');
                                setStatus((current) => (current === value ? 'all' : value));
                              }}
                            />
                          ))}
                        </Stack>
                        {statusFilterHelperText && (
                          <Typography variant="caption" color="text.secondary">
                            {statusFilterHelperText}
                          </Typography>
                        )}
                      </Stack>
                    )}
                  </Grid>
                </>
              )}
            </Grid>
            {showAdvancedLimitControl && !showInlineSingleChoiceLimitToggle && (
              <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
                <Button
                  size="small"
                  variant="text"
                  onClick={handleToggleAdvancedFilters}
                  aria-expanded={showAdvancedFilters}
                >
                  {limitToggleLabel}
                </Button>
              </Stack>
            )}
            <Collapse in={showAdvancedFilters && showAdvancedLimitControl} unmountOnExit>
              <Box sx={{ mt: 2, maxWidth: { xs: '100%', md: 280 } }}>
                <TextField
                  label="Límite"
                  type="number"
                  inputProps={{ min: 1 }}
                  value={limit}
                  onChange={(e) => {
                    setHasUsedFilterControl(true);
                    setLocalSearch('');
                    setLimit(parsePositiveLimit(e.target.value, DEFAULT_LIMIT));
                  }}
                  helperText="Máximo de filas a cargar en esta vista. Déjalo en 200 salvo que necesites revisar un lote distinto."
                  fullWidth
                  size="small"
                />
              </Box>
            </Collapse>
            {showFirstRunFilterHelper && filtersHelpText && !showFilteredEmptyState && (
              <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mt: 2 }}>
                {filtersHelpText}
              </Typography>
            )}
            {showFilteredUtilityRow && (
              <Stack
                direction="row"
                spacing={1}
                alignItems="center"
                sx={{ mt: 1.5 }}
                flexWrap="wrap"
                useFlexGap
                data-testid="course-registration-filter-utilities"
              >
                {filteredUtilitySummaryMessage && (
                  <Typography
                    data-testid="course-registration-filter-summary"
                    variant="body2"
                    color="text.secondary"
                  >
                    {filteredUtilitySummaryMessage}
                  </Typography>
                )}
                {showFilteredResetAction && (
                  <Button size="small" onClick={handleResetFilters}>
                    {resetViewLabel}
                  </Button>
                )}
                {canCopyCsv && (
                  <Button
                    size="small"
                    startIcon={<ContentCopyIcon fontSize="small" />}
                    onClick={() => void handleCopyCsv()}
                  >
                    {copyCsvButtonLabel}
                  </Button>
                )}
                {copyMessage && (
                  <Typography variant="caption" color="text.secondary">
                    {copyMessage}
                  </Typography>
                )}
              </Stack>
            )}
            {combinedSharedListContextSummary ? (
              <Typography variant="body2" color="text.secondary" sx={{ mt: 1.5 }}>
                {combinedSharedListContextSummary}
              </Typography>
            ) : (
              <>
                {shouldShowSharedCohortSummary && (
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1.5 }}>
                    Mostrando una sola cohorte: {singleVisibleCohortLabel}.
                  </Typography>
                )}
                {shouldShowSharedSourceSummary && (
                  <Typography variant="body2" color="text.secondary" sx={{ mt: shouldShowSharedCohortSummary ? 0.75 : 1.5 }}>
                    {sharedVisibleSourceSummary}
                  </Typography>
                )}
                {sharedVisibleNotesSummary && (
                  <Typography
                    variant="body2"
                    color="text.secondary"
                    sx={{ mt: shouldShowSharedCohortSummary || shouldShowSharedSourceSummary ? 0.75 : 1.5 }}
                  >
                    {sharedVisibleNotesSummary}
                  </Typography>
                )}
              </>
            )}
            {showStandaloneListUtilityRow && (
              <Stack
                direction="row"
                spacing={1}
                alignItems="center"
                sx={{ mt: 2 }}
                flexWrap="wrap"
                useFlexGap
                data-testid="course-registration-list-utilities"
              >
                {standaloneUtilitySummaryMessage && (
                  <Typography variant="body2" color="text.secondary">
                    {standaloneUtilitySummaryMessage}
                  </Typography>
                )}
                {canCopyCsv && (
                  <Button
                    size="small"
                    startIcon={<ContentCopyIcon fontSize="small" />}
                    onClick={() => void handleCopyCsv()}
                  >
                    {copyCsvButtonLabel}
                  </Button>
                )}
                {copyMessage && (
                  <Typography variant="caption" color="text.secondary">
                    {copyMessage}
                  </Typography>
                )}
              </Stack>
            )}
          </>
        </Paper>
      )}

      {!showInitialFilterGuidance && !showInitialCohortResolutionState && !showInitialCohortErrorState && (
        <Paper sx={{ p: 3, borderRadius: 3 }}>
          {regsQuery.isError && (
            <Alert
              severity="error"
              action={showRegistrationErrorInlineRetry ? (
                <Button color="inherit" size="small" onClick={handleRefresh}>
                  {registrationErrorRetryLabel}
                </Button>
              ) : undefined}
            >
              No se pudieron cargar las inscripciones: {regsQuery.error instanceof Error ? regsQuery.error.message : 'Error'}
            </Alert>
          )}
          {regsQuery.isLoading && <Typography>Cargando inscripciones…</Typography>}
          {showLocalSearchControl && (
            <Box sx={{ mb: 2 }}>
              <TextField
                label={LOCAL_SEARCH_LABEL}
                value={localSearch}
                onChange={(e) => {
                  setHasUsedFilterControl(true);
                  setLocalSearch(e.target.value);
                }}
                placeholder="Nombre, email, teléfono, estado, fuente o curso"
                helperText={localSearchHelperText}
                size="small"
                fullWidth
                data-testid="course-registration-local-search"
                InputProps={{
                  startAdornment: (
                    <InputAdornment position="start">
                      <SearchIcon fontSize="small" />
                    </InputAdornment>
                  ),
                  endAdornment: localSearchKey && !showEmptyLocalSearchResults ? (
                    <InputAdornment position="end">
                      <Tooltip title="Limpiar búsqueda">
                        <IconButton
                          aria-label="Limpiar búsqueda"
                          size="small"
                          edge="end"
                          onClick={() => setLocalSearch('')}
                        >
                          <ClearIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                    </InputAdornment>
                  ) : undefined,
                }}
              />
            </Box>
          )}
          {!regsQuery.isLoading && registrations.length === 0 && (
            hasCustomFilters ? (
              <Alert
                severity="info"
                action={(
                  <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                    {showFilteredEmptyStateResetAction && (
                      <Button color="inherit" size="small" onClick={handleResetFilters}>
                        {resetViewLabel}
                      </Button>
                    )}
                    {showFilteredEmptyStateRefreshAction && (
                      <Button
                        color="inherit"
                        size="small"
                        onClick={handleRefresh}
                        disabled={regsQuery.isFetching}
                      >
                        Refrescar lista
                      </Button>
                    )}
                  </Stack>
                )}
              >
                {filteredEmptyStateMessage}
              </Alert>
            ) : (
              <Typography color="text.secondary">Todavía no hay inscripciones para mostrar en esta vista.</Typography>
            )
          )}
          {!regsQuery.isLoading && showEmptyLocalSearchResults && (
            <Alert
              severity="info"
              action={(
                <Button color="inherit" size="small" onClick={() => setLocalSearch('')}>
                  Limpiar búsqueda
                </Button>
              )}
            >
              No hay coincidencias para "{localSearchTerm}" en las {formatRegistrationCountLabel(loadedRegistrationCount)} cargadas.
            </Alert>
          )}
          {searchedRegistrations.length ? (
            <Stack spacing={1.5}>
              <Stack divider={<Divider flexItem />} spacing={2}>
                {searchedRegistrations.map((reg) => {
                  const isUpdating = updateStatusMutation.isPending && currentMutationRegistrationId === reg.crId;
                  const rowIdentity = registrationIdentityDisplay(
                    reg.crFullName,
                    reg.crEmail,
                    reg.crPhoneE164,
                    reg.crId,
                  );
                  const rowUsesGeneratedIdentity = !reg.crFullName?.trim()
                    && !reg.crEmail?.trim()
                    && !reg.crPhoneE164?.trim();
                  const rowNeedsActionDisambiguator = registrationIdsRequiringActionDisambiguator.has(reg.crId);
                  const rowActionTarget = getActionTargetLabelForRegistration(reg);
                  const rowCohortSlug = reg.crCourseSlug.trim();
                  const rowCohortLabel = cohortLabelsBySlug.get(rowCohortSlug) ?? rowCohortSlug;
                  const hasRowNotes = Boolean(reg.crAdminNotes?.trim());
                  const showRowCohort = selectedSlug
                    ? rowCohortSlug !== selectedSlug
                    : !(singleVisibleCohortLabel || singleAvailableCohortLabel);
                  const showRowSource = !hasSharedVisibleSource;
                  const hasDateOnlyRowContext = !showRowCohort && !showRowSource && !hasRowNotes;
                  const hideDateOnlyRowContext = hasDateOnlyRowContext
                    && (
                      loadedRegistrationCount === 1
                      || !hasCustomFilters
                    );
                  const rowContextSummary = registrationListContextSummary({
                    cohortLabel: rowCohortLabel,
                    createdAt: reg.crCreatedAt,
                    hasNotes: hasRowNotes && !allVisibleRegistrationsHaveNotes,
                    showCreatedAt: !hideDateOnlyRowContext && !hideTinyDefaultListRowDates && !shouldHideSharedCreatedAtContext,
                    showCohort: showRowCohort,
                    showSource: showRowSource,
                    source: reg.crSource,
                  });
                  const showRowContext = Boolean(rowContextSummary);
                  return (
                    <Box key={reg.crId} sx={{ display: 'flex', gap: 2, alignItems: 'center', flexWrap: 'wrap' }}>
                      <Box sx={{ minWidth: 240 }}>
                        <Typography variant="subtitle1" fontWeight={700} component="div">
                          <Link
                            component="button"
                            type="button"
                            underline="hover"
                            color="inherit"
                            aria-label={`Abrir expediente de ${rowActionTarget}`}
                            onClick={() => handleOpenDossier(reg, 'review')}
                            sx={{
                              p: 0,
                              border: 0,
                              background: 'none',
                              font: 'inherit',
                              fontWeight: 'inherit',
                              textAlign: 'left',
                              cursor: 'pointer',
                              '&:focus-visible': {
                                outline: '2px solid',
                                outlineColor: 'primary.main',
                                outlineOffset: 2,
                                borderRadius: 0.5,
                              },
                            }}
                          >
                            {rowIdentity.primary}
                          </Link>
                        </Typography>
                        {rowIdentity.secondary && !rowUsesGeneratedIdentity && (
                          <Typography variant="body2" color="text.secondary">
                            {rowIdentity.secondary}
                          </Typography>
                        )}
                        {rowNeedsActionDisambiguator && (
                          <Typography variant="caption" color="text.secondary">
                            Registro #{reg.crId}
                          </Typography>
                        )}
                      </Box>
                      {showRowContext && (
                        <Box sx={{ minWidth: 180 }}>
                          <Typography variant="body2" color="text.secondary">
                            {rowContextSummary}
                          </Typography>
                        </Box>
                      )}
                      <Button
                        size="small"
                        variant="text"
                        color={registrationStatusButtonColor(reg.crStatus)}
                        endIcon={<ArrowDropDownIcon />}
                        title={`Cambiar estado; actual: ${registrationStatusLabel(reg.crStatus)}`}
                        aria-label={`Cambiar estado para ${rowActionTarget}`}
                        aria-haspopup="menu"
                        disabled={isUpdating}
                        onClick={(event) => handleOpenStatusMenu(event.currentTarget, reg)}
                      >
                        {registrationStatusButtonLabel(reg.crStatus, useCompactStatusActionLabel)}
                      </Button>
                      <Box sx={{ flexGrow: 1 }} />
                    </Box>
                  );
                })}
              </Stack>
            </Stack>
          ) : null}
        </Paper>
      )}

      <Menu
        open={Boolean(statusMenuTarget)}
        anchorEl={statusMenuTarget?.anchorEl ?? null}
        onClose={handleCloseStatusMenu}
      >
        {statusMenuReg && canOpenPaymentWorkflowFromStatus(statusMenuReg.crStatus) && (
          <MenuItem
            onClick={() => {
              handleCloseStatusMenu();
              handleOpenDossier(statusMenuReg, 'markPaid');
            }}
          >
            {openPaymentWorkflowLabel}
          </MenuItem>
        )}
        {statusMenuReg && canTransitionToStatus(statusMenuReg.crStatus, 'pending_payment') && (
          <MenuItem
            title={`Usa esta acción para ${pendingStatusMenuTargetLabel(statusMenuReg.crStatus)}.`}
            onClick={() => {
              handleCloseStatusMenu();
              handleQuickStatus(statusMenuReg, 'pending_payment');
            }}
          >
            {pendingStatusMenuLabel(statusMenuReg.crStatus)}
          </MenuItem>
        )}
        {statusMenuReg && canTransitionToStatus(statusMenuReg.crStatus, 'cancelled') && (
          <MenuItem
            onClick={() => {
              handleCloseStatusMenu();
              handleQuickStatus(statusMenuReg, 'cancelled');
            }}
          >
            Cancelar inscripción
          </MenuItem>
        )}
      </Menu>

      <Menu
        open={Boolean(receiptMenuTarget)}
        anchorEl={receiptMenuTarget?.anchorEl ?? null}
        onClose={handleCloseReceiptMenu}
      >
        {receiptMenuReceipt && (
          <MenuItem onClick={() => handleEditReceipt(receiptMenuReceipt)}>
            Editar comprobante
          </MenuItem>
        )}
        {receiptMenuReceipt && (
          <MenuItem onClick={() => handleDeleteReceipt(receiptMenuReceipt)}>
            Eliminar comprobante
          </MenuItem>
        )}
      </Menu>

      <Menu
        open={Boolean(followUpMenuTarget)}
        anchorEl={followUpMenuTarget?.anchorEl ?? null}
        onClose={handleCloseFollowUpMenu}
      >
        {followUpMenuEntry && (
          <MenuItem onClick={() => handleEditFollowUp(followUpMenuEntry)}>
            Editar seguimiento
          </MenuItem>
        )}
        {followUpMenuEntry && (
          <MenuItem onClick={() => handleDeleteFollowUp(followUpMenuEntry)}>
            Eliminar seguimiento
          </MenuItem>
        )}
      </Menu>

      <Dialog
        open={Boolean(selectedDossier)}
        onClose={() => setSelectedDossier(null)}
        fullWidth
        maxWidth="lg"
      >
        <DialogTitle>
          <Stack direction="row" alignItems="center" justifyContent="space-between" spacing={1} useFlexGap>
            <span>{dossierDialogTitle}</span>
            {showDossierRefreshAction && (
              <Tooltip title={dossierRefreshLabel}>
                <span>
                  <IconButton
                    size="small"
                    aria-label={dossierRefreshLabel}
                    onClick={handleRefreshDossier}
                    disabled={!selectedDossier || isRefreshingDossier}
                  >
                    <RefreshIcon fontSize="small" />
                  </IconButton>
                </span>
              </Tooltip>
            )}
          </Stack>
        </DialogTitle>
        <DialogContent dividers>
          {dossierQuery.isLoading && (
            <Stack direction="row" spacing={1} alignItems="center">
              <CircularProgress size={18} />
              <Typography variant="body2">Cargando expediente…</Typography>
            </Stack>
          )}

          {dossierQuery.isError && (
            <Alert severity="error">
              No se pudo cargar el expediente: {dossierQuery.error instanceof Error ? dossierQuery.error.message : 'Error'}
            </Alert>
          )}

          {activeRegistration && !dossierQuery.isLoading && !dossierQuery.isError && (
            <Stack spacing={2.5}>
              {dossierFlash && <Alert severity={dossierFlash.severity}>{dossierFlash.message}</Alert>}

              <Paper variant="outlined" sx={{ p: 2 }}>
                <Stack spacing={1.5}>
                  <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                    <Typography variant="h6">{activeRegistrationIdentity.primary}</Typography>
                    {statusChip(activeRegistration.crStatus)}
                  </Stack>
                  {activeRegistrationSecondaryLine && (
                    <Typography variant="body2" color="text.secondary">
                      {activeRegistrationSecondaryLine}
                    </Typography>
                  )}
                  {activeRegistrationSummary && (
                    <Typography variant="body2" color="text.secondary">
                      {activeRegistrationSummary}
                    </Typography>
                  )}
                  {showDossierActionRow && (
                    <Stack
                      direction="row"
                      spacing={1}
                      flexWrap="wrap"
                      useFlexGap
                      data-testid="course-registration-dossier-actions"
                    >
                      {showMarkPaidAction && (
                        <Button
                          variant="contained"
                          color="success"
                          onClick={handleMarkPaidFromDossier}
                          disabled={updateStatusMutation.isPending}
                        >
                          Marcar pagado
                        </Button>
                      )}
                      {showSystemEmailHistoryAction && (
                        <Button
                          variant="outlined"
                          onClick={() => setShowEmailHistory((current) => !current)}
                          aria-expanded={showEmailHistory}
                        >
                          {showEmailHistory ? hideSystemEmailsLabel : showSystemEmailsLabel}
                        </Button>
                      )}
                      {showInlineEmptyNotesAction && (
                        <Button
                          variant="outlined"
                          onClick={handleOpenNotesComposer}
                        >
                          Agregar nota
                        </Button>
                      )}
                      {showInlineEmptyFollowUpAction && (
                        <Button
                          variant="outlined"
                          onClick={() => setShowFollowUpComposer(true)}
                        >
                          Agregar seguimiento
                        </Button>
                      )}
                    </Stack>
                  )}
                </Stack>
              </Paper>

              <Collapse in={showSystemEmailHistoryAction && showEmailHistory} unmountOnExit>
                <Card variant="outlined">
                  <CardContent>
                    <Stack spacing={1.5}>
                      <Stack
                        direction="row"
                        alignItems="center"
                        justifyContent="space-between"
                        flexWrap="wrap"
                        useFlexGap
                      >
                        <Box sx={{ minWidth: 240, flexGrow: 1 }}>
                          <Typography variant="h6">Correos del sistema</Typography>
                          <Typography variant="body2" color="text.secondary">
                            {systemEmailHistoryHelperText}
                          </Typography>
                        </Box>
                      </Stack>

                      {emailEventsQuery.isLoading && (
                        <Stack direction="row" spacing={1} alignItems="center">
                          <CircularProgress size={18} />
                          <Typography variant="body2">Cargando historial…</Typography>
                        </Stack>
                      )}

                      {emailEventsQuery.isError && (
                        <Alert severity="error">
                          No se pudo cargar el historial: {emailEventsQuery.error instanceof Error ? emailEventsQuery.error.message : 'Error'}
                        </Alert>
                      )}

                      {!emailEventsQuery.isLoading && !emailEventsQuery.isError && (emailEventsQuery.data?.length ?? 0) === 0 && (
                        <Alert severity="info">{emptySystemEmailHistoryMessage}</Alert>
                      )}

                      {!emailEventsQuery.isLoading && !emailEventsQuery.isError && (emailEventsQuery.data?.length ?? 0) > 0 && (
                        <Stack spacing={1}>
                          {(emailEventsQuery.data ?? []).map((entry) => (
                            <Paper key={entry.ceId} variant="outlined" sx={{ p: 1.5 }}>
                              <Stack direction="row" spacing={1} alignItems="center" sx={{ mb: 0.75 }} flexWrap="wrap" useFlexGap>
                                <Chip size="small" label={eventStatusLabel(entry.ceStatus)} color={eventStatusColor(entry.ceStatus)} />
                                <Chip size="small" label={eventTypeLabel(entry.ceEventType)} variant="outlined" />
                                <Typography variant="caption" color="text.secondary">
                                  {formatDate(entry.ceCreatedAt)}
                                </Typography>
                              </Stack>
                              {entry.ceMessage && (
                                <Typography
                                  variant="body2"
                                  sx={{ fontFamily: 'monospace', whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}
                                >
                                  {entry.ceMessage}
                                </Typography>
                              )}
                            </Paper>
                          ))}
                        </Stack>
                      )}
                    </Stack>
                  </CardContent>
                </Card>
              </Collapse>

              {prioritizePaymentSection ? (
                <>
                  {receiptsSection}
                  {showNotesSection && notesSection}
                </>
              ) : (
                <>
                  {showNotesSection && notesSection}
                  {receiptsSection}
                </>
              )}

              {showFollowUpSection && (
                <Card variant="outlined">
                <CardContent>
                  <Stack spacing={2}>
                    <Stack direction="row" alignItems="center" justifyContent="space-between" flexWrap="wrap" useFlexGap>
                      <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                        <Typography variant="h6">{followUpSectionTitle}</Typography>
                        {showFollowUpCountChip && (
                          <Chip size="small" label={`${followUps.length} entrad${followUps.length === 1 ? 'a' : 'as'}`} />
                        )}
                      </Stack>
                      {!showFollowUpComposer && followUps.length > 0 && (
                        <Button
                          size="small"
                          variant="contained"
                          onClick={() => setShowFollowUpComposer(true)}
                        >
                          Agregar seguimiento
                        </Button>
                      )}
                    </Stack>

                    <Grid container spacing={2}>
                      {showFollowUpComposer && (
                        <Grid item xs={12} md={showFollowUpHistoryPane ? 6 : 12} data-testid="course-registration-follow-up-composer-pane">
                          <Stack spacing={1.5}>
                            <Box sx={{ minWidth: 240, flexGrow: 1 }}>
                              {!isCreatingFirstFollowUp && (
                                <Typography variant="subtitle2">
                                  {followUpComposerTitle}
                                </Typography>
                              )}
                              <Typography variant="body2" color="text.secondary">
                                {followUpComposerSummary}
                              </Typography>
                            </Box>
                            <Stack spacing={1.5} sx={{ pt: 0.5 }}>
                              <TextField
                                label="Nota de seguimiento"
                                value={followUpForm.notes}
                                onChange={(e) => setFollowUpForm((prev) => ({ ...prev, notes: e.target.value }))}
                                multiline
                                minRows={3}
                                placeholder="Qué pasó, qué se acordó y cuál es el siguiente paso."
                                fullWidth
                              />
                              {!showFollowUpOptionalFields && (
                                <Stack spacing={0.75} alignItems="flex-start">
                                  <Typography variant="caption" color="text.secondary">
                                    Agrega tipo, asunto, recordatorio o evidencia solo si hacen falta.
                                  </Typography>
                                  <Button
                                    size="small"
                                    variant="text"
                                    onClick={() => setShowFollowUpDetails(true)}
                                    aria-expanded={showFollowUpOptionalFields}
                                  >
                                    Agregar detalles opcionales
                                  </Button>
                                </Stack>
                              )}
                              <Collapse in={showFollowUpOptionalFields} unmountOnExit>
                                <Stack spacing={1.5}>
                                  <TextField
                                    select
                                    label="Tipo"
                                    value={followUpForm.entryType}
                                    onChange={(e) => setFollowUpForm((prev) => ({ ...prev, entryType: e.target.value }))}
                                    fullWidth
                                  >
                                    {followUpTypeOptions.map((option) => (
                                      <MenuItem key={option} value={option}>
                                        {eventTypeLabel(option)}
                                      </MenuItem>
                                    ))}
                                  </TextField>
                                  <TextField
                                    label="Asunto"
                                    value={followUpForm.subject}
                                    onChange={(e) => setFollowUpForm((prev) => ({ ...prev, subject: e.target.value }))}
                                    placeholder="Ej. Confirmó transferencia"
                                    fullWidth
                                  />
                                  <TextField
                                    label="Próximo seguimiento"
                                    type="datetime-local"
                                    value={followUpForm.nextFollowUpAt}
                                    onChange={(e) => setFollowUpForm((prev) => ({ ...prev, nextFollowUpAt: e.target.value }))}
                                    fullWidth
                                    InputLabelProps={{ shrink: true }}
                                  />
                                  <GoogleDriveUploadWidget
                                    label={
                                      followUpForm.attachmentName
                                        ? `Adjunto listo: ${followUpForm.attachmentName}`
                                        : 'Adjuntar evidencia opcional'
                                    }
                                    helperText="Puedes adjuntar un audio, captura, PDF o imagen."
                                    accept="application/pdf,image/*,audio/*"
                                    multiple={false}
                                    onComplete={handleFollowUpUpload}
                                    dense
                                  />
                                  {!showFollowUpUrlField && (
                                    <Button
                                      size="small"
                                      variant="text"
                                      sx={{ alignSelf: 'flex-start' }}
                                      aria-expanded={showFollowUpUrlField}
                                      onClick={() => setShowFollowUpUrlField(true)}
                                    >
                                      Usar enlace existente en lugar de subir adjunto
                                    </Button>
                                  )}
                                  {showFollowUpUrlField && (
                                    <Stack spacing={1}>
                                      <TextField
                                        label="URL del adjunto"
                                        value={followUpForm.attachmentUrl}
                                        onChange={(e) => setFollowUpForm((prev) => ({ ...prev, attachmentUrl: e.target.value }))}
                                        fullWidth
                                      />
                                      {canHideFollowUpUrlField && (
                                        <Button
                                          size="small"
                                          variant="text"
                                          sx={{ alignSelf: 'flex-start' }}
                                          aria-expanded={showFollowUpUrlField}
                                          onClick={() => setShowFollowUpUrlField(false)}
                                        >
                                          Ocultar enlace existente
                                        </Button>
                                      )}
                                    </Stack>
                                  )}
                                  {canHideFollowUpOptionalFields && (
                                    <Button
                                      size="small"
                                      variant="text"
                                      sx={{ alignSelf: 'flex-start' }}
                                      onClick={() => {
                                        setShowFollowUpDetails(false);
                                        setShowFollowUpUrlField(false);
                                      }}
                                    >
                                      Ocultar detalles opcionales
                                    </Button>
                                  )}
                                </Stack>
                              </Collapse>
                              <Stack direction="row" spacing={1}>
                                <Button
                                  variant="contained"
                                  onClick={handleSubmitFollowUp}
                                  disabled={createFollowUpMutation.isPending || updateFollowUpMutation.isPending || !canSubmitFollowUp}
                                >
                                  {followUpForm.editingId == null ? 'Guardar seguimiento' : 'Actualizar seguimiento'}
                                </Button>
                                <Button variant="text" onClick={resetFollowUpComposer}>
                                  {followUpForm.editingId == null ? 'Cancelar seguimiento' : 'Cancelar edición de seguimiento'}
                                </Button>
                              </Stack>
                            </Stack>
                          </Stack>
                        </Grid>
                      )}
                      {showFollowUpHistoryPane && (
                        <Grid item xs={12} md={showFollowUpComposer ? 6 : 12} data-testid="course-registration-follow-up-list-pane">
                          <Stack spacing={1.5}>
                            {showCompactMarkPaidFollowUpState ? (
                              <Stack spacing={0.75} alignItems="flex-start">
                                <Typography variant="body2" color="text.secondary">
                                  {markPaidEmptyFollowUpHelperText}
                                </Typography>
                                <Button size="small" variant="text" onClick={() => setShowFollowUpComposer(true)}>
                                  Agregar seguimiento opcional
                                </Button>
                              </Stack>
                            ) : followUps.length === 0 && !showFollowUpComposer ? (
                              <Alert
                                severity="info"
                                action={(
                                  <Button color="inherit" size="small" onClick={() => setShowFollowUpComposer(true)}>
                                    Registrar primer seguimiento
                                  </Button>
                                )}
                              >
                                {emptyFollowUpAlertMessage}
                              </Alert>
                            ) : null}
                            {followUps.map((entry) => (
                              <Paper key={entry.crfId} variant="outlined" sx={{ p: 1.5 }}>
                                <Stack spacing={1}>
                                  <Stack direction="row" justifyContent="space-between" alignItems="flex-start" flexWrap="wrap" useFlexGap>
                                    <Stack direction="row" spacing={0.75} alignItems="center" flexWrap="wrap" useFlexGap>
                                      <Chip size="small" label={eventTypeLabel(entry.crfEntryType)} variant="outlined" />
                                      <Typography variant="caption" color="text.secondary">
                                        {formatDate(entry.crfCreatedAt)}
                                      </Typography>
                                      {entry.crfNextFollowUpAt && (
                                        <Chip
                                          size="small"
                                          color="warning"
                                          label={`Próximo: ${formatDate(entry.crfNextFollowUpAt)}`}
                                        />
                                      )}
                                    </Stack>
                                    <IconButton
                                      size="small"
                                      title="Opciones del seguimiento"
                                      aria-label={`Abrir acciones para seguimiento ${followUpActionTargetLabel(entry)}`}
                                      aria-haspopup="menu"
                                      onClick={(event) => handleOpenFollowUpMenu(event.currentTarget, entry)}
                                    >
                                      <MoreVertIcon fontSize="small" />
                                    </IconButton>
                                  </Stack>
                                  {entry.crfSubject && (
                                    <Typography variant="subtitle2">{entry.crfSubject}</Typography>
                                  )}
                                  <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap' }}>
                                    {entry.crfNotes}
                                  </Typography>
                                  {entry.crfAttachmentUrl && (
                                    <Link href={entry.crfAttachmentUrl} target="_blank" rel="noreferrer" underline="hover">
                                      <Stack direction="row" spacing={0.75} alignItems="center">
                                        <OpenInNewIcon sx={{ fontSize: 16 }} />
                                        <span>{entry.crfAttachmentName ?? 'Abrir adjunto'}</span>
                                      </Stack>
                                    </Link>
                                  )}
                                </Stack>
                              </Paper>
                            ))}
                          </Stack>
                        </Grid>
                      )}
                    </Grid>
                  </Stack>
                </CardContent>
              </Card>
              )}
            </Stack>
          )}
        </DialogContent>
        {showDossierFooterCloseAction && (
          <DialogActions>
            <Button onClick={() => setSelectedDossier(null)}>Cerrar</Button>
          </DialogActions>
        )}
      </Dialog>

    </Stack>
  );
}
