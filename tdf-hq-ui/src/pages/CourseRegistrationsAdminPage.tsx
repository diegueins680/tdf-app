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
const emptyReceiptAlertMessage = 'El primer comprobante documenta el pago y habilita Marcar pagado. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const emptyReceiptEvidenceAlertMessage = 'El primer comprobante queda como evidencia de pago solo si hace falta documentarla. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const firstReceiptComposerHelpText = 'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y aparecerá aquí con enlace y acciones para revisarlo después.';
const receiptComposerHelpText = 'Este formulario ya está abierto para guardar otro comprobante o pegar un enlace existente.';
const editingReceiptComposerHelpText = 'Edita el comprobante y guarda los cambios para actualizar el registro.';
const initialEmptyStateConfigMessage = 'Todavía no hay inscripciones. Configura el primer formulario público de curso para empezar a recibirlas aquí.';
const buildInitialEmptyStateMultiCohortMessage = (count: number) =>
  `Todavía no hay inscripciones. Hay ${count} formularios públicos listos; revisa cursos para compartir uno.`;
const initialEmptyStateConfigActionLabel = 'Configurar cursos';
const initialEmptyStateMultiCohortActionLabel = 'Revisar cursos';
const initialEmptyStateFormActionLabel = 'Abrir formulario público';
const initialRegistrationLoadingMessage = 'Cargando inscripciones…';
const initialCohortResolutionMessage = 'Revisando formularios de curso para mostrar el siguiente paso.';
const initialCohortErrorMessage = 'No se pudieron cargar los formularios de curso. Reintenta para elegir qué enlace compartir.';
const initialCohortRetryLabel = 'Reintentar formularios';
const cohortFilterUnavailableMessage = 'No se pudieron cargar cohortes. La lista sigue disponible; el filtro por curso volverá cuando se recupere esa información.';
const cohortFilterLoadingMessage = 'La lista ya está disponible; el filtro por curso aparecerá cuando terminen de cargar los formularios.';
const emptyCohortFilterMessage = 'La lista sigue disponible; configura cursos para habilitar el filtro por cohorte.';
const buildSingleCohortInitialEmptyStateMessage = (cohortLabel: string) =>
  `Todavía no hay inscripciones para ${cohortLabel}. Cuando llegue la primera podrás revisar pago, seguimiento y correos aquí.`;
type RegistrationIdentityKind = 'name' | 'contact' | 'record';
const buildCompactDossierScopeHint = (targetLabel: string) =>
  `Abre el expediente desde ${targetLabel}; usa Cambiar estado para acciones rápidas.`;
const buildDossierOnlyScopeHint = (targetLabel: string) =>
  `Abre el expediente desde ${targetLabel}; el estado abre acciones rápidas.`;
const emptyNotesHelperText = 'Aún no hay notas internas. Registra la primera solo cuando necesites dejar contexto, acuerdos o próximos pasos.';
const markPaidEmptyNotesHelperText = 'Agrega una nota solo si necesitas dejar contexto extra sobre este pago.';
const showSystemEmailsLabel = 'Ver correos del sistema';
const hideSystemEmailsLabel = 'Ocultar correos del sistema';
const retrySystemEmailsLabel = 'Reintentar correos';
const optionalDossierContextActionsFallbackLabel = 'Agregar contexto';
const optionalDossierNotesAndFollowUpActionsLabel = 'Agregar nota o seguimiento';
const optionalDossierNotesActionLabel = 'Agregar nota interna';
const optionalDossierFollowUpActionLabel = 'Agregar seguimiento manual';
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
const MIN_LOCAL_SEARCH_REGISTRATIONS = 8;
const MIN_DEFAULT_CSV_EXPORT_ROWS = MIN_LOCAL_SEARCH_REGISTRATIONS;
const MIN_PHONE_SEARCH_DIGITS = 4;
const MAX_LOCAL_SEARCH_PLACEHOLDER_TERMS = 4;
const MAX_LOCAL_SEARCH_QUERY_SUMMARY_LENGTH = 64;
const LOCAL_SEARCH_LABEL = 'Buscar inscripciones';
const LOAD_LIMIT_LABEL = 'Límite de carga';
const LOAD_LIMIT_HELPER_TEXT = 'Máximo de inscripciones cargadas en esta vista.';
const missingContactSummary = 'Sin correo ni teléfono';

const formatDossierContextActionsLabel = ({
  showInlineEmptyFollowUpAction,
  showInlineEmptyNotesAction,
}: {
  showInlineEmptyFollowUpAction: boolean;
  showInlineEmptyNotesAction: boolean;
}) => {
  if (showInlineEmptyNotesAction && showInlineEmptyFollowUpAction) {
    return optionalDossierNotesAndFollowUpActionsLabel;
  }
  if (showInlineEmptyNotesAction) return optionalDossierNotesActionLabel;
  if (showInlineEmptyFollowUpAction) return optionalDossierFollowUpActionLabel;
  return optionalDossierContextActionsFallbackLabel;
};

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
const manualFollowUpTypeOptions = ['note', 'call', 'whatsapp', 'email'] as const;

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

const normalizeBackendStatusToken = (status: string) =>
  status.trim().toLowerCase().replace(/[\s._/-]+/g, '_').replace(/^_+|_+$/g, '');

const normalizeStatusFilterAlias = (value: string): StatusFilter | null => {
  const normalized = normalizeBackendStatusToken(value);
  if (normalized === 'payment_pending') return 'pending_payment';
  if (normalized === 'canceled') return 'cancelled';
  return isStatusFilter(normalized) ? normalized : null;
};

const parseStatusFilter = (value: string | null): StatusFilter => {
  return normalizeStatusFilterAlias(value ?? '') ?? 'all';
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
const buildLoadedSearchScopeHint = (loadedCount: number) =>
  `Busca dentro de las ${formatRegistrationCountLabel(loadedCount)} cargadas.`;
const buildFullLocalSearchMatchHint = (loadedCount: number) =>
  loadedCount === 1
    ? 'La búsqueda coincide con la inscripción cargada.'
    : `La búsqueda coincide con las ${formatRegistrationCountLabel(loadedCount)} cargadas.`;
const cappedLocalSearchEmptyHint =
  'Aumenta el límite si el registro puede estar fuera del lote cargado.';

const spanishOrConnector = (term: string) => (/^h?o/i.test(term.trim()) ? 'u' : 'o');
const formatLocalSearchPlaceholder = (terms: readonly string[]) => {
  const visibleTerms = terms.length > MAX_LOCAL_SEARCH_PLACEHOLDER_TERMS
    ? [...terms.slice(0, MAX_LOCAL_SEARCH_PLACEHOLDER_TERMS - 1), 'otros datos']
    : terms;
  if (visibleTerms.length <= 1) return visibleTerms[0] ?? '';
  const lastTerm = visibleTerms[visibleTerms.length - 1] ?? '';
  const connector = spanishOrConnector(lastTerm);
  if (visibleTerms.length === 2) return `${visibleTerms[0]} ${connector} ${lastTerm}`;
  return `${visibleTerms.slice(0, -1).join(', ')} ${connector} ${lastTerm}`;
};

const normalizeLocalSearchText = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');
const normalizeVisibleLocalSearchInput = (value: string) => (
  value.trim().length === 0 ? '' : value
);
const normalizeLocalSearchDigits = (value: string) => value.replace(/\D/g, '');
const looksLikeShortPhoneSearch = (value: string, digits: string) => (
  digits.length > 0
  && digits.length < MIN_PHONE_SEARCH_DIGITS
  && /^[\d\s()+.-]+$/.test(value.trim())
);
const normalizeLocalSearchQuery = (value: string) => value.trim().replace(/\s+/g, ' ');
const formatLocalSearchQuerySummary = (value: string) => {
  const normalizedValue = normalizeLocalSearchQuery(value);

  if (normalizedValue.length <= MAX_LOCAL_SEARCH_QUERY_SUMMARY_LENGTH) {
    return normalizedValue;
  }

  const summaryPrefix = normalizedValue.slice(0, MAX_LOCAL_SEARCH_QUERY_SUMMARY_LENGTH + 1);
  const lastWordBoundary = summaryPrefix.lastIndexOf(' ');
  const compactPrefix = (
    lastWordBoundary > 0
      ? summaryPrefix.slice(0, lastWordBoundary)
      : normalizedValue.slice(0, MAX_LOCAL_SEARCH_QUERY_SUMMARY_LENGTH)
  ).trimEnd();

  return `${compactPrefix}...`;
};
const normalizeContactComparisonValue = (value: string | null | undefined) =>
  value?.trim().toLocaleLowerCase('es') ?? '';
const normalizePhoneComparisonValue = (value: string | null | undefined) => {
  const trimmedValue = value?.trim() ?? '';
  if (!/^\+?[\d\s().-]+$/.test(trimmedValue)) return '';

  const digits = normalizeLocalSearchDigits(trimmedValue);
  return digits.length >= 7 ? digits : '';
};
const phoneComparisonValuesMatch = (left: string | null | undefined, right: string | null | undefined) => {
  const leftDigits = normalizePhoneComparisonValue(left);
  const rightDigits = normalizePhoneComparisonValue(right);

  return Boolean(
    leftDigits
    && rightDigits
    && (leftDigits === rightDigits || leftDigits.endsWith(rightDigits) || rightDigits.endsWith(leftDigits)),
  );
};
const contactComparisonValuesMatch = (left: string | null | undefined, right: string | null | undefined) => {
  const normalizedLeft = normalizeContactComparisonValue(left);
  const normalizedRight = normalizeContactComparisonValue(right);

  return Boolean(normalizedLeft && normalizedRight && normalizedLeft === normalizedRight)
    || phoneComparisonValuesMatch(left, right);
};
const visibleRegistrationContactParts = (
  contactParts: readonly string[],
  visibleIdentityValues: readonly string[] = [],
) => contactParts.filter((part) => (
  !visibleIdentityValues.some((identityValue) => contactComparisonValuesMatch(identityValue, part))
));

const formatDate = (iso: string | null | undefined) => formatTimestampForDisplay(iso, '-');
const formatOptionalDate = (iso: string | null | undefined) => {
  const formatted = formatDate(iso);
  return formatted === '-' ? '' : formatted;
};
const getSharedOptionalDateLabel = (values: readonly (string | null | undefined)[]) => {
  if (values.length < 2) return '';
  const labels = values.map(formatOptionalDate);
  const [firstLabel] = labels;
  if (!firstLabel || labels.some((label) => label === '')) return '';
  return labels.every((label) => label === firstLabel) ? firstLabel : '';
};

type RegistrationStatus = Exclude<StatusFilter, 'all'>;

const normalizeRegistrationStatusKey = (status: string) =>
  normalizeBackendStatusToken(status);

const normalizeKnownRegistrationStatus = (status: string): RegistrationStatus | null => {
  const statusFilter = normalizeStatusFilterAlias(status);
  return statusFilter && statusFilter !== 'all' ? statusFilter : null;
};

const customRegistrationStatusLabel = (status: string) => {
  const normalized = status.trim().toLowerCase().replace(/[\s._/-]+/g, ' ').trim();
  if (!normalized) return 'Estado desconocido';
  return normalized.replace(/\b\w/g, (match) => match.toUpperCase());
};

const registrationStatusLabel = (status: string) => {
  const knownStatus = normalizeKnownRegistrationStatus(status);
  return knownStatus ? statusFilterLabels[knownStatus] : customRegistrationStatusLabel(status);
};

const registrationStatusButtonLabel = (
  status: string,
  useCompactActionLabel: boolean,
) => (useCompactActionLabel ? 'Cambiar estado' : registrationStatusLabel(status));

const registrationStatusChipColor = (
  status: string,
): 'default' | 'success' | 'warning' | 'error' => {
  const knownStatus = normalizeKnownRegistrationStatus(status);
  if (knownStatus === 'paid') return 'success';
  if (knownStatus === 'cancelled') return 'error';
  if (knownStatus === 'pending_payment') return 'warning';
  return 'default';
};

const registrationStatusButtonColor = (
  status: string,
): 'inherit' | 'success' | 'warning' | 'error' => {
  const knownStatus = normalizeKnownRegistrationStatus(status);
  if (knownStatus === 'paid') return 'success';
  if (knownStatus === 'cancelled') return 'error';
  if (knownStatus === 'pending_payment') return 'warning';
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

const statusFilterChipAriaLabel = (status: StatusFilter, isActive: boolean) => {
  const statusLabel = statusFilterLabels[status];
  return isActive
    ? `Quitar filtro de estado ${statusLabel}`
    : `Filtrar inscripciones por estado ${statusLabel}`;
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
  nextStatus: RegistrationStatus,
) => {
  const knownStatus = normalizeKnownRegistrationStatus(currentStatus);
  if (!knownStatus) return true;
  return knownStatus !== nextStatus;
};

const canOpenPaymentWorkflowFromStatus = (currentStatus: string) =>
  normalizeKnownRegistrationStatus(currentStatus) === 'pending_payment';

const pendingStatusMenuLabel = (currentStatus: string) =>
  normalizeKnownRegistrationStatus(currentStatus) === 'cancelled' ? 'Reabrir como pendiente' : 'Marcar pendiente';

const pendingStatusMenuTargetLabel = (currentStatus: string) =>
  normalizeKnownRegistrationStatus(currentStatus) === 'cancelled'
    ? 'reabrir la inscripción como pendiente'
    : 'marcarla pendiente';

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

const getFollowUpTypeOptions = (entryType: string) => {
  const normalizedEntryType = entryType.trim().toLowerCase();
  if (
    !normalizedEntryType
    || manualFollowUpTypeOptions.some((option) => option === normalizedEntryType)
  ) {
    return manualFollowUpTypeOptions;
  }

  return [...manualFollowUpTypeOptions, normalizedEntryType];
};

const followUpSubjectLabel = (entry: Pick<CourseRegistrationFollowUpDTO, 'crfSubject'>) =>
  entry.crfSubject?.trim() ?? '';

const followUpActionTargetLabel = (entry: CourseRegistrationFollowUpDTO) =>
  followUpSubjectLabel(entry) || `${eventTypeLabel(entry.crfEntryType)} del ${formatDate(entry.crfCreatedAt)}`;

const normalizeFollowUpActionTargetKey = (entry: CourseRegistrationFollowUpDTO) =>
  followUpActionTargetLabel(entry).toLocaleLowerCase('es');

const getFollowUpIdsRequiringActionDisambiguator = (
  followUps: readonly CourseRegistrationFollowUpDTO[],
) => {
  const labelCounts = new Map<string, number>();

  followUps.forEach((entry) => {
    const labelKey = normalizeFollowUpActionTargetKey(entry);
    labelCounts.set(labelKey, (labelCounts.get(labelKey) ?? 0) + 1);
  });

  return new Set(
    followUps
      .filter((entry) => (labelCounts.get(normalizeFollowUpActionTargetKey(entry)) ?? 0) > 1)
      .map((entry) => entry.crfId),
  );
};

const followUpActionTargetLabelWithContext = (
  entry: CourseRegistrationFollowUpDTO,
  needsDisambiguator: boolean,
) => {
  const label = followUpActionTargetLabel(entry);
  return needsDisambiguator ? `${label} · #${entry.crfId}` : label;
};

const receiptDisplayLabel = (receipt: CourseRegistrationReceiptDTO) => {
  const fileName = receipt.crrFileName?.trim();
  if (fileName) return fileName;
  return `Comprobante #${receipt.crrId}`;
};

const normalizeReceiptDisplayLabelKey = (receipt: CourseRegistrationReceiptDTO) =>
  receiptDisplayLabel(receipt).toLocaleLowerCase('es');

const getReceiptIdsRequiringFileDisambiguator = (
  receipts: readonly CourseRegistrationReceiptDTO[],
) => {
  const labelCounts = new Map<string, number>();

  receipts.forEach((receipt) => {
    const labelKey = normalizeReceiptDisplayLabelKey(receipt);
    labelCounts.set(labelKey, (labelCounts.get(labelKey) ?? 0) + 1);
  });

  return new Set(
    receipts
      .filter((receipt) => (labelCounts.get(normalizeReceiptDisplayLabelKey(receipt)) ?? 0) > 1)
      .map((receipt) => receipt.crrId),
  );
};

const receiptDisplayLabelWithContext = (
  receipt: CourseRegistrationReceiptDTO,
  needsDisambiguator: boolean,
) => {
  const label = receiptDisplayLabel(receipt);
  return needsDisambiguator ? `${label} · #${receipt.crrId}` : label;
};

const normalizeCohortLabelKey = (value: string) =>
  value.trim().toLocaleLowerCase('es');

const escapeRegExp = (value: string) => value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

const stripTrailingCohortSlug = (title: string, slug: string) => {
  const trimmedTitle = title.trim();
  const trimmedSlug = slug.trim();
  if (!trimmedTitle || !trimmedSlug) return trimmedTitle;

  const escapedSlug = escapeRegExp(trimmedSlug);
  const suffixPattern = new RegExp(
    `\\s*(?:\\(${escapedSlug}\\)|\\[${escapedSlug}\\]|[-:/|]\\s*${escapedSlug})\\s*$`,
    'i',
  );
  const strippedTitle = trimmedTitle.replace(suffixPattern, '').trim();
  return strippedTitle || trimmedSlug;
};

const stripFirstRunCohortDescriptorPrefix = (title: string) => {
  const trimmedTitle = title.trim();
  const strippedTitle = trimmedTitle
    .replace(
      /^(?:formulario\s+(?:p[uú]blico|de\s+inscripci[oó]n|de\s+registro)|public\s+form|registration\s+form|landing\s+(?:del\s+curso|de\s+curso|de\s+inscripci[oó]n|de\s+registro|para\s+el|para|del|de)|course\s+landing(?:\s+page)?|landing\s+page)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i,
      '',
    )
    .trim();
  const strippedCourseNoun = strippedTitle === trimmedTitle
    ? strippedTitle
    : strippedTitle
      .replace(/^(?:curso|course)\s*(?:[-:/|]\s*)?/i, '')
      .trim();

  return strippedCourseNoun || strippedTitle || trimmedTitle;
};

const cohortOptionLabel = (cohort: CourseCohortOptionDTO) => {
  const slug = cohort.ccSlug.trim();
  const title = cohort.ccTitle?.trim();
  if (!title || normalizeCohortLabelKey(title) === normalizeCohortLabelKey(slug)) return slug;
  if (stripTrailingCohortSlug(title, slug) !== title) return title;
  return `${title} (${slug})`;
};

const cohortFirstRunLabel = (cohort: CourseCohortOptionDTO) => {
  const slug = cohort.ccSlug.trim();
  const title = cohort.ccTitle?.trim();
  if (!title) return slug;
  return stripTrailingCohortSlug(stripFirstRunCohortDescriptorPrefix(title), slug);
};

const humanizeDelimitedSourceLabel = (source: string) => {
  if (!/[_./-]/.test(source)) return source;
  const normalized = source.replace(/[_./-]+/g, ' ').replace(/\s+/g, ' ').trim();
  if (!normalized) return source;
  const sentenceCaseSource = normalized.toLocaleLowerCase('es');
  return `${sentenceCaseSource.charAt(0).toLocaleUpperCase('es')}${sentenceCaseSource.slice(1)}`;
};

const normalizeSourceAliasKey = (source: string) =>
  normalizeLocalSearchText(humanizeDelimitedSourceLabel(source));

const defaultPublicFormSourceKeys = new Set([
  defaultPublicFormSource,
  'public form',
  'formulario publico',
  'registration form',
  'formulario de inscripcion',
  'formulario de registro',
].map(normalizeSourceAliasKey));

const registrationSourceLabel = (source: string | null | undefined) => {
  const trimmed = source?.trim() ?? '';
  return trimmed === '' ? 'Sin fuente' : humanizeDelimitedSourceLabel(trimmed);
};

const normalizeRegistrationSourceKey = (sourceLabel: string) =>
  registrationSourceLabel(sourceLabel).toLocaleLowerCase('es');

const isDefaultPublicFormSource = (sourceLabel: string) =>
  defaultPublicFormSourceKeys.has(normalizeSourceAliasKey(sourceLabel));

const getSearchableRegistrationSource = (source: string | null | undefined) => {
  const trimmedSource = source?.trim() ?? '';
  if (!trimmedSource || isDefaultPublicFormSource(trimmedSource)) return '';
  const displayLabel = registrationSourceLabel(trimmedSource);
  return displayLabel === trimmedSource ? trimmedSource : `${trimmedSource} ${displayLabel}`;
};

const getSearchableRegistrationAcquisitionContext = (
  reg: Pick<
    CourseRegistrationDTO,
    'crHowHeard' | 'crUtmSource' | 'crUtmMedium' | 'crUtmCampaign' | 'crUtmContent'
  >,
) => (
  [
    reg.crHowHeard,
    reg.crUtmSource,
    reg.crUtmMedium,
    reg.crUtmCampaign,
    reg.crUtmContent,
  ]
    .map((value) => {
      const trimmedValue = value?.trim() ?? '';
      if (!trimmedValue) return '';
      const displayLabel = humanizeDelimitedSourceLabel(trimmedValue);
      return displayLabel === trimmedValue ? trimmedValue : `${trimmedValue} ${displayLabel}`;
    })
    .filter(Boolean)
    .join(' ')
);

type HiddenLocalSearchField = 'note' | 'origin';

const hiddenLocalSearchFieldLabels: Record<HiddenLocalSearchField, string> = {
  note: 'nota interna',
  origin: 'origen o campaña',
};

const formatHiddenLocalSearchFieldList = (fields: readonly HiddenLocalSearchField[]) => {
  const labels = fields.map((field) => hiddenLocalSearchFieldLabels[field]);
  if (labels.length <= 1) return labels[0] ?? '';
  return `${labels.slice(0, -1).join(', ')} y ${labels[labels.length - 1]}`;
};

const localSearchTextMatches = (value: string | null | undefined, localSearchKey: string) =>
  Boolean(localSearchKey) && normalizeLocalSearchText(value ?? '').includes(localSearchKey);

const registrationVisibleSearchText = (
  reg: CourseRegistrationDTO,
  cohortLabelsBySlug: ReadonlyMap<string, string>,
) => {
  const courseSlug = reg.crCourseSlug.trim();
  return [
    reg.crFullName,
    reg.crEmail,
    reg.crPhoneE164,
    `registro #${reg.crId}`,
    String(reg.crId),
    courseSlug,
    cohortLabelsBySlug.get(courseSlug),
    registrationStatusLabel(reg.crStatus),
    getSearchableRegistrationSource(reg.crSource),
  ].join(' ');
};

const registrationMatchesVisibleSearchFields = ({
  cohortLabelsBySlug,
  localSearchDigitsKey,
  localSearchKey,
  reg,
}: {
  cohortLabelsBySlug: ReadonlyMap<string, string>;
  localSearchDigitsKey: string;
  localSearchKey: string;
  reg: CourseRegistrationDTO;
}) => {
  if (localSearchTextMatches(registrationVisibleSearchText(reg, cohortLabelsBySlug), localSearchKey)) {
    return true;
  }

  if (localSearchDigitsKey.length < MIN_PHONE_SEARCH_DIGITS) {
    return false;
  }

  return normalizeLocalSearchDigits(reg.crPhoneE164 ?? '').includes(localSearchDigitsKey);
};

const hiddenLocalSearchFieldsForRegistration = (
  reg: CourseRegistrationDTO,
  localSearchKey: string,
): HiddenLocalSearchField[] => {
  const fields: HiddenLocalSearchField[] = [];

  if (localSearchTextMatches(reg.crAdminNotes, localSearchKey)) {
    fields.push('note');
  }

  if (localSearchTextMatches(getSearchableRegistrationAcquisitionContext(reg), localSearchKey)) {
    fields.push('origin');
  }

  return fields;
};

const buildHiddenLocalSearchMatchSummary = ({
  cohortLabelsBySlug,
  localSearchDigitsKey,
  localSearchKey,
  registrations,
}: {
  cohortLabelsBySlug: ReadonlyMap<string, string>;
  localSearchDigitsKey: string;
  localSearchKey: string;
  registrations: readonly CourseRegistrationDTO[];
}) => {
  if (!localSearchKey || registrations.length === 0) return '';

  const hiddenFields = new Set<HiddenLocalSearchField>();
  let hiddenOnlyMatchCount = 0;

  registrations.forEach((reg) => {
    if (registrationMatchesVisibleSearchFields({
      cohortLabelsBySlug,
      localSearchDigitsKey,
      localSearchKey,
      reg,
    })) {
      return;
    }

    const fields = hiddenLocalSearchFieldsForRegistration(reg, localSearchKey);
    if (fields.length === 0) return;

    hiddenOnlyMatchCount += 1;
    fields.forEach((field) => hiddenFields.add(field));
  });

  if (hiddenOnlyMatchCount === 0) return '';

  const fieldList = formatHiddenLocalSearchFieldList([...hiddenFields]);
  if (!fieldList) return '';

  if (hiddenOnlyMatchCount === registrations.length) {
    return `${registrations.length === 1 ? 'Coincide' : 'Coinciden'} con ${fieldList}.`;
  }

  return `Algunas coincidencias vienen de ${fieldList}.`;
};

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
      secondary: registrationContactSummary(trimmedEmail, trimmedPhone, [trimmedName]),
    };
  }

  if (trimmedEmail) {
    return {
      primary: trimmedEmail,
      secondary: visibleRegistrationContactParts([trimmedPhone].filter(Boolean), [trimmedEmail]).join(' · '),
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
    secondary: missingContactSummary,
  };
};

const registrationIdentityKind = (
  reg: Pick<CourseRegistrationDTO, 'crFullName' | 'crEmail' | 'crPhoneE164'>,
): RegistrationIdentityKind => {
  if (reg.crFullName?.trim()) return 'name';
  if (reg.crEmail?.trim() || reg.crPhoneE164?.trim()) return 'contact';
  return 'record';
};

const namedRegistrationNeedsContact = (
  reg: Pick<CourseRegistrationDTO, 'crFullName' | 'crEmail' | 'crPhoneE164'>,
) => (
  Boolean(reg.crFullName?.trim())
  && !reg.crEmail?.trim()
  && !reg.crPhoneE164?.trim()
);

const formatVisibleMissingContactSummary = (missingContactCount: number, visibleCount: number) => {
  if (visibleCount <= 1 || missingContactCount === 0) return '';

  if (missingContactCount === visibleCount) {
    return 'Contacto pendiente en todas las inscripciones visibles.';
  }

  const countLabel = missingContactCount === 1 ? 'inscripción visible' : 'inscripciones visibles';
  return `${missingContactCount} ${countLabel} con contacto pendiente.`;
};

const registrationIdentityTargetLabel = (registrations: readonly CourseRegistrationDTO[]) => {
  const identityKinds = new Set(registrations.map(registrationIdentityKind));
  if (identityKinds.size === 1) {
    const [kind] = Array.from(identityKinds);
    if (kind === 'contact') return 'el contacto';
    if (kind === 'record') return 'el registro';
  }
  if (identityKinds.size > 1) return 'el dato principal de cada fila';
  return 'el nombre';
};

const registrationContactSummary = (
  email: string | null | undefined,
  phone: string | null | undefined,
  visibleIdentityValues: readonly string[] = [],
) => {
  const trimmedEmail = email?.trim() ?? '';
  const trimmedPhone = phone?.trim() ?? '';
  const parts = [trimmedEmail, trimmedPhone].filter((value) => value !== '');
  if (parts.length === 0) return missingContactSummary;
  return visibleRegistrationContactParts(parts, visibleIdentityValues).join(' · ');
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

const getRegistrationIdsRequiringActionRecordDisambiguator = (
  registrations: readonly CourseRegistrationDTO[],
) => {
  const contextualLabelCounts = new Map<string, number>();

  registrations.forEach((reg) => {
    const identity = registrationIdentityDisplay(reg.crFullName, reg.crEmail, reg.crPhoneE164, reg.crId);
    const secondary = identity.secondary.trim();

    if (!secondary || secondary === missingContactSummary) {
      return;
    }

    const contextualLabelKey = normalizeLocalSearchText(`${registrationActionTargetLabel(reg)} (${secondary})`);
    contextualLabelCounts.set(contextualLabelKey, (contextualLabelCounts.get(contextualLabelKey) ?? 0) + 1);
  });

  return new Set(
    registrations
      .filter((reg) => {
        const identity = registrationIdentityDisplay(reg.crFullName, reg.crEmail, reg.crPhoneE164, reg.crId);
        const secondary = identity.secondary.trim();

        if (!secondary || secondary === missingContactSummary) {
          return false;
        }

        const contextualLabelKey = normalizeLocalSearchText(`${registrationActionTargetLabel(reg)} (${secondary})`);
        return (contextualLabelCounts.get(contextualLabelKey) ?? 0) > 1;
      })
      .map((reg) => reg.crId),
  );
};

const registrationActionTargetLabelWithContext = (
  reg: Pick<CourseRegistrationDTO, 'crId' | 'crFullName' | 'crEmail' | 'crPhoneE164'>,
  needsDisambiguator: boolean,
  needsRecordDisambiguator: boolean,
) => {
  const baseLabel = registrationActionTargetLabel(reg);
  if (!needsDisambiguator) return baseLabel;

  const identity = registrationIdentityDisplay(reg.crFullName, reg.crEmail, reg.crPhoneE164, reg.crId);
  const secondary = identity.secondary.trim();
  if (secondary && secondary !== missingContactSummary) {
    const disambiguatingContext = needsRecordDisambiguator
      ? `${secondary} · registro #${reg.crId}`
      : secondary;
    return `${baseLabel} (${disambiguatingContext})`;
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

const hasSearchableCustomRegistrationStatus = (registrations: readonly CourseRegistrationDTO[]) => {
  const statusKeys = new Set<string>();
  let hasCustomStatus = false;

  registrations.forEach((reg) => {
    const statusLabel = registrationStatusLabel(reg.crStatus).trim();
    if (statusLabel) statusKeys.add(normalizeLocalSearchText(statusLabel));
    if (!normalizeKnownRegistrationStatus(reg.crStatus)) hasCustomStatus = true;
  });

  return hasCustomStatus && statusKeys.size > 1;
};

const buildLocalSearchPlaceholder = (registrations: readonly CourseRegistrationDTO[]) => {
  const sourceKeys = new Set<string>();
  const cohortKeys = new Set<string>();
  let hasNameIdentity = false;
  let hasEmailIdentity = false;
  let hasPhoneIdentity = false;
  let hasGeneratedRegistrationIdentity = false;
  let hasRowsWithoutNotes = false;
  let hasHiddenDefaultOrEmptySource = false;
  let hasHiddenAcquisitionContext = false;
  const noteKeys = new Set<string>();
  const acquisitionContextKeys = new Set<string>();

  registrations.forEach((reg) => {
    const hasName = Boolean(reg.crFullName?.trim());
    const hasEmail = Boolean(reg.crEmail?.trim());
    const hasPhone = Boolean(reg.crPhoneE164?.trim());
    const hasContact = hasEmail || hasPhone;

    if (hasName) hasNameIdentity = true;
    if (hasEmail) hasEmailIdentity = true;
    if (hasPhone) hasPhoneIdentity = true;
    if (!hasName && !hasContact) {
      hasGeneratedRegistrationIdentity = true;
    }

    const searchableSource = getSearchableRegistrationSource(reg.crSource);
    if (searchableSource) {
      sourceKeys.add(normalizeRegistrationSourceKey(reg.crSource ?? ''));
    } else {
      hasHiddenDefaultOrEmptySource = true;
    }

    const cohortKey = reg.crCourseSlug.trim().toLocaleLowerCase('es');
    if (cohortKey) cohortKeys.add(cohortKey);

    const noteKey = normalizeLocalSearchText(reg.crAdminNotes ?? '');
    if (noteKey) {
      noteKeys.add(noteKey);
    } else {
      hasRowsWithoutNotes = true;
    }

    const acquisitionContext = getSearchableRegistrationAcquisitionContext(reg);
    if (acquisitionContext) {
      acquisitionContextKeys.add(normalizeLocalSearchText(acquisitionContext));
    } else {
      hasHiddenAcquisitionContext = true;
    }
  });

  const terms: string[] = [];
  if (hasNameIdentity) terms.push('Nombre');
  if (hasEmailIdentity || hasPhoneIdentity) {
    const contactTerm = hasEmailIdentity && hasPhoneIdentity
      ? 'contacto'
      : hasEmailIdentity
        ? 'correo'
        : 'teléfono';
    const capitalizedContactTerm = `${contactTerm.charAt(0).toLocaleUpperCase('es')}${contactTerm.slice(1)}`;
    terms.push(hasNameIdentity ? contactTerm : capitalizedContactTerm);
  }
  if (hasGeneratedRegistrationIdentity) terms.push(terms.length === 0 ? 'Registro' : 'registro');
  if (noteKeys.size > 1 || (noteKeys.size === 1 && hasRowsWithoutNotes)) terms.push('nota');
  if (acquisitionContextKeys.size > 1 || (acquisitionContextKeys.size === 1 && hasHiddenAcquisitionContext)) {
    terms.push(terms.length === 0 ? 'Origen' : 'origen');
  }
  if (hasSearchableCustomRegistrationStatus(registrations)) terms.push('estado');
  if (sourceKeys.size > 1 || (sourceKeys.size === 1 && hasHiddenDefaultOrEmptySource)) terms.push('fuente');
  if (cohortKeys.size > 1) terms.push('curso');

  return formatLocalSearchPlaceholder(terms);
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
    parts.push(`Fuente: ${registrationSourceLabel(trimmedSource)}`);
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
  if (trimmedFallback) return trimmedFallback;
  return null;
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
  crStatus: preferNonEmptyText(primary.crStatus, fallback.crStatus) ?? primary.crStatus,
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
  const receiptsById = new Map<number, CourseRegistrationReceiptDTO>();

  receipts.forEach((receipt) => {
    const existingReceipt = receiptsById.get(receipt.crrId);
    if (!existingReceipt) {
      receiptsById.set(receipt.crrId, receipt);
      return;
    }

    receiptsById.set(receipt.crrId, {
      ...existingReceipt,
      crrPartyId: preferPositiveId(existingReceipt.crrPartyId, receipt.crrPartyId),
      crrFileUrl: preferNonEmptyText(existingReceipt.crrFileUrl, receipt.crrFileUrl) ?? existingReceipt.crrFileUrl,
      crrFileName: preferNonEmptyText(existingReceipt.crrFileName, receipt.crrFileName),
      crrMimeType: preferNonEmptyText(existingReceipt.crrMimeType, receipt.crrMimeType),
      crrNotes: preferNonEmptyText(existingReceipt.crrNotes, receipt.crrNotes),
      crrUploadedBy: preferPositiveId(existingReceipt.crrUploadedBy, receipt.crrUploadedBy),
      crrCreatedAt: preferNonEmptyText(existingReceipt.crrCreatedAt, receipt.crrCreatedAt) ?? existingReceipt.crrCreatedAt,
      crrUpdatedAt: preferNonEmptyText(existingReceipt.crrUpdatedAt, receipt.crrUpdatedAt) ?? existingReceipt.crrUpdatedAt,
    });
  });

  return [...receiptsById.values()];
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
  const [dossierContextMenuAnchor, setDossierContextMenuAnchor] = useState<HTMLElement | null>(null);
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
    const options: { value: string; label: string; firstRunLabel: string }[] = [];
    const seenSlugs = new Set<string>();

    for (const cohort of cohortsQuery.data ?? []) {
      const cohortSlug = cohort.ccSlug.trim();
      if (!cohortSlug || seenSlugs.has(cohortSlug)) continue;
      seenSlugs.add(cohortSlug);
      options.push({
        value: cohortSlug,
        label: cohortOptionLabel(cohort),
        firstRunLabel: cohortFirstRunLabel(cohort),
      });
    }

    return options;
  }, [cohortsQuery.data]);
  const singleAvailableCohort = useMemo(() => {
    if (cohortsQuery.isError || configuredCohortOptions.length !== 1) return null;
    const [onlyConfiguredCohort] = configuredCohortOptions;
    if (!onlyConfiguredCohort) return null;
    return !selectedSlug || selectedSlug === onlyConfiguredCohort.value ? onlyConfiguredCohort : null;
  }, [cohortsQuery.isError, configuredCohortOptions, selectedSlug]);
  const selectedConfiguredCohort = useMemo(() => {
    if (cohortsQuery.isError || !selectedSlug) return null;
    return configuredCohortOptions.find((option) => option.value === selectedSlug) ?? null;
  }, [cohortsQuery.isError, configuredCohortOptions, selectedSlug]);

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
  const hasVisibleRegistrations = registrations.length > 0;
  const visibleCohortSlugs = useMemo(() => new Set(
    registrations
      .map((reg) => reg.crCourseSlug.trim())
      .filter((value) => value !== ''),
  ), [registrations]);
  const singleAvailableCohortLabel = useMemo(() => {
    if (!singleAvailableCohort) return '';
    if (!hasVisibleRegistrations || selectedSlug === singleAvailableCohort.value) {
      return singleAvailableCohort.label;
    }
    return visibleCohortSlugs.size === 1 && visibleCohortSlugs.has(singleAvailableCohort.value)
      ? singleAvailableCohort.label
      : '';
  }, [hasVisibleRegistrations, selectedSlug, singleAvailableCohort, visibleCohortSlugs]);

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
        const knownStatus = normalizeKnownRegistrationStatus(reg.crStatus);
        acc.total += 1;
        if (knownStatus) {
          acc[knownStatus] += 1;
        }
        return acc;
      },
      { ...base },
    );
  }, [registrations]);
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
    if (!hasVisibleRegistrations || status !== 'all') return null;
    const statusesByKey = new Map<string, string>();

    registrations.forEach((reg) => {
      const trimmedStatus = reg.crStatus.trim();
      const statusKey = normalizeKnownRegistrationStatus(trimmedStatus)
        ?? normalizeRegistrationStatusKey(trimmedStatus);
      if (!statusesByKey.has(statusKey)) {
        statusesByKey.set(statusKey, trimmedStatus);
      }
    });

    const [onlyStatus] = Array.from(statusesByKey.values());
    if (statusesByKey.size !== 1 || onlyStatus == null) return null;
    return !normalizeKnownRegistrationStatus(onlyStatus) ? onlyStatus : null;
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
  const showPassiveSingleCohortLimitEmptyState = !hasVisibleRegistrations
    && Boolean(singleAvailableCohort)
    && hasRedundantSingleCohortFilter
    && hasCustomLimit
    && !hasStatusFilter;
  const showSelectedCohortFirstRunEmptyState = !hasVisibleRegistrations
    && Boolean(selectedConfiguredCohort)
    && hasSlugFilter
    && !hasStatusFilter;
  const showCohortFilterUnavailableSummary = cohortsQuery.isError && hasVisibleRegistrations && !hasSlugFilter;
  const activeFilterSummary = useMemo(
    () => summarizeActiveFilters({
      cohortLabel: hasEffectiveSlugFilter ? activeCohortLabel : '',
      status,
      limit,
    }),
    [activeCohortLabel, hasEffectiveSlugFilter, status, limit],
  );
  const combinedSingleChoiceSummary = singleAvailableCohortLabel && showSingleStatusSummary && singleVisibleStatus
    ? `${singleAvailableCohortLabel} · ${statusFilterLabels[singleVisibleStatus]}`
    : '';
  const loadedRegistrationCount = registrations.length;
  const viewHitsCurrentLimit = hasVisibleRegistrations && loadedRegistrationCount >= limit;
  const showFilterOnboardingCopy = !hasUsedRowAction && !hasUsedFilterControl;
  const dossierIdentityTargetLabel = registrationIdentityTargetLabel(registrations);
  const localSearchTerm = normalizeLocalSearchQuery(localSearch);
  const localSearchSummary = formatLocalSearchQuerySummary(localSearch);
  const localSearchKey = normalizeLocalSearchText(localSearchTerm);
  const localSearchDigitsKey = normalizeLocalSearchDigits(localSearchTerm);
  const hasLocalSearch = Boolean(localSearchKey);
  const searchedRegistrations = useMemo(() => {
    if (!localSearchKey) return registrations;
    return registrations.filter((reg) => {
      const courseSlug = reg.crCourseSlug.trim();
      const haystack = [
        reg.crFullName,
        reg.crEmail,
        reg.crPhoneE164,
        `registro #${reg.crId}`,
        String(reg.crId),
        reg.crAdminNotes,
        courseSlug,
        cohortLabelsBySlug.get(courseSlug),
        registrationStatusLabel(reg.crStatus),
        getSearchableRegistrationSource(reg.crSource),
        getSearchableRegistrationAcquisitionContext(reg),
      ].join(' ');
      const searchableText = normalizeLocalSearchText(haystack);
      if (searchableText.includes(localSearchKey)) return true;

      if (localSearchDigitsKey.length >= MIN_PHONE_SEARCH_DIGITS) {
        const phoneDigits = normalizeLocalSearchDigits(reg.crPhoneE164 ?? '');
        return phoneDigits.includes(localSearchDigitsKey);
      }

      return false;
    });
  }, [cohortLabelsBySlug, localSearchDigitsKey, localSearchKey, registrations]);
  const registrationIdsRequiringActionDisambiguator = useMemo(
    () => getRegistrationIdsRequiringActionDisambiguator(searchedRegistrations),
    [searchedRegistrations],
  );
  const registrationIdsRequiringActionRecordDisambiguator = useMemo(
    () => getRegistrationIdsRequiringActionRecordDisambiguator(searchedRegistrations),
    [searchedRegistrations],
  );
  const actionTargetLabelsByRegistrationId = useMemo(
    () => new Map(
      searchedRegistrations.map((reg) => [
        reg.crId,
        registrationActionTargetLabelWithContext(
          reg,
          registrationIdsRequiringActionDisambiguator.has(reg.crId),
          registrationIdsRequiringActionRecordDisambiguator.has(reg.crId),
        ),
      ]),
    ),
    [
      registrationIdsRequiringActionDisambiguator,
      registrationIdsRequiringActionRecordDisambiguator,
      searchedRegistrations,
    ],
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
  const singleSearchedStatusLabel = useMemo(() => {
    if (searchedRegistrations.length < 2) return '';
    const statusLabelsByKey = new Map<string, string>();

    searchedRegistrations.forEach((reg) => {
      const trimmedStatus = reg.crStatus.trim();
      const statusKey = normalizeKnownRegistrationStatus(trimmedStatus)
        ?? normalizeRegistrationStatusKey(trimmedStatus);
      if (!statusLabelsByKey.has(statusKey)) {
        statusLabelsByKey.set(statusKey, trimmedStatus);
      }
    });

    const [onlyStatus] = Array.from(statusLabelsByKey.values());
    return statusLabelsByKey.size === 1 && onlyStatus ? registrationStatusLabel(onlyStatus) : '';
  }, [searchedRegistrations]);
  const sharedVisibleSourceSummary = hasNamedVisibleSource
    ? `Mostrando una sola fuente: ${singleVisibleSourceLabel}.`
    : '';
  const showEmptyLocalSearchResults = hasLocalSearch
    && loadedRegistrationCount > 0
    && searchedRegistrations.length === 0;
  const showDefaultEmptyLocalSearchFocus = showEmptyLocalSearchResults
    && !hasCustomFilters
    && !viewHitsCurrentLimit;
  const localSearchNarrowsRegistrations = hasLocalSearch && searchedRegistrations.length < loadedRegistrationCount;
  const singleVisibleNamedRegistrationNeedsContact = searchedRegistrations.length === 1
    && searchedRegistrations[0] != null
    && namedRegistrationNeedsContact(searchedRegistrations[0]);
  const singleVisibleMissingContactSummary = singleVisibleNamedRegistrationNeedsContact
    ? 'Contacto pendiente en esta inscripción.'
    : '';
  const hiddenLocalSearchMatchSummary = useMemo(
    () => buildHiddenLocalSearchMatchSummary({
      cohortLabelsBySlug,
      localSearchDigitsKey,
      localSearchKey,
      registrations: searchedRegistrations,
    }),
    [cohortLabelsBySlug, localSearchDigitsKey, localSearchKey, searchedRegistrations],
  );
  const shortPhoneSearchHint = looksLikeShortPhoneSearch(localSearchTerm, localSearchDigitsKey)
    ? `Para buscar por teléfono, usa al menos ${MIN_PHONE_SEARCH_DIGITS} dígitos del número.`
    : '';
  const showLocalSearchControl = loadedRegistrationCount >= MIN_LOCAL_SEARCH_REGISTRATIONS || Boolean(localSearchKey);
  const localSearchPlaceholder = useMemo(
    () => buildLocalSearchPlaceholder(registrations),
    [registrations],
  );
  const hasCustomStatusSearch = useMemo(
    () => hasSearchableCustomRegistrationStatus(registrations),
    [registrations],
  );
  const localSearchOnboardingActionHint = showFilterOnboardingCopy
    ? ` ${buildDossierOnlyScopeHint(dossierIdentityTargetLabel)}`
    : '';
  const localSearchHelperText = localSearchKey
    ? showEmptyLocalSearchResults
      ? undefined
      : localSearchNarrowsRegistrations
        ? [
          formatLocalSearchResultSummary(searchedRegistrations.length, loadedRegistrationCount),
          singleVisibleMissingContactSummary,
          hiddenLocalSearchMatchSummary,
        ].filter(Boolean).join(' ')
        : loadedRegistrationCount > 0
          ? buildFullLocalSearchMatchHint(loadedRegistrationCount)
          : undefined
    : viewHitsCurrentLimit
      ? `${buildLoadedSearchScopeHint(loadedRegistrationCount)}${localSearchOnboardingActionHint}`
      : `Busca dentro de las ${formatRegistrationCountLabel(loadedRegistrationCount)} cargadas sin cambiar filtros.${localSearchOnboardingActionHint}`;
  const emptyLocalSearchResultsMessage = showEmptyLocalSearchResults
    ? [
      shortPhoneSearchHint
        || `No hay coincidencias para "${localSearchSummary}" en las ${formatRegistrationCountLabel(loadedRegistrationCount)} cargadas.`,
      shortPhoneSearchHint ? '' : viewHitsCurrentLimit ? cappedLocalSearchEmptyHint : '',
    ].filter(Boolean).join(' ')
    : '';
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
  const showSingleStatusSummaryBlock = showSingleStatusSummary
    && !(showCohortFilterUnavailableSummary && loadedRegistrationCount === 1);
  const standaloneSingleChoiceSourceSummary = !combinedSingleChoiceSummary
    && (singleAvailableCohortLabel || showSingleStatusSummaryBlock)
    ? summarizedVisibleSourceLabel
    : '';
  const resetViewLabel = getResetViewLabel({
    hasCustomLimit,
    hasSlugFilter: hasEffectiveSlugFilter,
    hasStatusFilter,
  });
  const showCohortSelect = !combinedSingleChoiceSummary && !singleAvailableCohortLabel;
  const showCohortFilterLoadingSummary = showCohortSelect
    && cohortsQuery.isLoading
    && hasVisibleRegistrations
    && !hasSlugFilter;
  const showEmptyCohortFilterSummary = showCohortSelect
    && !cohortsQuery.isLoading
    && !cohortsQuery.isError
    && hasVisibleRegistrations
    && cohortOptions.length === 0
    && !hasSlugFilter;
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
  const hasExplicitCsvExportScope = hasCustomFilters || localSearchNarrowsRegistrations;
  const canCopyCsv = searchedRegistrations.length > 1 && hasExplicitCsvExportScope;
  const copiedCsvRecently = copyMessage?.startsWith('Copiado CSV') ?? false;
  const showCopyCsvAction = canCopyCsv && !copiedCsvRecently;
  const showLocalSearchInlineClearAction = hasLocalSearch
    && !showEmptyLocalSearchResults;
  const showLocalSearchUtilityRow = hasLocalSearch && localSearchNarrowsRegistrations && (
    showCopyCsvAction
    || Boolean(copyMessage)
  );
  const showScopedCopyCsvAction = showCopyCsvAction && !showLocalSearchUtilityRow;
  const showScopedCopyMessage = Boolean(copyMessage) && !showLocalSearchUtilityRow;
  const hideTinyDefaultListRowDates = !hasCustomFilters && loadedRegistrationCount < MIN_DEFAULT_CSV_EXPORT_ROWS;
  const shouldShowSharedCohortSummary = !hasCustomFilters && Boolean(singleVisibleCohortLabel) && !singleAvailableCohortLabel;
  const hasSharedVisibleSource = Boolean(singleVisibleSourceLabel);
  const shouldShowSharedSourceSummary = hasNamedVisibleSource
    && !combinedSingleChoiceSourceSummary
    && !standaloneSingleChoiceSourceSummary;
  const showActiveStatusFilterSummary = hasStatusFilter && (hasEffectiveSlugFilter || hasCustomLimit);
  const statusAlreadyVisibleInFilterStrip = hasStatusFilter && !showSingleStatusSummary && !showActiveStatusFilterSummary;
  const showSingleCustomStatusSummary = singleVisibleCustomStatus != null && actionableStatusFilters.length === 0;
  const shouldShowSharedStatusSummary = Boolean(singleSearchedStatusLabel)
    && !showSingleStatusSummary
    && !statusAlreadyVisibleInFilterStrip
    && !showSingleCustomStatusSummary;
  const sharedVisibleStatusSummary = shouldShowSharedStatusSummary
    ? `Estado visible: ${singleSearchedStatusLabel}.`
    : '';
  const visibleNamedRegistrationsMissingContactCount =
    searchedRegistrations.filter(namedRegistrationNeedsContact).length;
  const sharedVisibleMissingContactSummary = formatVisibleMissingContactSummary(
    visibleNamedRegistrationsMissingContactCount,
    searchedRegistrations.length,
  );
  const sharedVisibleCreatedAtLabel = useMemo(() => {
    if (searchedRegistrations.length < 2) return '';
    const createdLabels = searchedRegistrations.map((reg) => formatOptionalDate(reg.crCreatedAt));
    if (createdLabels.some((label) => label === '')) return '';
    const [firstLabel] = createdLabels;
    return firstLabel && createdLabels.every((label) => label === firstLabel) ? firstLabel : '';
  }, [searchedRegistrations]);
  const sharedVisibleCreatedAtSummary = sharedVisibleCreatedAtLabel
    && hasCustomFilters
    && localSearchNarrowsRegistrations
    ? `Misma fecha de registro: ${sharedVisibleCreatedAtLabel}.`
    : '';
  const shouldHideSharedCreatedAtContext = Boolean(sharedVisibleCreatedAtLabel)
    && (hasCustomFilters || searchedRegistrations.length > 1);
  const allVisibleRegistrationsHaveNotes = searchedRegistrations.length > 1
    && searchedRegistrations.every((reg) => Boolean(reg.crAdminNotes?.trim()));
  const sharedVisibleNotesSummary = allVisibleRegistrationsHaveNotes
    ? 'Notas internas en todas las inscripciones visibles.'
    : '';
  const sharedListContextSummaries = [
    sharedVisibleStatusSummary,
    shouldShowSharedCohortSummary ? `Mostrando una sola cohorte: ${singleVisibleCohortLabel}.` : '',
    shouldShowSharedSourceSummary ? `Fuente visible: ${singleVisibleSourceLabel}.` : '',
    sharedVisibleCreatedAtSummary,
    sharedVisibleMissingContactSummary,
    sharedVisibleNotesSummary,
  ].filter(Boolean);
  const combinedSharedListContextSummary = sharedListContextSummaries.length > 1
    ? sharedListContextSummaries.join(' ')
    : '';
  const copyCsvButtonLabel = showLocalSearchUtilityRow
    ? 'Copiar visibles como CSV'
    : `Copiar CSV (${formatRowCountLabel(searchedRegistrations.length)})`;
  const copyCsvButtonAccessibleLabel = `Copiar ${formatRowCountLabel(searchedRegistrations.length)} visibles como CSV`;
  const visibleCsvScopeKey = useMemo(
    () => searchedRegistrations
      .map((reg) => `${reg.crCourseSlug}:${reg.crId}:${reg.crStatus}:${reg.crUpdatedAt}`)
      .join('|'),
    [searchedRegistrations],
  );
  const suppressDefaultMediumListUtilityRow = !hasCustomFilters
    && !hasLocalSearch
    && loadedRegistrationCount > 1
    && loadedRegistrationCount < MIN_LOCAL_SEARCH_REGISTRATIONS
    && !viewHitsCurrentLimit;
  const suppressDefaultUnscopedListUtilityRow = !hasCustomFilters
    && !hasLocalSearch;
  const showUtilityCountSummary = !hasLocalSearch
    && !canCopyCsv
    && !showTinyDefaultCountInCurrentView
    && !suppressDefaultMediumListUtilityRow
    && !suppressDefaultUnscopedListUtilityRow
    && (loadedRegistrationCount > 1 || Boolean(copyMessage));
  const standaloneReachedListLimitSummary = !hasCustomFilters
    && viewHitsCurrentLimit
    && !(showLocalSearchControl && !hasLocalSearch)
    ? buildReachedListLimitSummary(limit)
    : '';
  const showAdvancedLimitControl = viewHitsCurrentLimit || limit !== DEFAULT_LIMIT;
  const showSingleResultWithoutHiddenLimit = loadedRegistrationCount === 1 && !showAdvancedLimitControl;
  const showSingleResultWithOnlyPassiveFilterContext = showSingleResultWithoutHiddenLimit
    && !hasCustomFilters
    && !hasSlugFilter
    && !showCohortFilterUnavailableSummary
    && (Boolean(combinedSingleChoiceSummary) || showSingleStatusSummary);
  const showSingleStatusSummaryInPageChrome = showSingleStatusSummaryBlock && !showSingleResultWithOnlyPassiveFilterContext;
  const useCompactStatusActionLabel = showSingleStatusSummaryInPageChrome
    || statusAlreadyVisibleInFilterStrip
    || showActiveStatusFilterSummary
    || showSingleCustomStatusSummary
    || shouldShowSharedStatusSummary;
  const dossierScopeHint = [
    useCompactStatusActionLabel
      ? buildCompactDossierScopeHint(dossierIdentityTargetLabel)
      : buildDossierOnlyScopeHint(dossierIdentityTargetLabel),
    singleVisibleMissingContactSummary,
  ].filter(Boolean).join(' ');
  const showBusyListSearchOnboarding = showLocalSearchControl && !hasLocalSearch;
  const showDossierScopeHint = loadedRegistrationCount > 0
    && !hasUsedRowAction
    && !hasUsedFilterControl
    && !showBusyListSearchOnboarding;
  const showFirstRunFilterHelper = showFilterOnboardingCopy
    && !showSingleResultWithoutHiddenLimit
    && !showBusyListSearchOnboarding;
  const visibleActiveFilterSummary = useMemo(() => {
    const parts: string[] = [];
    const cohortAlreadyExplained = Boolean(combinedSingleChoiceSummary || singleAvailableCohortLabel);
    const statusAlreadyExplained = Boolean(
      combinedSingleChoiceSummary
      || showSingleStatusSummaryBlock
      || statusAlreadyVisibleInFilterStrip
      || showActiveStatusFilterSummary,
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
    showSingleStatusSummaryBlock,
    showActiveStatusFilterSummary,
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
    && hasVisibleRegistrations
    && !showEmptyLocalSearchResults,
  );
  const showInlineSingleChoiceLimitToggle = showAdvancedLimitControl
    && Boolean(combinedSingleChoiceSummary || singleAvailableCohortLabel || showSingleStatusSummaryBlock);
  const statusFilterCanSelfReset = statusAlreadyVisibleInFilterStrip && !hasEffectiveSlugFilter && !hasCustomLimit;
  const showFilteredResetAction = !showEmptyLocalSearchResults
    && !showInlineSummaryResetAction
    && !cohortFilterCanSelfReset
    && !statusFilterCanSelfReset;
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
    && !showPassiveSingleCohortLimitEmptyState
    && !showSelectedCohortFirstRunEmptyState
    && !hasVisibleRegistrations;
  const showInitialCohortErrorState = !regsQuery.isLoading
    && !regsQuery.isError
    && cohortsQuery.isError
    && !hasCustomFilters
    && !hasVisibleRegistrations;
  const showRegistrationErrorInlineRetry = regsQuery.isError && !hasVisibleRegistrations;
  const showInlineCohortRetryAction = showCohortFilterUnavailableSummary && !regsQuery.isError;
  const showHeaderRefreshAction = !showInitialCohortErrorState
    && !showRegistrationErrorInlineRetry
    && !showInlineCohortRetryAction
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
      || showScopedCopyCsvAction
      || showScopedCopyMessage
      || showFilteredResetAction
    );
  const showStandaloneListUtilityRow = !hasCustomFilters
    && hasVisibleRegistrations
    && (
      Boolean(standaloneUtilitySummaryMessage)
      || showScopedCopyCsvAction
      || showScopedCopyMessage
    );
  const showInitialFilterGuidance = !regsQuery.isLoading
    && !regsQuery.isError
    && !cohortsQuery.isError
    && !cohortsQuery.isLoading
    && (!hasCustomFilters || showPassiveSingleCohortLimitEmptyState || showSelectedCohortFirstRunEmptyState)
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
    && !showDefaultEmptyLocalSearchFocus
    && !showSingleResultWithOnlyPassiveFilterContext
    && (!regsQuery.isError || hasCustomFilters);
  const showRegistrationResultsPanel = !showInitialRegistrationLoading
    && !showInitialFilterGuidance
    && !showInitialCohortResolutionState
    && !showInitialCohortErrorState;
  const limitToggleLabel = showAdvancedFilters
    ? 'Ocultar límite'
    : limit !== DEFAULT_LIMIT
      ? `Ajustar límite (${limit})`
      : 'Ajustar límite';
  const singleAvailableCohortHelperText = actionableStatusFilters.length === 0
    ? 'Cohorte única por ahora.'
    : showAdvancedLimitControl
      ? 'Cohorte única por ahora. Usa Estado o Ajustar límite para cambiar la vista.'
      : 'Cohorte única por ahora. Usa Estado para cambiar la vista.';
  const singleVisibleStatusHelperText = showAdvancedLimitControl
    ? 'Estado único en esta vista. Usa cohorte o Ajustar límite para cambiar la vista.'
    : 'Estado único en esta vista. Usa cohorte para cambiar la vista.';
  const filtersHelpText = buildAutomaticFilterHelpText({
    combinedSingleChoiceSummary,
    hasVisibleRegistrations,
    showAdvancedLimitControl,
    showSingleStatusSummary: showSingleStatusSummaryBlock,
    singleAvailableCohortLabel,
  });
  const statusFilterHelperText = statusFilterCanSelfReset
    ? activeStatusFilterHelperText
    : hasHiddenStatusFilters
      ? 'Solo aparecen estados con inscripciones en esta vista.'
      : '';
  const showStatusFilterCaption = !showBusyListSearchOnboarding
    && !hasLocalSearch
    && !(statusFilterCanSelfReset && actionableStatusFilters.length === 1);
  const statusFilterGroupLabel = statusFilterCanSelfReset
    ? `Filtro de estado activo: ${statusFilterLabels[status]}`
    : 'Filtros de estado de inscripciones';
  const hideCustomStatusFilterSummaryForSearch = showLocalSearchControl
    && hasCustomStatusSearch
    && actionableStatusFilters.length === 0
    && !showSingleStatusSummary;
  const showCustomStatusFilterUnavailableSummary = hasVisibleRegistrations
    && !showSingleStatusSummary
    && actionableStatusFilters.length === 0
    && !hideCustomStatusFilterSummaryForSearch;
  const showStatusFilterColumn = !hideCustomStatusFilterSummaryForSearch;
  const filterGridColumns = showStatusFilterColumn ? 6 : 12;
  const customStatusFilterGuidance = customStatusFilterUnavailableMessage;
  const combinedSingleChoiceHelperText = showAdvancedLimitControl
    ? 'Vista única por ahora: una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.'
    : 'Vista única por ahora: una cohorte y un estado.';
  const canReviewSystemEmails = selectedDossier?.intent !== 'markPaid';
  const hasSystemEmailHistory = canReviewSystemEmails && (emailEventsQuery.data?.length ?? 0) > 0;
  const showSystemEmailHistoryAction = canReviewSystemEmails
    && (showEmailHistory || hasSystemEmailHistory || emailEventsQuery.isError);
  const showSystemEmailHistoryRetryAction = showSystemEmailHistoryAction
    && showEmailHistory
    && emailEventsQuery.isError;
  const hasMultipleAvailableCohorts = !cohortsQuery.isError && configuredCohortOptions.length > 1;
  const firstRunCohort = singleAvailableCohort ?? (showSelectedCohortFirstRunEmptyState ? selectedConfiguredCohort : null);
  const initialEmptyStateMessage = firstRunCohort
    ? buildSingleCohortInitialEmptyStateMessage(firstRunCohort.firstRunLabel)
    : hasMultipleAvailableCohorts
      ? buildInitialEmptyStateMultiCohortMessage(configuredCohortOptions.length)
    : initialEmptyStateConfigMessage;
  const initialEmptyStateAction = firstRunCohort
    ? {
      label: initialEmptyStateFormActionLabel,
      to: `/inscripcion/${encodeURIComponent(firstRunCohort.value)}`,
      ariaLabel: `Abrir formulario público de ${firstRunCohort.firstRunLabel}`,
    }
    : {
      label: hasMultipleAvailableCohorts
        ? initialEmptyStateMultiCohortActionLabel
        : initialEmptyStateConfigActionLabel,
      to: '/configuracion/cursos',
      ariaLabel: undefined,
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
    setCopyMessage(null);
  }, [localSearchKey, slug, status, limit, visibleCsvScopeKey]);

  useEffect(() => {
    setPageFlash(null);
  }, [localSearchKey, slug, status, limit]);

  useEffect(() => {
    if (!selectedDossier) {
      setDossierFlash(null);
      setMarkedPaidRegistrationId(null);
      setShowEmailHistory(false);
      setDossierContextMenuAnchor(null);
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
    setDossierContextMenuAnchor(null);
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

  const handleRefreshSystemEmails = () => {
    if (selectedDossierId == null) return;
    void emailEventsQuery.refetch();
  };

  const handleCopyCsv = async () => {
    if (searchedRegistrations.length < 2) return;
    const header = ['id', 'slug', 'nombre', 'email', 'telefono', 'estado', 'creado'];
    const rows = searchedRegistrations.map((reg) => [
      reg.crId,
      reg.crCourseSlug,
      reg.crFullName ?? '',
      reg.crEmail ?? '',
      reg.crPhoneE164 ?? '',
      registrationStatusLabel(reg.crStatus),
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

  const handleCloseDossierContextMenu = () => {
    setDossierContextMenuAnchor(null);
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
    setShowReceiptUrlField(false);
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
    setShowFollowUpUrlField(false);
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
      subject: followUpSubjectLabel(entry),
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
  const receiptIdsRequiringFileDisambiguator = getReceiptIdsRequiringFileDisambiguator(receipts);
  const followUps = dedupeCourseRegistrationFollowUps(dossierData?.crdFollowUps ?? []);
  const followUpIdsRequiringActionDisambiguator = getFollowUpIdsRequiringActionDisambiguator(followUps);
  const sharedReceiptCreatedLabel = getSharedOptionalDateLabel(receipts.map((receipt) => receipt.crrCreatedAt));
  const sharedFollowUpCreatedLabel = getSharedOptionalDateLabel(followUps.map((entry) => entry.crfCreatedAt));
  const sharedEmailEventCreatedLabel = getSharedOptionalDateLabel((emailEventsQuery.data ?? []).map((entry) => entry.ceCreatedAt));
  const persistedNotes = trimToNull(getPersistedNotesValue());
  const hasSavedNotes = Boolean(persistedNotes);
  const hasNotesDraftChanges = trimToNull(notesDraft) !== persistedNotes;
  const canMarkPaid = dossierData?.crdCanMarkPaid ?? false;
  const isMarkPaidIntent = selectedDossier?.intent === 'markPaid';
  const hasMarkedPaidInCurrentDossier =
    isMarkPaidIntent && selectedDossierId != null && markedPaidRegistrationId === selectedDossierId;
  const activeRegistrationStatus = hasMarkedPaidInCurrentDossier
    ? 'paid'
    : activeRegistration?.crStatus ?? '';
  const showMarkPaidAction = canMarkPaid && !hasMarkedPaidInCurrentDossier;
  const showInlineEmptyNotesAction = !isMarkPaidIntent && !showNotesComposer && !showFollowUpComposer && !hasSavedNotes;
  const showInlineEmptyFollowUpAction = !isMarkPaidIntent && !showFollowUpComposer && !showNotesComposer && followUps.length === 0;
  const hasPrimaryDossierAction = showMarkPaidAction || showSystemEmailHistoryAction;
  const hasAnyInlineDossierContextAction = showInlineEmptyNotesAction || showInlineEmptyFollowUpAction;
  const hasMultipleInlineDossierContextActions = showInlineEmptyNotesAction && showInlineEmptyFollowUpAction;
  const showGroupedDossierContextActions = hasAnyInlineDossierContextAction
    && hasMultipleInlineDossierContextActions;
  const showDirectInlineEmptyNotesAction = showInlineEmptyNotesAction && !showGroupedDossierContextActions;
  const showDirectInlineEmptyFollowUpAction = showInlineEmptyFollowUpAction && !showGroupedDossierContextActions;
  const showDossierActionRow = hasPrimaryDossierAction
    || showGroupedDossierContextActions
    || showDirectInlineEmptyNotesAction
    || showDirectInlineEmptyFollowUpAction;
  const groupedDossierContextActionsLabel = formatDossierContextActionsLabel({
    showInlineEmptyFollowUpAction,
    showInlineEmptyNotesAction,
  });
  const hasReceipts = receipts.length > 0;
  const activeRegistrationKnownStatus = activeRegistrationStatus
    ? normalizeKnownRegistrationStatus(activeRegistrationStatus)
    : null;
  const showEvidenceOnlyEmptyReceiptCopy = canMarkPaid
    || activeRegistrationKnownStatus === 'paid'
    || activeRegistrationKnownStatus === 'cancelled';
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
  const showReceiptExistingLinkAction = !showReceiptUrlField && !canSubmitReceipt;
  const showReceiptReviewPane = hasReceipts || !showReceiptComposer;
  const showReceiptMetadataFields = (
    receiptForm.editingId != null
    || showReceiptUrlField
    || Boolean(trimToNull(receiptForm.fileName))
    || canSubmitReceipt
  );
  const isMarkPaidFirstReceiptFlow = selectedDossier?.intent === 'markPaid'
    && !dossierQuery.isLoading
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
  const hasFollowUpAttachmentUrl = Boolean(trimToNull(followUpForm.attachmentUrl));
  const canHideFollowUpUrlField = showFollowUpUrlField && !hasFollowUpAttachmentUrl && !canHideFollowUpOptionalFields;
  const showFollowUpExistingLinkAction = !showFollowUpUrlField && !hasFollowUpAttachmentUrl;
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
  const followUpTypeOptions = getFollowUpTypeOptions(followUpForm.entryType);
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
  const hasOpenDossierComposer = showNotesComposer || showReceiptComposer || showFollowUpComposer;
  const hasDossierRefreshContext = hasReceipts
    || followUps.length > 0
    || hasSavedNotes
    || (showSystemEmailHistoryAction && showEmailHistory);
  const showDossierRefreshAction = Boolean(selectedDossier)
    && !dossierQuery.isLoading
    && !dossierQuery.isError
    && !isMarkPaidIntent
    && !hasOpenDossierComposer
    && !showSystemEmailHistoryRetryAction
    && hasDossierRefreshContext;
  const dossierRefreshLabel = showSystemEmailHistoryAction && showEmailHistory
    ? 'Refrescar expediente y correos'
    : 'Refrescar expediente';
  const dossierErrorRetryLabel = 'Reintentar expediente';

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
    ? hasMarkedPaidInCurrentDossier
      ? 'Pago registrado'
      : canMarkPaid
      ? 'Confirmar pago de inscripción'
      : 'Registrar pago de inscripción'
    : 'Expediente de inscripción';
  const copyCsvActionButton = showCopyCsvAction ? (
    <Button
      size="small"
      startIcon={<ContentCopyIcon fontSize="small" />}
      aria-label={copyCsvButtonAccessibleLabel}
      title="Copia solo las filas visibles de esta vista."
      onClick={() => void handleCopyCsv()}
    >
      {copyCsvButtonLabel}
    </Button>
  ) : null;

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
              {sharedReceiptCreatedLabel && (
                <Typography variant="body2" color="text.secondary">
                  Todos subidos: {sharedReceiptCreatedLabel}
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
                  {showReceiptExistingLinkAction && (
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
                  {showReceiptUrlField && (
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
                  )}
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
                  {receipts.map((receipt) => {
                    const receiptLabel = receiptDisplayLabelWithContext(
                      receipt,
                      receiptIdsRequiringFileDisambiguator.has(receipt.crrId),
                    );
                    const receiptCreatedLabel = sharedReceiptCreatedLabel
                      ? ''
                      : formatOptionalDate(receipt.crrCreatedAt);

                    return (
                      <Paper key={receipt.crrId} variant="outlined" sx={{ p: 1.5 }}>
                        <Stack spacing={1}>
                          {looksLikeImageResource(receipt.crrFileUrl, receipt.crrFileName) && (
                            <Box
                              component="img"
                              src={receipt.crrFileUrl}
                              alt={receiptLabel}
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
                                {receiptLabel}
                                <OpenInNewIcon sx={{ fontSize: 16 }} />
                              </Link>
                              {receiptCreatedLabel && (
                                <Typography variant="caption" color="text.secondary">
                                  Subido: {receiptCreatedLabel}
                                </Typography>
                              )}
                            </Box>
                            <IconButton
                              size="small"
                              title="Opciones del comprobante"
                              aria-label={`Abrir acciones para comprobante ${receiptLabel}`}
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
                    );
                  })}
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
              {initialCohortRetryLabel}
            </Button>
          )}
        >
          {initialCohortErrorMessage}
        </Alert>
      )}

      {showInitialRegistrationLoading && (
        <Alert
          severity="info"
          variant="outlined"
          icon={<CircularProgress size={18} />}
          data-testid="course-registration-initial-registration-loading"
        >
          {initialRegistrationLoadingMessage}
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
              aria-label={initialEmptyStateAction.ariaLabel}
              title={initialEmptyStateAction.ariaLabel}
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
                  <Grid item xs={12} md={filterGridColumns}>
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
                        {showInlineCohortRetryAction && (
                          <Button
                            size="small"
                            variant="text"
                            sx={{ alignSelf: 'flex-start', mt: 0.5 }}
                            onClick={handleRefresh}
                            disabled={cohortsQuery.isFetching}
                          >
                            {headerRefreshLabel}
                          </Button>
                        )}
                      </Stack>
                    ) : showCohortFilterLoadingSummary ? (
                      <Stack
                        data-testid="course-registration-cohort-filter-loading"
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
                          Cohortes cargando
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {cohortFilterLoadingMessage}
                        </Typography>
                      </Stack>
                    ) : showEmptyCohortFilterSummary ? (
                      <Stack
                        data-testid="course-registration-empty-cohort-filter"
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
                          Cohortes no configuradas
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {emptyCohortFilterMessage}
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
                  {showStatusFilterColumn && (
                    <Grid item xs={12} md={6}>
                      {showSingleStatusSummaryBlock && singleVisibleStatus ? (
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
                      ) : showActiveStatusFilterSummary ? (
                        <Stack
                          data-testid="course-registration-active-status-summary"
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
                            Estado filtrado
                          </Typography>
                          <Typography variant="body2" fontWeight={600}>
                            {statusFilterLabels[status]}
                          </Typography>
                          <Typography variant="caption" color="text.secondary">
                            La vista filtrada ya incluye este estado; usa {resetViewLabel.toLowerCase()} si necesitas volver a ampliar la lista.
                          </Typography>
                        </Stack>
                      ) : showSingleCustomStatusSummary && singleVisibleCustomStatus != null ? (
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
                            {customStatusFilterGuidance}
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
                            {customStatusFilterGuidance}
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
                                aria-label={statusFilterChipAriaLabel(value, status === value)}
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
                  )}
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
                  label={LOAD_LIMIT_LABEL}
                  type="number"
                  inputProps={{ min: 1 }}
                  value={limit}
                  onChange={(e) => {
                    setHasUsedFilterControl(true);
                    setLocalSearch('');
                    setLimit(parsePositiveLimit(e.target.value, DEFAULT_LIMIT));
                  }}
                  helperText={LOAD_LIMIT_HELPER_TEXT}
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
                {showScopedCopyCsvAction && copyCsvActionButton}
                {showScopedCopyMessage && copyMessage && (
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
                {shouldShowSharedStatusSummary && (
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1.5 }}>
                    {sharedVisibleStatusSummary}
                  </Typography>
                )}
                {shouldShowSharedCohortSummary && (
                  <Typography
                    variant="body2"
                    color="text.secondary"
                    sx={{ mt: shouldShowSharedStatusSummary ? 0.75 : 1.5 }}
                  >
                    Mostrando una sola cohorte: {singleVisibleCohortLabel}.
                  </Typography>
                )}
                {shouldShowSharedSourceSummary && (
                  <Typography
                    variant="body2"
                    color="text.secondary"
                    sx={{ mt: shouldShowSharedStatusSummary || shouldShowSharedCohortSummary ? 0.75 : 1.5 }}
                  >
                    {sharedVisibleSourceSummary}
                  </Typography>
                )}
                {sharedVisibleCreatedAtSummary && (
                  <Typography
                    variant="body2"
                    color="text.secondary"
                    data-testid="course-registration-shared-created-at-summary"
                    sx={{
                      mt: shouldShowSharedStatusSummary || shouldShowSharedCohortSummary || shouldShowSharedSourceSummary
                        ? 0.75
                        : 1.5,
                    }}
                  >
                    {sharedVisibleCreatedAtSummary}
                  </Typography>
                )}
                {sharedVisibleMissingContactSummary && (
                  <Typography
                    variant="body2"
                    color="text.secondary"
                    sx={{
                      mt: shouldShowSharedStatusSummary || shouldShowSharedCohortSummary || shouldShowSharedSourceSummary
                        || Boolean(sharedVisibleCreatedAtSummary)
                        ? 0.75
                        : 1.5,
                    }}
                  >
                    {sharedVisibleMissingContactSummary}
                  </Typography>
                )}
                {sharedVisibleNotesSummary && (
                  <Typography
                    variant="body2"
                    color="text.secondary"
                    sx={{
                      mt: shouldShowSharedStatusSummary
                        || shouldShowSharedCohortSummary
                        || shouldShowSharedSourceSummary
                        || Boolean(sharedVisibleCreatedAtSummary)
                        || Boolean(sharedVisibleMissingContactSummary)
                        ? 0.75
                        : 1.5,
                    }}
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
                {showScopedCopyCsvAction && copyCsvActionButton}
                {showScopedCopyMessage && copyMessage && (
                  <Typography variant="caption" color="text.secondary">
                    {copyMessage}
                  </Typography>
                )}
              </Stack>
            )}
          </>
        </Paper>
      )}

      {showRegistrationResultsPanel && (
        <Paper sx={{ p: 3, borderRadius: 3 }} data-testid="course-registration-results-panel">
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
          {regsQuery.isLoading && <Typography>{initialRegistrationLoadingMessage}</Typography>}
          {showLocalSearchControl && (
            <Box sx={{ mb: 2 }}>
              <Stack spacing={1} alignItems="flex-start">
                <TextField
                  label={LOCAL_SEARCH_LABEL}
                  value={localSearch}
                  onChange={(e) => {
                    setHasUsedFilterControl(true);
                    setLocalSearch(normalizeVisibleLocalSearchInput(e.target.value));
                  }}
                  placeholder={localSearchPlaceholder}
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
                    endAdornment: showLocalSearchInlineClearAction ? (
                      <InputAdornment position="end">
                        <Tooltip title="Limpiar búsqueda">
                          <IconButton
                            edge="end"
                            size="small"
                            aria-label="Limpiar búsqueda"
                            onClick={() => setLocalSearch('')}
                          >
                            <ClearIcon fontSize="small" />
                          </IconButton>
                        </Tooltip>
                      </InputAdornment>
                    ) : undefined,
                  }}
                />
                {showLocalSearchUtilityRow && (
                  <Stack
                    direction="row"
                    spacing={1}
                    alignItems="center"
                    flexWrap="wrap"
                    useFlexGap
                    data-testid="course-registration-local-search-utilities"
                  >
                    {copyCsvActionButton}
                    {copyMessage && (
                      <Typography variant="caption" color="text.secondary">
                        {copyMessage}
                      </Typography>
                    )}
                  </Stack>
                )}
              </Stack>
            </Box>
          )}
          {!regsQuery.isLoading && !regsQuery.isError && registrations.length === 0 && (
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
              {emptyLocalSearchResultsMessage}
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
                  const rowSecondaryIdentity =
                    rowIdentity.secondary === missingContactSummary ? '' : rowIdentity.secondary;
                  const rowUsesGeneratedIdentity = !reg.crFullName?.trim()
                    && !reg.crEmail?.trim()
                    && !reg.crPhoneE164?.trim();
                  const showRowRegistrationDisambiguator =
                    registrationIdsRequiringActionRecordDisambiguator.has(reg.crId)
                    || (
                      registrationIdsRequiringActionDisambiguator.has(reg.crId)
                      && !rowSecondaryIdentity
                    );
                  const rowActionTarget = getActionTargetLabelForRegistration(reg);
                  const rowCohortSlug = reg.crCourseSlug.trim();
                  const rowCohortLabel = cohortLabelsBySlug.get(rowCohortSlug) ?? rowCohortSlug;
                  const hasRowNotes = Boolean(reg.crAdminNotes?.trim());
                  const rowMatchesVisibleSearchFields = hasLocalSearch
                    ? registrationMatchesVisibleSearchFields({
                      cohortLabelsBySlug,
                      localSearchDigitsKey,
                      localSearchKey,
                      reg,
                    })
                    : false;
                  const rowMatchedOnlyHiddenNote = hasLocalSearch
                    && !rowMatchesVisibleSearchFields
                    && localSearchTextMatches(reg.crAdminNotes, localSearchKey);
                  const showRowCohort = selectedSlug
                    ? rowCohortSlug !== selectedSlug
                    : !(singleVisibleCohortLabel || singleAvailableCohortLabel);
                  const showRowSource = !hasSharedVisibleSource || !showRegistrationFilterPanel;
                  const hasDateOnlyRowContext = !showRowCohort && !showRowSource && !hasRowNotes;
                  const hideDateOnlyRowContext = hasDateOnlyRowContext
                    && (
                      loadedRegistrationCount === 1
                      || !hasCustomFilters
                    );
                  const rowContextSummary = registrationListContextSummary({
                    cohortLabel: rowCohortLabel,
                    createdAt: reg.crCreatedAt,
                    hasNotes: hasRowNotes && !allVisibleRegistrationsHaveNotes && !rowMatchedOnlyHiddenNote,
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
                            title={`Abrir expediente de ${rowActionTarget}`}
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
                        {rowSecondaryIdentity && !rowUsesGeneratedIdentity && (
                          <Typography variant="body2" color="text.secondary">
                            {rowSecondaryIdentity}
                          </Typography>
                        )}
                        {showRowRegistrationDisambiguator && (
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
        open={Boolean(dossierContextMenuAnchor)}
        anchorEl={dossierContextMenuAnchor}
        onClose={handleCloseDossierContextMenu}
      >
        {showInlineEmptyNotesAction && (
          <MenuItem
            onClick={() => {
              handleCloseDossierContextMenu();
              handleOpenNotesComposer();
            }}
          >
            Agregar nota
          </MenuItem>
        )}
        {showInlineEmptyFollowUpAction && (
          <MenuItem
            onClick={() => {
              handleCloseDossierContextMenu();
              setShowFollowUpComposer(true);
            }}
          >
            Agregar seguimiento
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
            <Alert
              severity="error"
              action={(
                <Button
                  color="inherit"
                  size="small"
                  onClick={handleRefreshDossier}
                  disabled={!selectedDossier || isRefreshingDossier}
                >
                  {dossierErrorRetryLabel}
                </Button>
              )}
            >
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
                    {activeRegistrationStatus && statusChip(activeRegistrationStatus)}
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
                      {showGroupedDossierContextActions && (
                        <Button
                          variant="outlined"
                          endIcon={<ArrowDropDownIcon />}
                          aria-haspopup="menu"
                          aria-expanded={Boolean(dossierContextMenuAnchor)}
                          onClick={(event) => setDossierContextMenuAnchor(event.currentTarget)}
                        >
                          {groupedDossierContextActionsLabel}
                        </Button>
                      )}
                      {showDirectInlineEmptyNotesAction && (
                        <Button
                          variant="outlined"
                          onClick={handleOpenNotesComposer}
                        >
                          Agregar nota
                        </Button>
                      )}
                      {showDirectInlineEmptyFollowUpAction && (
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
                      {sharedEmailEventCreatedLabel && (
                        <Typography variant="body2" color="text.secondary">
                          Correos registrados: {sharedEmailEventCreatedLabel}
                        </Typography>
                      )}

                      {emailEventsQuery.isLoading && (
                        <Stack direction="row" spacing={1} alignItems="center">
                          <CircularProgress size={18} />
                          <Typography variant="body2">Cargando historial…</Typography>
                        </Stack>
                      )}

                      {emailEventsQuery.isError && (
                        <Alert
                          severity="error"
                          action={(
                            <Button
                              color="inherit"
                              size="small"
                              onClick={handleRefreshSystemEmails}
                              disabled={emailEventsQuery.isFetching}
                            >
                              {retrySystemEmailsLabel}
                            </Button>
                          )}
                        >
                          No se pudo cargar el historial: {emailEventsQuery.error instanceof Error ? emailEventsQuery.error.message : 'Error'}
                        </Alert>
                      )}

                      {!emailEventsQuery.isLoading && !emailEventsQuery.isError && (emailEventsQuery.data?.length ?? 0) === 0 && (
                        <Alert severity="info">{emptySystemEmailHistoryMessage}</Alert>
                      )}

                      {!emailEventsQuery.isLoading && !emailEventsQuery.isError && (emailEventsQuery.data?.length ?? 0) > 0 && (
                        <Stack spacing={1}>
                          {(emailEventsQuery.data ?? []).map((entry) => {
                            const emailEventCreatedLabel = sharedEmailEventCreatedLabel ? '' : formatDate(entry.ceCreatedAt);

                            return (
                              <Paper key={entry.ceId} variant="outlined" sx={{ p: 1.5 }}>
                                <Stack direction="row" spacing={1} alignItems="center" sx={{ mb: 0.75 }} flexWrap="wrap" useFlexGap>
                                  <Chip size="small" label={eventStatusLabel(entry.ceStatus)} color={eventStatusColor(entry.ceStatus)} />
                                  <Chip size="small" label={eventTypeLabel(entry.ceEventType)} variant="outlined" />
                                  {emailEventCreatedLabel && (
                                    <Typography variant="caption" color="text.secondary">
                                      {emailEventCreatedLabel}
                                    </Typography>
                                  )}
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
                            );
                          })}
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
                    {sharedFollowUpCreatedLabel && (
                      <Typography variant="body2" color="text.secondary">
                        Todos registrados: {sharedFollowUpCreatedLabel}
                      </Typography>
                    )}

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
                                  {showFollowUpExistingLinkAction && (
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
                            {followUps.map((entry) => {
                              const followUpActionLabel = followUpActionTargetLabelWithContext(
                                entry,
                                followUpIdsRequiringActionDisambiguator.has(entry.crfId),
                              );
                              const followUpSubject = followUpSubjectLabel(entry);
                              const followUpCreatedLabel = sharedFollowUpCreatedLabel
                                ? ''
                                : formatOptionalDate(entry.crfCreatedAt);
                              const followUpAttachmentLabel =
                                entry.crfAttachmentName?.trim() || `Adjunto de ${followUpActionLabel}`;

                              return (
                                <Paper key={entry.crfId} variant="outlined" sx={{ p: 1.5 }}>
                                  <Stack spacing={1}>
                                    <Stack direction="row" justifyContent="space-between" alignItems="flex-start" flexWrap="wrap" useFlexGap>
                                      <Stack direction="row" spacing={0.75} alignItems="center" flexWrap="wrap" useFlexGap>
                                        <Chip size="small" label={eventTypeLabel(entry.crfEntryType)} variant="outlined" />
                                        {followUpCreatedLabel && (
                                          <Typography variant="caption" color="text.secondary">
                                            {followUpCreatedLabel}
                                          </Typography>
                                        )}
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
                                        aria-label={`Abrir acciones para seguimiento ${followUpActionLabel}`}
                                        aria-haspopup="menu"
                                        onClick={(event) => handleOpenFollowUpMenu(event.currentTarget, entry)}
                                      >
                                        <MoreVertIcon fontSize="small" />
                                      </IconButton>
                                    </Stack>
                                    {followUpSubject && (
                                      <Typography variant="subtitle2">{followUpSubject}</Typography>
                                    )}
                                    <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap' }}>
                                      {entry.crfNotes}
                                    </Typography>
                                    {entry.crfAttachmentUrl && (
                                      <Link href={entry.crfAttachmentUrl} target="_blank" rel="noreferrer" underline="hover">
                                        <Stack direction="row" spacing={0.75} alignItems="center">
                                          <OpenInNewIcon sx={{ fontSize: 16 }} />
                                          <span>{followUpAttachmentLabel}</span>
                                        </Stack>
                                      </Link>
                                    )}
                                  </Stack>
                                </Paper>
                              );
                            })}
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
