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
import ReceiptLongIcon from '@mui/icons-material/ReceiptLong';
import SearchIcon from '@mui/icons-material/Search';
import ClearIcon from '@mui/icons-material/Clear';
import UndoIcon from '@mui/icons-material/Undo';
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
const emptyReceiptEvidenceAlertMessage = 'Agrega evidencia solo si necesitas documentar este pago. Se guardará aquí con un enlace para revisarla después.';
const firstReceiptComposerHelpText = 'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y aparecerá aquí con enlace y acciones para revisarlo después.';
const receiptComposerHelpText = 'Este formulario ya está abierto para guardar otro comprobante o pegar un enlace existente.';
const editingReceiptComposerHelpText = 'Edita el comprobante y guarda los cambios para actualizar el registro.';
const receiptUrlFallbackHelpText = 'Pega un enlace existente; si prefieres subir un archivo, oculta este campo.';
const initialEmptyStateConfigMessage = 'Todavía no hay inscripciones. El formulario público se configura en el primer curso.';
const INITIAL_COHORT_PREVIEW_LIMIT = 2;
const INITIAL_COHORT_ACTION_TITLE_PREVIEW_LIMIT = 3;
const cleanInitialCohortPreviewLabel = (label: string) =>
  label.trim().replace(/\s*[.!:;]+$/g, '').trim();
const normalizeInitialCohortPreviewKey = (label: string) =>
  cleanInitialCohortPreviewLabel(label)
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');

const uniqueInitialCohortLabels = (labels: readonly string[]) => {
  const uniqueLabelsByKey = new Map<string, string>();

  labels.forEach((label) => {
    const trimmedLabel = cleanInitialCohortPreviewLabel(label);
    if (!trimmedLabel) return;

    const labelKey = normalizeInitialCohortPreviewKey(trimmedLabel);
    if (!uniqueLabelsByKey.has(labelKey)) {
      uniqueLabelsByKey.set(labelKey, trimmedLabel);
    }
  });

  return Array.from(uniqueLabelsByKey.values());
};

const formatInitialCohortLabelList = (labels: readonly string[]) => {
  if (labels.length === 0) return '';

  if (labels.length === 1) return labels[0] ?? '';
  if (labels.length === 2) return `${labels[0]} y ${labels[1]}`;
  return `${labels.slice(0, -1).join(', ')} y ${labels[labels.length - 1]}`;
};

const formatInitialCohortPreview = (labels: readonly string[]) => {
  const uniqueLabels = uniqueInitialCohortLabels(labels);
  const visibleLabels = uniqueLabels.slice(0, INITIAL_COHORT_PREVIEW_LIMIT);
  const hiddenUniqueLabelCount = Math.max(0, uniqueLabels.length - visibleLabels.length);
  const hiddenCount = uniqueLabels.length > 1 ? hiddenUniqueLabelCount : 0;

  if (hiddenCount > 0) {
    const hiddenLabel = `${hiddenCount} ${hiddenCount === 1 ? 'curso más' : 'cursos más'}`;
    return formatInitialCohortLabelList([...visibleLabels, hiddenLabel]);
  }

  return formatInitialCohortLabelList(visibleLabels);
};

const formatInitialCohortActionTitleList = (labels: readonly string[]) => {
  if (labels.length <= INITIAL_COHORT_ACTION_TITLE_PREVIEW_LIMIT + 1) {
    return formatInitialCohortLabelList(labels);
  }

  const visibleLabels = labels.slice(0, INITIAL_COHORT_ACTION_TITLE_PREVIEW_LIMIT);
  const hiddenCount = labels.length - visibleLabels.length;
  return formatInitialCohortLabelList([
    ...visibleLabels,
    `${hiddenCount} ${hiddenCount === 1 ? 'curso más' : 'cursos más'}`,
  ]);
};

const countInitialCohortPreviewLabels = (labels: readonly string[]) => {
  return uniqueInitialCohortLabels(labels).length;
};

const buildInitialEmptyStateMultiCohortActionTitle = (count: number, labels: readonly string[] = []) => {
  const uniqueLabels = uniqueInitialCohortLabels(labels);
  if (uniqueLabels.length === 0) return initialEmptyStateMultiCohortActionAriaLabel;

  const formsLabel = `${count} formulario${count === 1 ? '' : 's'} público${count === 1 ? '' : 's'}`;
  if (count > 1 && uniqueLabels.length === 1) {
    return `Elegir entre ${formsLabel} para ${uniqueLabels[0]}.`;
  }

  return `Elegir entre ${formsLabel}: ${formatInitialCohortActionTitleList(uniqueLabels)}.`;
};

const buildInitialEmptyStateSingleCourseVariantActionTitle = (count: number, labels: readonly string[] = []) => {
  const [label] = uniqueInitialCohortLabels(labels);
  if (!label) return initialEmptyStateSingleCourseVariantActionAriaLabel;

  const formsLabel = `${count} formulario${count === 1 ? '' : 's'} público${count === 1 ? '' : 's'}`;
  return `Elegir entre ${formsLabel} de ${label}.`;
};

const buildInitialEmptyStateMultiCohortMessage = (count: number, labels: readonly string[] = []) => {
  const preview = formatInitialCohortPreview(labels);
  if (preview) {
    if (count > 1 && countInitialCohortPreviewLabels(labels) === 1) {
      return `Hay ${count} formularios públicos de ${preview} listos para recibir la primera inscripción.`;
    }
    return `Hay ${count} formularios públicos listos para recibir la primera inscripción: ${preview}.`;
  }
  return `Hay ${count} formularios públicos listos para recibir la primera inscripción.`;
};
const initialEmptyStateConfigActionLabel = 'Crear curso';
const initialEmptyStateChooseFormActionLabel = 'Elegir formulario';
const initialEmptyStateFormActionLabel = 'Abrir formulario público';
const initialEmptyStateNewTabDescription = 'Se abre en una pestaña nueva.';
const initialEmptyStateNewTabDescriptionId = 'course-registration-initial-empty-state-new-tab-description';
const initialRegistrationLoadingMessage = 'Cargando inscripciones…';
const initialCohortResolutionMessage = 'Revisando formularios de curso para mostrar el siguiente paso.';
const initialCohortErrorMessage = 'No se pudieron cargar los formularios de curso. Reintenta para elegir qué formulario compartir.';
const initialCohortRetryLabel = 'Reintentar formularios';
const unavailableCohortFilterLabel = 'Formularios públicos no disponibles';
const unavailableCohortFilterRetryLabel = 'Reintentar formularios públicos';
const unavailableCohortFilterRetryTitle = 'Reintenta solo los formularios públicos; la lista visible no se recarga.';
const initialEmptyStateConfigActionAriaLabel = 'Crear el primer curso';
const initialEmptyStateMultiCohortActionAriaLabel = 'Ver formularios públicos para elegir cuál compartir primero';
const initialEmptyStateSingleCourseVariantActionAriaLabel = 'Ver formularios públicos de este curso para elegir cuál compartir primero';
const cohortFilterUnavailableMessage = 'No se pudieron cargar los formularios públicos. La lista sigue disponible; el selector por formulario volverá cuando se recupere esa información.';
const busyCohortFilterUnavailableMessage = 'La lista sigue cargada; los formularios públicos no están disponibles.';
const cohortFilterLoadingMessage = 'La lista ya está disponible; el selector por formulario aparecerá cuando terminen de cargar los formularios.';
const emptyCohortFilterMessage = 'Sin selector por formulario hasta configurar cursos. La lista sigue disponible.';
const genericSingleCohortInitialEmptyStateMessage =
  'Todavía no hay inscripciones. La página pública ya está lista para recibir la primera.';
const buildSingleCohortInitialEmptyStateMessage = (cohortLabel: string) =>
  cohortLabel
    ? `Todavía no hay inscripciones para ${cohortLabel}. La página pública ya está lista para recibir la primera.`
    : genericSingleCohortInitialEmptyStateMessage;
type RegistrationIdentityKind = 'name' | 'email' | 'phone' | 'record';
const buildCompactDossierScopeHint = (targetLabel: string) =>
  `Abre expediente desde ${targetLabel}; las opciones de estado muestran acciones.`;
const buildDossierLinkScopeHint = (targetLabel: string) =>
  `Abre expediente desde ${targetLabel}.`;
const buildPaymentWorkflowScopeHint = (targetLabel: string) =>
  `Abre expediente desde ${targetLabel}; las opciones de pago y estado incluyen Registrar pago.`;
const buildDossierOnlyScopeHint = (targetLabel: string) =>
  `Abre expediente desde ${targetLabel}; las opciones de estado abren acciones rápidas.`;
const buildCustomStatusNormalizationScopeHint = (targetLabel: string) =>
  `Abre expediente desde ${targetLabel}; las opciones de estado normalizan a pendiente o cancelado.`;
const buildPendingRecoveryScopeHint = (targetLabel: string) =>
  `Abre expediente desde ${targetLabel}; Reabrir vuelve a pendiente.`;
const buildPaidRecoveryScopeHint = (targetLabel: string) =>
  `Abre expediente desde ${targetLabel}; Marcar pago pendiente devuelve la inscripción a pendiente.`;
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
const firstFollowUpComposerHelpText = 'Este formulario ya está abierto para registrar el primer seguimiento. Escribe la nota y aparecerá Guardar seguimiento.';
const followUpComposerHelpText = 'Este formulario ya está abierto para registrar seguimiento. Escribe la nota y aparecerá Guardar seguimiento.';
const editingFollowUpComposerHelpText = 'Edita el seguimiento y guarda los cambios para actualizar el historial.';
const markPaidOptionalFollowUpActionLabel = 'Agregar seguimiento';
const markPaidOptionalFollowUpAccessibleLabel = 'Agregar seguimiento opcional';
const openPaymentWorkflowLabel = 'Registrar pago';
const paymentStatusMenuButtonLabel = 'Pago y estado';
const markPaidSuccessMessage = 'Inscripción marcada como pagada.';
const activeStatusFilterHelperText = 'Selecciona el estado activo otra vez para volver a ver todos.';
const customStatusFilterUnavailableMessage = 'Normaliza cada fila desde Estado para recuperar los filtros estándar.';
const defaultPublicFormSource = 'landing';
const MIN_LOCAL_SEARCH_REGISTRATIONS = 8;
const MIN_DEFAULT_CSV_EXPORT_ROWS = MIN_LOCAL_SEARCH_REGISTRATIONS;
const MIN_REPEATED_DIRECT_RECOVERY_ICON_ACTIONS = 2;
const MIN_REPEATED_CUSTOM_STATUS_ICON_ACTIONS = 2;
const MIN_PHONE_SEARCH_DIGITS = 4;
const MIN_FULL_PHONE_MATCH_DIGITS = 7;
const MAX_LOCAL_SEARCH_PLACEHOLDER_TERMS = 4;
const MAX_LOCAL_SEARCH_QUERY_SUMMARY_LENGTH = 64;
const COHORT_FILTER_LABEL = 'Formulario público';
const LOCAL_SEARCH_LABEL = 'Buscar inscripciones';
const LOAD_LIMIT_LABEL = 'Límite de carga';
const LOAD_LIMIT_HELPER_TEXT = 'Máximo de inscripciones cargadas en esta vista.';
const LOCAL_SEARCH_COMPACT_CONTEXT_TITLE = 'Otros datos: estado, curso, fuente, origen o nota cuando existan.';
const missingContactSummary = 'Sin correo ni teléfono';
const registrationStatusNeedsReviewLabel = 'Estado por revisar';
const CONTACT_PLACEHOLDER_VALUE_KEYS = new Set([
  '-',
  'n a',
  'na',
  'ninguna',
  'ninguno',
  'celular pendiente',
  'correo pendiente',
  'email pendiente',
  'numero pendiente',
  'no tiene celular',
  'no tiene correo',
  'no tiene email',
  'no tiene numero',
  'no tiene telefono',
  'no tiene numero de whatsapp',
  'no tiene whatsapp',
  'no aplica',
  'no disponible',
  'no registra',
  'no registra correo',
  'no registra email',
  'no registra numero',
  'no registra numero de whatsapp',
  'no registra telefono',
  'no registra whatsapp',
  'no informada',
  'no informado',
  'no registrado',
  'no registrada',
  'no reportado',
  'no proporcionada',
  'no proporcionado',
  'none',
  'not available',
  'not provided',
  'pendiente',
  'pendiente de celular',
  'pendiente de correo',
  'pendiente de email',
  'pendiente de numero',
  'pendiente de telefono',
  'pendiente de whatsapp',
  'pendiente por validar',
  'telefono pendiente',
  'whatsapp pendiente',
  'por actualizar',
  'por completar',
  'por confirmar',
  'por definir',
  'por validar',
  's n',
  'desconocido',
  'desconocida',
  'sin celular',
  'sin completar',
  'sin correo',
  'sin datos',
  'sin email',
  'sin informacion',
  'sin numero',
  'sin numero de whatsapp',
  'sin telefono',
  'sin telefono ni whatsapp',
  'sin telefono ni correo',
  'sin whatsapp',
  'sin actualizar',
  'tbd',
]);

const normalizeContactPlaceholderKey = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[.!?:;]+$/g, '')
    .replace(/[^a-z0-9-]+/gi, ' ')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');

const isPlaceholderContactValue = (value: string) => {
  const placeholderKey = normalizeContactPlaceholderKey(value);
  return placeholderKey === '' || CONTACT_PLACEHOLDER_VALUE_KEYS.has(placeholderKey);
};

const normalizeRegistrationContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  if (isPlaceholderContactValue(trimmed)) return null;
  return trimmed;
};

const NAME_PLACEHOLDER_VALUE_KEYS = new Set([
  ...CONTACT_PLACEHOLDER_VALUE_KEYS,
  'nombre pendiente',
  'nombre por confirmar',
  'nombre por definir',
  'no registra nombre',
  'no tiene nombre',
  'sin nombre',
  'sin nombre completo',
]);

const normalizeRegistrationNameValue = (value?: string | null) => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  if (NAME_PLACEHOLDER_VALUE_KEYS.has(normalizeContactPlaceholderKey(trimmed))) return null;
  return trimmed;
};

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
  if (
    normalized === 'pending'
    || normalized === 'awaiting_payment'
    || normalized === 'not_paid'
    || normalized === 'payment_pending'
    || normalized === 'payment_due'
    || normalized === 'pago_pendiente'
    || normalized === 'no_pagado'
    || normalized === 'pendiente_de_pago'
    || normalized === 'pendiente_pago'
    || normalized === 'por_pagar'
    || normalized === 'sin_pago'
    || normalized === 'unpaid'
  ) {
    return 'pending_payment';
  }
  if (
    normalized === 'payment_paid'
    || normalized === 'payment_received'
    || normalized === 'payment_confirmed'
    || normalized === 'payment_succeeded'
    || normalized === 'pago_recibido'
    || normalized === 'pago_confirmado'
    || normalized === 'pagado'
  ) {
    return 'paid';
  }
  if (
    normalized === 'canceled'
    || normalized === 'cancelado'
    || normalized === 'cancelada'
  ) {
    return 'cancelled';
  }
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
  hasCohortFilterControl,
  hasStatusFilterControl,
  hasVisibleRegistrations,
  showAdvancedLimitControl,
  showSingleStatusSummary,
  singleAvailableCohortLabel,
}: {
  combinedSingleChoiceSummary: string;
  hasCohortFilterControl: boolean;
  hasStatusFilterControl: boolean;
  hasVisibleRegistrations: boolean;
  showAdvancedLimitControl: boolean;
  showSingleStatusSummary: boolean;
  singleAvailableCohortLabel: string;
}) => {
  if (combinedSingleChoiceSummary) return '';

  if (singleAvailableCohortLabel || showSingleStatusSummary) {
    return '';
  }

  if (!hasStatusFilterControl) return '';

  const filterStartingPoint = hasCohortFilterControl
    ? 'Cambia cohorte o estado para actualizar la lista.'
    : 'Cambia Estado para actualizar la lista.';
  const limitGuidance = showAdvancedLimitControl
    ? 'Usa Ajustar límite solo si necesitas otro lote.'
    : 'Ajustar límite aparecerá cuando se llene el lote.';
  const emptySuffix = hasVisibleRegistrations
    ? ''
    : ' Si esperabas resultados, ajusta la vista o refresca.';

  return `${filterStartingPoint} ${limitGuidance}${emptySuffix}`;
};

const getResetViewLabel = ({
  hasCustomLimit,
  hasSlugFilter,
  hasStatusFilter,
  hasUnconfiguredSlugFilter = false,
}: {
  hasCustomLimit: boolean;
  hasSlugFilter: boolean;
  hasStatusFilter: boolean;
  hasUnconfiguredSlugFilter?: boolean;
}) => {
  if (hasCustomLimit && (hasSlugFilter || hasStatusFilter)) return 'Restablecer vista';
  if (hasCustomLimit) return 'Restablecer límite';
  if (hasSlugFilter && !hasStatusFilter && hasUnconfiguredSlugFilter) return 'Quitar filtro de formulario';
  if (hasSlugFilter && !hasStatusFilter) return 'Mostrar todos los formularios';
  if (!hasSlugFilter && hasStatusFilter) return 'Mostrar todos los estados';
  if (hasSlugFilter && hasStatusFilter) return 'Restablecer vista';
  return 'Restablecer vista';
};

const formatCsvRegistrationCountLabel = (count: number) => `${count} inscripci${count === 1 ? 'ón' : 'ones'}`;
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
    ? 'Sin cambios: la búsqueda coincide con la inscripción cargada.'
    : `Sin cambios: la búsqueda coincide con las ${formatRegistrationCountLabel(loadedCount)} cargadas.`;
const cappedLocalSearchEmptyHint =
  'Aumenta el límite si el registro puede estar fuera del lote cargado.';
const emptyLocalSearchLimitRecoveryLabel = 'Revisar más registros';
const emptyLocalSearchLimitRecoveryAccessibleLabel = 'Revisar más registros aumentando el límite de carga';
const emptyLocalSearchLimitRecoveryTitle = 'Muestra el campo de límite de carga para buscar fuera del lote actual.';

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
const normalizeCompactLocalSearchText = (value: string) =>
  normalizeLocalSearchText(value).replace(/[^a-z0-9]+/g, '');
const normalizeVisibleLocalSearchInput = (value: string) => (
  value.trim().length === 0 ? '' : value
);
const normalizeLocalSearchDigits = (value: string) => value.replace(/\D/g, '');
const phoneSearchDigitCandidates = (digits: string) => {
  const candidates = [digits];
  if (digits.startsWith('0') && digits.length > MIN_PHONE_SEARCH_DIGITS) {
    candidates.push(digits.slice(1));
  }

  return candidates.filter((candidate, index) => (
    candidate.length >= MIN_PHONE_SEARCH_DIGITS && candidates.indexOf(candidate) === index
  ));
};
const phoneDigitsMatchLocalSearch = (phoneValue: string | null | undefined, localSearchDigitsKey: string) => {
  const phoneCandidates = phoneSearchDigitCandidates(
    normalizeLocalSearchDigits(normalizeRegistrationContactValue(phoneValue) ?? ''),
  );
  const searchCandidates = phoneSearchDigitCandidates(localSearchDigitsKey);

  return phoneCandidates.some((phoneDigits) => (
    searchCandidates.some((searchDigits) => (
      phoneDigits.includes(searchDigits)
      || (
        phoneDigits.length >= MIN_FULL_PHONE_MATCH_DIGITS
        && searchDigits.length >= MIN_FULL_PHONE_MATCH_DIGITS
        && searchDigits.endsWith(phoneDigits)
      )
    ))
  ));
};
const shortPhoneSearchHintFor = (value: string, digits: string) => {
  if (!digits || !/^[\d\s()+.-]+$/.test(value.trim())) return '';

  if (digits.length < MIN_PHONE_SEARCH_DIGITS) {
    return `Para buscar por teléfono, usa al menos ${MIN_PHONE_SEARCH_DIGITS} dígitos del número.`;
  }

  if (digits.startsWith('0') && digits.slice(1).length < MIN_PHONE_SEARCH_DIGITS) {
    return `Para buscar teléfonos locales con 0 inicial, escribe al menos ${MIN_PHONE_SEARCH_DIGITS} dígitos después del 0.`;
  }

  return '';
};
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
  normalizeRegistrationContactValue(value)?.toLocaleLowerCase('es') ?? '';
const normalizePhoneComparisonValue = (value: string | null | undefined) => {
  const trimmedValue = normalizeRegistrationContactValue(value) ?? '';
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

const REGISTRATION_STATUS_PLACEHOLDER_VALUE_KEYS = new Set([
  '',
  '-',
  'desconocida',
  'desconocido',
  'n a',
  'na',
  'no disponible',
  'no asignada',
  'no asignado',
  'no definida',
  'no definido',
  'none',
  'not provided',
  'not set',
  'not available',
  'null',
  'pendiente por validar',
  'por actualizar',
  'por confirmar',
  'por definir',
  'por validar',
  'sin actualizar',
  'sin asignar',
  'sin estado',
  'sin status',
  'tbd',
  'unknown',
  'undefined',
]);

const normalizeRegistrationStatusPlaceholderKey = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[.!?:;]+$/g, '')
    .replace(/[^a-z0-9-]+/gi, ' ')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');

const isPlaceholderRegistrationStatus = (status: string) =>
  REGISTRATION_STATUS_PLACEHOLDER_VALUE_KEYS.has(normalizeRegistrationStatusPlaceholderKey(status));

const normalizeRegistrationStatusKey = (status: string) =>
  isPlaceholderRegistrationStatus(status)
    ? '__placeholder_registration_status__'
    : normalizeBackendStatusToken(status);

const normalizeKnownRegistrationStatus = (status: string): RegistrationStatus | null => {
  const statusFilter = normalizeStatusFilterAlias(status);
  return statusFilter && statusFilter !== 'all' ? statusFilter : null;
};

const customStatusLabelSpecialWords = new Map([
  ['api', 'API'],
  ['crm', 'CRM'],
  ['id', 'ID'],
  ['sms', 'SMS'],
  ['url', 'URL'],
  ['utm', 'UTM'],
  ['whatsapp', 'WhatsApp'],
]);

const formatCustomStatusWord = (word: string) => (
  customStatusLabelSpecialWords.get(word) ?? `${word.charAt(0).toLocaleUpperCase('es')}${word.slice(1)}`
);

const customRegistrationStatusLabel = (status: string) => {
  if (isPlaceholderRegistrationStatus(status)) return registrationStatusNeedsReviewLabel;

  const normalized = status.trim().toLowerCase().replace(/[\s._/-]+/g, ' ').trim();
  if (!normalized) return registrationStatusNeedsReviewLabel;
  return normalized.split(' ').map(formatCustomStatusWord).join(' ');
};

const registrationStatusLabel = (status: string) => {
  const knownStatus = normalizeKnownRegistrationStatus(status);
  return knownStatus ? statusFilterLabels[knownStatus] : customRegistrationStatusLabel(status);
};

const registrationStatusSearchValues = (status: string) => {
  const label = registrationStatusLabel(status);
  const knownStatus = normalizeKnownRegistrationStatus(status);

  if (knownStatus === 'paid') return [label, 'Pagada', 'Pagados', 'Pagadas'];
  if (knownStatus === 'cancelled') return [label, 'Cancelada', 'Cancelados', 'Canceladas'];
  if (knownStatus === 'pending_payment') return [label, 'Pendiente', 'Pendientes', 'Pagos pendientes'];
  return [label];
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

const statusFilterChipTitle = (
  status: StatusFilter,
  isActive: boolean,
  clearsLocalSearch: boolean,
) => (
  clearsLocalSearch
    ? `${statusFilterChipAriaLabel(status, isActive)} y limpiar la búsqueda actual.`
    : undefined
);
const statusFilterChipAccessibleLabel = (
  status: StatusFilter,
  isActive: boolean,
  clearsLocalSearch: boolean,
) => (
  statusFilterChipTitle(status, isActive, clearsLocalSearch)
  ?? statusFilterChipAriaLabel(status, isActive)
);

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
  normalizeKnownRegistrationStatus(currentStatus) === 'cancelled'
    ? 'Reabrir como pendiente'
    : normalizeKnownRegistrationStatus(currentStatus) == null
      ? 'Normalizar a pendiente'
      : 'Marcar pago pendiente';

const cancelStatusMenuLabel = (currentStatus: string) =>
  normalizeKnownRegistrationStatus(currentStatus) == null ? 'Normalizar a cancelado' : 'Cancelar inscripción';

const pendingStatusButtonLabel = (currentStatus: string, useCompactActionLabel: boolean) => {
  const knownStatus = normalizeKnownRegistrationStatus(currentStatus);
  if (knownStatus === 'cancelled' && useCompactActionLabel) return 'Reabrir';
  if (knownStatus === 'paid' && useCompactActionLabel) return 'Pasar a pendiente';
  return pendingStatusMenuLabel(currentStatus);
};

const pendingStatusMenuTargetLabel = (currentStatus: string) =>
  normalizeKnownRegistrationStatus(currentStatus) === 'cancelled'
    ? 'reabrir la inscripción como pendiente'
    : normalizeKnownRegistrationStatus(currentStatus) == null
      ? 'normalizar la inscripción a pendiente de pago'
      : 'marcar el pago como pendiente';

const cancelStatusMenuTargetLabel = (currentStatus: string) =>
  normalizeKnownRegistrationStatus(currentStatus) == null
    ? 'normalizar la inscripción como cancelada'
    : 'cancelar la inscripción';

const statusMenuButtonTitle = (currentStatus: string, targetLabel?: string) => {
  const currentStatusLabel = registrationStatusLabel(currentStatus);
  const targetSuffix = targetLabel ? ` para ${targetLabel}` : '';
  if (canOpenPaymentWorkflowFromStatus(currentStatus)) {
    return `${openPaymentWorkflowLabel} o cambiar estado${targetSuffix}; actual: ${currentStatusLabel}`;
  }
  return `Cambiar estado${targetSuffix}; actual: ${currentStatusLabel}`;
};

const statusMenuIconButtonAriaLabel = (currentStatus: string, targetLabel: string) => {
  const actionLabel = canOpenPaymentWorkflowFromStatus(currentStatus)
    ? `${openPaymentWorkflowLabel} o cambiar estado`
    : 'Cambiar estado';
  return `${actionLabel} para ${targetLabel}; estado actual: ${registrationStatusLabel(currentStatus)}`;
};

const paymentStatusMenuButtonAriaLabel = (targetLabel: string) =>
  `Abrir opciones de pago y estado para ${targetLabel}`;

const shouldUseDirectPendingRecoveryAction = (
  currentStatus: string,
  includePaidRecovery = false,
) => {
  const knownStatus = normalizeKnownRegistrationStatus(currentStatus);
  return knownStatus === 'cancelled' || (includePaidRecovery && knownStatus === 'paid');
};

const canCancelRegistrationFromStatus = (currentStatus: string) => {
  const knownStatus = normalizeKnownRegistrationStatus(currentStatus);
  return knownStatus == null || knownStatus === 'pending_payment';
};

const hasOnlyPendingRecoveryStatusAction = (currentStatus: string) => (
  canTransitionToStatus(currentStatus, 'pending_payment')
  && !canOpenPaymentWorkflowFromStatus(currentStatus)
  && !canCancelRegistrationFromStatus(currentStatus)
);

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
  return eventStatusLabels[normalized] ?? (trimmed || 'Estado no registrado');
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
  eventTypeLabels[normalizeBackendStatusToken(eventType)]
  ?? eventType
    .trim()
    .toLowerCase()
    .replace(/[\s._/-]+/g, ' ')
    .replace(/\b\w/g, (m) => m.toUpperCase());

const followUpTypeLabel = (entryType: string) => eventTypeLabel(entryType) || 'Seguimiento';

const getSharedEmailEventTypeLabel = (
  events: readonly Pick<CourseEmailEventDTO, 'ceEventType'>[],
) => {
  if (events.length < 2) return '';

  const labels = events.map((entry) => eventTypeLabel(entry.ceEventType).trim());
  const [firstLabel] = labels;

  if (!firstLabel || labels.some((label) => label !== firstLabel)) return '';
  return firstLabel;
};

const getSharedEmailEventStatusLabel = (
  events: readonly Pick<CourseEmailEventDTO, 'ceStatus'>[],
) => {
  if (events.length < 2) return '';

  const labels = events.map((entry) => eventStatusLabel(entry.ceStatus));
  const [firstLabel] = labels;

  if (!firstLabel || labels.some((label) => label !== firstLabel)) return '';
  return firstLabel;
};

const getSharedFollowUpTypeLabel = (
  followUps: readonly Pick<CourseRegistrationFollowUpDTO, 'crfEntryType'>[],
) => {
  if (followUps.length < 2) return '';

  const labels = followUps.map((entry) => followUpTypeLabel(entry.crfEntryType).trim());
  const [firstLabel] = labels;

  if (!firstLabel || labels.some((label) => label !== firstLabel)) return '';
  return firstLabel;
};

const getSharedReceiptNotes = (
  receipts: readonly Pick<CourseRegistrationReceiptDTO, 'crrNotes'>[],
) => {
  if (receipts.length < 2) return '';

  const notes = receipts.map((receipt) => receipt.crrNotes?.trim() ?? '');
  const [firstNote] = notes;

  if (!firstNote || notes.some((note) => note !== firstNote)) return '';
  return firstNote;
};

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

const followUpActionTargetLabel = (entry: CourseRegistrationFollowUpDTO) => {
  const subject = followUpSubjectLabel(entry);
  if (subject) return subject;

  const typeLabel = followUpTypeLabel(entry.crfEntryType);
  const createdLabel = formatOptionalDate(entry.crfCreatedAt);
  return createdLabel ? `${typeLabel} del ${createdLabel}` : typeLabel;
};

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
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[()[\]{}]+/g, ' ')
    .replace(/[_./-]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');

const escapeRegExp = (value: string) => value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

const normalizeFirstRunDescriptorSeparators = (value: string) =>
  value.replace(/\s*[\u00b7\u2022\u2013\u2014]\s*/g, ' - ');

const firstRunDecorativeEdgeMarkerPattern =
  /^(?:[\p{Extended_Pictographic}\p{Emoji_Presentation}\uFE0F\u200D]\s*)+|(?:\s*[\p{Extended_Pictographic}\p{Emoji_Presentation}\uFE0F\u200D])+$/gu;
const firstRunListMarkerPattern = /^(?:[-*•]\s+|\d+[.)]\s+)/;
const firstRunInvisibleFormatCharacterPattern = /[\u200B-\u200F\u202A-\u202E\u2060-\u206F\uFEFF]/g;
const firstRunUrlDescriptorPattern = String.raw`(?:(?:https?:\/\/|www\.)[^\s]+)`;
const firstRunUrlDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunUrlDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunUrlDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunUrlDescriptorPattern})\s*$`,
  'i',
);
const firstRunShortlinkProviderPattern = String.raw`(?:bit\s*\.?\s*ly|bitly|tiny\s*url|tinyurl|rebrandly|short\s*\.?\s*io|shortio|cutt\s*\.?\s*ly|cuttly|t\s*\.?\s*ly|tly)`;
const firstRunShortlinkDescriptorPattern = String.raw`(?:(?:${firstRunShortlinkProviderPattern})(?:\s+(?:(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up)\s+)?(?:(?:short|tracking|redirect)\s+)?(?:links?|urls?|pages?|forms?))?|(?:(?:short|tracking|redirect)\s+links?|link\s+shorteners?|shortened\s+urls?|short\s+urls?)|(?:enlaces?|links?|urls?)\s+cort[oa]s?|acortadores?\s+de\s+enlaces?)`;
const firstRunShortlinkDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunShortlinkDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunShortlinkDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunShortlinkDescriptorPattern})\s*$`,
  'i',
);

const firstRunCohortHtmlEntities: Record<string, string> = {
  amp: '&',
  apos: "'",
  gt: '>',
  lt: '<',
  nbsp: ' ',
  quot: '"',
};

const decodeFirstRunCohortCodePoint = (entity: string, codePoint: number) => (
  Number.isInteger(codePoint) && codePoint >= 0 && codePoint <= 0x10ffff
    ? String.fromCodePoint(codePoint)
    : `&${entity};`
);

const decodeFirstRunCohortHtmlEntity = (entity: string) => {
  const namedEntity = firstRunCohortHtmlEntities[entity.toLocaleLowerCase('es')];
  if (namedEntity != null) return namedEntity;

  if (entity.startsWith('#x') || entity.startsWith('#X')) {
    const codePoint = Number.parseInt(entity.slice(2), 16);
    return decodeFirstRunCohortCodePoint(entity, codePoint);
  }

  if (entity.startsWith('#')) {
    const codePoint = Number.parseInt(entity.slice(1), 10);
    return decodeFirstRunCohortCodePoint(entity, codePoint);
  }

  return `&${entity};`;
};

const decodeFirstRunCohortHtmlEntities = (value: string) =>
  value.replace(/&(amp|apos|gt|lt|nbsp|quot|#[0-9]+|#x[0-9a-f]+);/gi, (_, entity: string) =>
    decodeFirstRunCohortHtmlEntity(entity),
  );

const stripFirstRunCohortPresentationMarkers = (value: string) =>
  decodeFirstRunCohortHtmlEntities(value)
    .replace(firstRunInvisibleFormatCharacterPattern, '')
    .trim()
    .replace(firstRunListMarkerPattern, '')
    .replace(/^#{1,6}\s+/, '')
    .replace(/\*\*([^*\n]+)\*\*/g, '$1')
    .replace(/__([^_\n]+)__/g, '$1')
    .replace(/`([^`\n]+)`/g, '$1')
    .replace(/!\[([^\]\n]*)\]\([^)]+\)/g, '$1')
    .replace(/\[([^\]\n]+)\]\([^)]+\)/g, '$1')
    .replace(/<\/?(?:strong|b|em|i|span|a)[^>\n]*>/gi, '')
    .replace(
      /^\[(?:TODO|FIXME|DRAFT|BORRADOR|PENDING|PENDIENTE|POR\s+PUBLICAR|SIN\s+PUBLICAR|NO\s+PUBLICAR|WIP|TEST|QA|UAT|DEV|DEVELOPMENT|DESARROLLO|INTERNAL|INTERN[OA]|DEMO|SAMPLE|STAGING|SANDBOX|PREVIEW|PRUEBA|MUESTRA|EJEMPLO|VISTA\s+PREVIA|ARCHIVE|ARCHIVED|ARCHIVAD[OA]|BACKUP|RESPALDO)\]\s*/gi,
      '',
    )
    .replace(
      /^(?:TODO|FIXME|DRAFT|BORRADOR|PENDING|PENDIENTE|POR\s+PUBLICAR|SIN\s+PUBLICAR|NO\s+PUBLICAR|WIP|TEST|QA|UAT|DEV|DEVELOPMENT|DESARROLLO|INTERNAL|INTERN[OA]|DEMO|SAMPLE|STAGING|SANDBOX|PREVIEW|PRUEBA|MUESTRA|EJEMPLO|VISTA\s+PREVIA|ARCHIVE|ARCHIVED|ARCHIVAD[OA]|BACKUP|RESPALDO)\s*(?::|-|\u2013|\u2014)\s*/gi,
      '',
    )
    .replace(
      /\s*(?:[[(]\s*(?:TODO|FIXME|DRAFT|BORRADOR|PENDING|PENDIENTE|POR\s+PUBLICAR|SIN\s+PUBLICAR|NO\s+PUBLICAR|WIP|TEST|QA|UAT|DEV|DEVELOPMENT|DESARROLLO|INTERNAL|INTERN[OA]|DEMO|SAMPLE|STAGING|SANDBOX|PREVIEW|PRUEBA|MUESTRA|EJEMPLO|VISTA\s+PREVIA|ARCHIVE|ARCHIVED|ARCHIVAD[OA]|BACKUP|RESPALDO)\s*[\])]|\s*(?::|-|\u2013|\u2014)\s*(?:TODO|FIXME|DRAFT|BORRADOR|PENDING|PENDIENTE|POR\s+PUBLICAR|SIN\s+PUBLICAR|NO\s+PUBLICAR|WIP|TEST|QA|UAT|DEV|DEVELOPMENT|DESARROLLO|INTERNAL|INTERN[OA]|DEMO|SAMPLE|STAGING|SANDBOX|PREVIEW|PRUEBA|MUESTRA|EJEMPLO|VISTA\s+PREVIA|ARCHIVE|ARCHIVED|ARCHIVAD[OA]|BACKUP|RESPALDO))\s*$/gi,
      '',
    )
    .replace(firstRunListMarkerPattern, '')
    .replace(firstRunDecorativeEdgeMarkerPattern, '')
    .trim();

const humanizeCohortSlug = (slug: string) => {
  const normalized = slug.trim().replace(/[_./-]+/g, ' ').replace(/\s+/g, ' ');
  if (!normalized) return '';

  return normalized
    .split(' ')
    .map((part) => (part ? `${part.charAt(0).toLocaleUpperCase('es')}${part.slice(1)}` : part))
    .join(' ');
};

const readableCohortFallbackLabel = (slug: string) => humanizeCohortSlug(slug) || slug.trim();

const stripTrailingCohortSlug = (title: string, slug: string) => {
  const trimmedTitle = title.trim();
  const trimmedSlug = slug.trim();
  if (!trimmedTitle || !trimmedSlug) return trimmedTitle;

  const normalizedTitle = normalizeFirstRunDescriptorSeparators(trimmedTitle);
  const escapedSlug = escapeRegExp(trimmedSlug);
  const suffixPattern = new RegExp(
    `\\s*(?:\\(${escapedSlug}\\)|\\[${escapedSlug}\\]|[-:/|]\\s*${escapedSlug})\\s*$`,
    'i',
  );
  const strippedTitle = normalizedTitle.replace(suffixPattern, '').trim();
  return strippedTitle === normalizedTitle ? trimmedTitle : strippedTitle || trimmedSlug;
};

const dedupeRepeatedCohortTitleSegments = (title: string) => {
  const trimmedTitle = title.trim();
  const trailingWrappedSegment = /^(.*?)\s*(?:\(|\[)\s*([^)\]]+)\s*(?:\)|\])$/.exec(trimmedTitle);
  const leadingWrappedSegment = /^(?:\(|\[)\s*([^)\]]+)\s*(?:\)|\])\s*(.*)$/.exec(trimmedTitle);
  let unwrappedTitle = trimmedTitle;

  if (
    trailingWrappedSegment
    && normalizeCohortLabelKey(trailingWrappedSegment[1] ?? '')
      === normalizeCohortLabelKey(trailingWrappedSegment[2] ?? '')
  ) {
    unwrappedTitle = (trailingWrappedSegment[1] ?? '').trim();
  } else if (
    leadingWrappedSegment
    && normalizeCohortLabelKey(leadingWrappedSegment[1] ?? '')
      === normalizeCohortLabelKey(leadingWrappedSegment[2] ?? '')
  ) {
    unwrappedTitle = (leadingWrappedSegment[2] ?? '').trim();
  }

  const normalizedTitle = normalizeFirstRunDescriptorSeparators(unwrappedTitle);
  const parts = normalizedTitle
    .split(/\s*(?:[-:/|])\s*/)
    .map((part) => part.trim())
    .filter(Boolean);

  if (parts.length < 2) return unwrappedTitle;

  const uniqueParts: string[] = [];
  parts.forEach((part) => {
    const partKey = normalizeCohortLabelKey(part);
    if (!partKey || uniqueParts.some((uniquePart) => normalizeCohortLabelKey(uniquePart) === partKey)) return;
    uniqueParts.push(part);
  });

  if (uniqueParts.length === parts.length) return unwrappedTitle;
  return uniqueParts.join(' - ');
};

const firstRunApplicationDescriptorPrefixPattern =
  /^(?:(?:course\s+)?applications?(?:\s+(?:form|page|portal|packet))?|student\s+applications?\s+(?:form|page|portal|packet)|(?:student\s+|course\s+)?admissions?\s+packets?|application\s+(?:form|page|portal|packet)|formulario\s+de\s+(?:aplicaci[oó]n|postulaci[oó]n)|paquetes?\s+de\s+(?:aplicaci[oó]n|postulaci[oó]n|admisi[oó]n|admisiones|ingreso)|solicitud(?:es)?\s+de\s+postulaci[oó]n|postulaci[oó]n(?:es)?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunApplicationDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:course\s+)?applications?(?:\s+(?:form|page|portal|packet))?|student\s+applications?\s+(?:form|page|portal|packet)|(?:student\s+|course\s+)?admissions?\s+packets?|application\s+(?:form|page|portal|packet)|formulario\s+de\s+(?:aplicaci[oó]n|postulaci[oó]n)|paquetes?\s+de\s+(?:aplicaci[oó]n|postulaci[oó]n|admisi[oó]n|admisiones|ingreso)|solicitud(?:es)?\s+de\s+postulaci[oó]n|postulaci[oó]n(?:es)?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)\s*$/i;

const firstRunFinancialAidDescriptorPrefixPattern =
  /^(?:(?:scholarships?|grants?|financial\s+aid|tuition\s+(?:assistance|support)|payment\s+(?:assistance|support)|bursar(?:y|ies))\s+(?:(?:applications?|requests?)(?:\s+(?:forms?|pages?|portals?))?|forms?|pages?|portals?)|(?:forms?|pages?|portals?|applications?|requests?)\s+(?:for\s+)?(?:scholarships?|grants?|financial\s+aid|tuition\s+(?:assistance|support)|payment\s+(?:assistance|support)|bursar(?:y|ies))|(?:formulario|p[aá]gina|portal|solicitud(?:es)?)\s+(?:de|para(?:\s+la)?)\s+(?:becas?|ayuda\s+financiera|apoyo\s+econ[oó]mico|ayuda\s+de\s+matr[ií]cula|apoyo\s+de\s+matr[ií]cula|asistencia\s+de\s+matr[ií]cula|ayuda\s+de\s+pago|facilidades\s+de\s+pago)|(?:becas?|ayuda\s+financiera|apoyo\s+econ[oó]mico|ayuda\s+de\s+matr[ií]cula|apoyo\s+de\s+matr[ií]cula|asistencia\s+de\s+matr[ií]cula|ayuda\s+de\s+pago|facilidades\s+de\s+pago)\s+(?:formularios?|p[aá]ginas?|portales?|solicitud(?:es)?))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunFinancialAidDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:scholarships?|grants?|financial\s+aid|tuition\s+(?:assistance|support)|payment\s+(?:assistance|support)|bursar(?:y|ies))\s+(?:(?:applications?|requests?)(?:\s+(?:forms?|pages?|portals?))?|forms?|pages?|portals?)|(?:forms?|pages?|portals?|applications?|requests?)\s+(?:for\s+)?(?:scholarships?|grants?|financial\s+aid|tuition\s+(?:assistance|support)|payment\s+(?:assistance|support)|bursar(?:y|ies))|(?:formulario|p[aá]gina|portal|solicitud(?:es)?)\s+(?:de|para(?:\s+la)?)\s+(?:becas?|ayuda\s+financiera|apoyo\s+econ[oó]mico|ayuda\s+de\s+matr[ií]cula|apoyo\s+de\s+matr[ií]cula|asistencia\s+de\s+matr[ií]cula|ayuda\s+de\s+pago|facilidades\s+de\s+pago)|(?:becas?|ayuda\s+financiera|apoyo\s+econ[oó]mico|ayuda\s+de\s+matr[ií]cula|apoyo\s+de\s+matr[ií]cula|asistencia\s+de\s+matr[ií]cula|ayuda\s+de\s+pago|facilidades\s+de\s+pago)\s+(?:formularios?|p[aá]ginas?|portales?|solicitud(?:es)?))\s*$/i;

const firstRunOnboardingDescriptorPrefixPattern =
  /^(?:(?:(?:student|course|class|program)\s+)?onboarding\s+(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)|(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)\s+(?:for\s+)?(?:(?:student|course|class|program)\s+)?onboarding|(?:formulario|p[aá]gina|portal|paquete|enlace|link|url|checklist|lista\s+de\s+(?:tareas|verificaci[oó]n))\s+de\s+(?:onboarding|bienvenida|inducci[oó]n)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunOnboardingDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:student|course|class|program)\s+)?onboarding\s+(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)|(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)\s+(?:for\s+)?(?:(?:student|course|class|program)\s+)?onboarding|(?:formulario|p[aá]gina|portal|paquete|enlace|link|url|checklist|lista\s+de\s+(?:tareas|verificaci[oó]n))\s+de\s+(?:onboarding|bienvenida|inducci[oó]n)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)\s*$/i;

const firstRunOrientationDescriptorPrefixPattern =
  /^(?:(?:(?:student|course|class|program)\s+)?orientation\s+(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)|(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)\s+(?:for\s+)?(?:(?:student|course|class|program)\s+)?orientation|(?:formulario|p[aá]gina|portal|paquete|enlace|link|url|checklist|lista\s+de\s+(?:tareas|verificaci[oó]n))\s+de\s+orientaci[oó]n(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunOrientationDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:student|course|class|program)\s+)?orientation\s+(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)|(?:forms?|pages?|portals?|packets?|links?|urls?|checklists?)\s+(?:for\s+)?(?:(?:student|course|class|program)\s+)?orientation|(?:formulario|p[aá]gina|portal|paquete|enlace|link|url|checklist|lista\s+de\s+(?:tareas|verificaci[oó]n))\s+de\s+orientaci[oó]n(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)\s*$/i;

const firstRunAuditionDescriptorPrefixPattern =
  /^(?:(?:audition|casting)\s+(?:forms?|pages?|portals?|sign[-\s]?ups?|registrations?|applications?)|(?:formulario|p[aá]gina|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?)\s+de\s+(?:audici[oó]n(?:es)?|casting)|(?:audici[oó]n(?:es)?|casting)\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunAuditionDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:audition|casting)\s+(?:forms?|pages?|portals?|sign[-\s]?ups?|registrations?|applications?)|(?:formulario|p[aá]gina|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?)\s+de\s+(?:audici[oó]n(?:es)?|casting)|(?:audici[oó]n(?:es)?|casting)\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))\s*$/i;

const firstRunAssessmentDescriptorPrefixPattern =
  /^(?:(?:course\s+)?(?:placement\s+test|level\s+test|assessment|diagnostic|quiz(?:zes)?)\s+(?:forms?|pages?|portals?|tests?|quiz(?:zes)?|funnels?)|(?:formulario|p[aá]gina|prueba|test|cuestionario)\s+de\s+(?:nivel|ubicaci[oó]n|diagn[oó]stico|evaluaci[oó]n)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunAssessmentDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:course\s+)?(?:placement\s+test|level\s+test|assessment|diagnostic|quiz(?:zes)?)\s+(?:forms?|pages?|portals?|tests?|quiz(?:zes)?|funnels?)|(?:formulario|p[aá]gina|prueba|test|cuestionario)\s+de\s+(?:nivel|ubicaci[oó]n|diagn[oó]stico|evaluaci[oó]n)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)\s*$/i;

const firstRunCohortDescriptorPrefixPattern =
  /^(?:cohorte|cohort|grupo|group|batch|ciclo|cycle|edici[oó]n|edition)\s*[-:/|]\s*/i;

const firstRunCohortDescriptorSuffixPattern =
  /\s*[-:/|]\s*(?:cohorte|cohort|grupo|group|batch|ciclo|cycle|edici[oó]n|edition)\s*$/i;

const firstRunCourseNounDescriptorPrefixPattern =
  /^(?:(?:curso|course|clase|class|programa|program)\s*(?:[-:/|]\s*|(?:de|del|para(?:\s+el)?|for)\s+))/i;

const firstRunCourseNounDescriptorSuffixPattern =
  /(?:\s*[-:/|]\s*|\s+(?:de|del|para(?:\s+el)?|for)\s+)(?:curso|course|clase|class|programa|program)\s*$/i;

const firstRunUntitledDescriptorPrefixPattern =
  /^(?:(?:untitled|sin\s+t[ií]tulo)(?=\s*(?:[-:/|]|$))|untitled\s+(?:forms?|pages?|portals?|links?|urls?)|new\s+(?:forms?|pages?|portals?|links?|urls?)|(?:formulario|p[aá]gina|portal|enlace|link|url)\s+sin\s+t[ií]tulo|nuevo\s+(?:formulario|p[aá]gina|portal|enlace|link|url))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunUntitledDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:untitled(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|new\s+(?:forms?|pages?|portals?|links?|urls?)|sin\s+t[ií]tulo|(?:formulario|p[aá]gina|portal|enlace|link|url)\s+sin\s+t[ií]tulo|nuevo\s+(?:formulario|p[aá]gina|portal|enlace|link|url))\s*$/i;

const firstRunVariantDescriptorPrefixPattern =
  /^(?:(?:(?:a\s*\/\s*b|ab|split)\s+tests?)|(?:tests?|pruebas?)\s+a\s*\/\s*b|(?:variant|variation|variante|variaci[oó]n)\s+(?:[a-z]\d*|\d+|uno|dos|tres|cuatro))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunVariantDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:a\s*\/\s*b|ab|split)\s+tests?)|(?:tests?|pruebas?)\s+a\s*\/\s*b|(?:variant|variation|variante|variaci[oó]n)\s+(?:[a-z]\d*|\d+|uno|dos|tres|cuatro))\s*$/i;

const firstRunTemplateDescriptorPrefixPattern =
  /^(?:(?:template|plantilla)(?:\s+(?:forms?|pages?|portals?|links?|urls?|formularios?|p[aá]ginas?|portales?|enlaces?))?|(?:forms?|pages?|portals?|links?|urls?|formularios?|p[aá]ginas?|portales?|enlaces?)\s+(?:template|plantilla))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunTemplateDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:template|plantilla)(?:\s+(?:forms?|pages?|portals?|links?|urls?|formularios?|p[aá]ginas?|portales?|enlaces?))?|(?:forms?|pages?|portals?|links?|urls?|formularios?|p[aá]ginas?|portales?|enlaces?)\s+(?:template|plantilla))\s*$/i;

const firstRunCopyDescriptorPrefixPattern =
  /^(?:(?:copy|duplicates?|duplicated)(?:\s*[-#_]?\s*\d+)?(?:[\s_]+of(?:[\s_]+|\s*[-:/|_]\s*)|\s*[-:/|_]\s*)|(?:clone|cloned)(?:\s*[-#_]?\s*\d+)?(?:[\s_]+(?:of|from)(?:[\s_]+|\s*[-:/|_]\s*)|\s*[-:/|_]\s*)|(?:copia|duplicad[oa]s?|clon(?:ad[oa])?)(?:\s*[-#_]?\s*\d+)?(?:[\s_]+de(?:[\s_]+|\s*[-:/|_]\s*)|\s*[-:/|_]\s*))\s*/i;

const firstRunCopyDescriptorSuffixPattern =
  /\s*(?:(?:[-:/|_]\s*)|(?:\(\s*)|(?:\[\s*))?(?:copy|duplicate|clone|cloned|copia|duplicad[oa]|clon(?:ad[oa])?)(?:\s*[-#_]?\s*\d+)?\s*(?:\)|\])?\s*$/i;

const firstRunRegistrationLinkDescriptorPrefixPattern =
  /^(?:(?:(?:public\s+)?(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist)\s+(?:links?|urls?))|(?:(?:links?|enlaces?|v[ií]nculos?|urls?)\s+(?:p[uú]blic[oa]s?\s+)?(?:de|para)\s+(?:pre)?inscripci[oó]n)|(?:(?:links?|enlaces?|v[ií]nculos?|urls?)\s+(?:del?\s+curso|de\s+curso|p[uú]blicos?)))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunRegistrationLinkDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:public\s+)?(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist)\s+(?:links?|urls?))|(?:(?:links?|enlaces?|v[ií]nculos?|urls?)\s+(?:p[uú]blic[oa]s?\s+)?(?:de|para)\s+(?:pre)?inscripci[oó]n)|(?:(?:links?|enlaces?|v[ií]nculos?|urls?)\s+(?:del?\s+curso|de\s+curso|p[uú]blicos?)))\s*$/i;

const firstRunQrRegistrationDescriptorPattern = String.raw`(?:(?:qr\s*(?:code)?|c[oó]digo\s+qr)\s+(?:(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist)(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|(?:forms?|pages?|portals?|links?|urls?)\s+(?:for\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist)|(?:de|para(?:\s+la)?)\s+(?:pre)?inscripci[oó]n(?:es)?|(?:de|para)\s+registro|(?:de|para)\s+matr[ií]cula)|(?:(?:formulario|p[aá]gina|portal|enlaces?|links?|urls?)\s+(?:de|para)\s+(?:qr|c[oó]digo\s+qr)(?:\s+(?:(?:de|para(?:\s+la)?)\s+(?:pre)?inscripci[oó]n(?:es)?|(?:de|para)\s+registro|(?:de|para)\s+matr[ií]cula))?))`;
const firstRunQrRegistrationDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunQrRegistrationDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunQrRegistrationDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunQrRegistrationDescriptorPattern})\s*$`,
  'i',
);

const firstRunEnrollmentFlowDescriptorPrefixPattern =
  /^(?:(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|admissions?|application|intake|sign[-\s]?up)\s+(?:flows?|funnels?|workflows?|pipelines?|landing(?:\s+pages?)?)|(?:flujos?|embudos?|canalizaci[oó]n(?:es)?)\s+(?:de|para)\s+(?:pre)?inscripci[oó]n(?:es)?|(?:flujos?|embudos?|canalizaci[oó]n(?:es)?)\s+(?:de|para)\s+(?:admisiones|ingreso))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunEnrollmentFlowDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|admissions?|application|intake|sign[-\s]?up)\s+(?:flows?|funnels?|workflows?|pipelines?|landing(?:\s+pages?)?)|(?:flujos?|embudos?|canalizaci[oó]n(?:es)?)\s+(?:de|para)\s+(?:pre)?inscripci[oó]n(?:es)?|(?:flujos?|embudos?|canalizaci[oó]n(?:es)?)\s+(?:de|para)\s+(?:admisiones|ingreso))\s*$/i;

const firstRunOfferDescriptorPrefixPattern =
  /^(?:(?:early[-\s]?bird|promo(?:tion(?:al)?)?(?:\s+codes?)?|discount(?:ed)?(?:\s+codes?)?|coupon(?:\s+codes?)?|pre[-\s]?sale)\s+(?:(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|sign[-\s]?up)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?|forms?|pages?|links?|urls?|portals?)|(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?)\s+de\s+(?:descuento|promoci[oó]n|oferta|preventa|cup[oó]n(?:es)?|c[oó]digos?\s+(?:promocional(?:es)?|de\s+descuento))(?:\s+de\s+(?:inscripci[oó]n|registro|matr[ií]cula))?|(?:descuento|promoci[oó]n|oferta|preventa|cup[oó]n(?:es)?|c[oó]digos?\s+(?:promocional(?:es)?|de\s+descuento))\s+de\s+(?:inscripci[oó]n|registro|matr[ií]cula))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunOfferDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:early[-\s]?bird|promo(?:tion(?:al)?)?(?:\s+codes?)?|discount(?:ed)?(?:\s+codes?)?|coupon(?:\s+codes?)?|pre[-\s]?sale)\s+(?:(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|sign[-\s]?up)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?|forms?|pages?|links?|urls?|portals?)|(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?)\s+de\s+(?:descuento|promoci[oó]n|oferta|preventa|cup[oó]n(?:es)?|c[oó]digos?\s+(?:promocional(?:es)?|de\s+descuento))(?:\s+de\s+(?:inscripci[oó]n|registro|matr[ií]cula))?|(?:descuento|promoci[oó]n|oferta|preventa|cup[oó]n(?:es)?|c[oó]digos?\s+(?:promocional(?:es)?|de\s+descuento))\s+de\s+(?:inscripci[oó]n|registro|matr[ií]cula))\s*$/i;

const firstRunUrgencyOfferDescriptorPrefixPattern =
  /^(?:(?:(?:last[-\s]?chance|final\s+call|closing|deadline)\s+(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|sign[-\s]?up)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?)|(?:(?:last[-\s]?chance|final\s+call|closing|deadline)\s+(?:forms?|pages?|links?|urls?|portals?))|(?:(?:registration|enrollment|sign[-\s]?up)\s+(?:deadline|closing)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?)|(?:[uú]ltimos?\s+cupos?|cierre\s+de\s+(?:inscripciones|inscripci[oó]n|registro|matr[ií]cula)|fecha\s+l[ií]mite\s+de\s+(?:inscripciones|inscripci[oó]n|registro|matr[ií]cula))(?:\s+(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunUrgencyOfferDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:last[-\s]?chance|final\s+call|closing|deadline)\s+(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|sign[-\s]?up)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?)|(?:(?:last[-\s]?chance|final\s+call|closing|deadline)\s+(?:forms?|pages?|links?|urls?|portals?))|(?:(?:registration|enrollment|sign[-\s]?up)\s+(?:deadline|closing)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?)|(?:[uú]ltimos?\s+cupos?|cierre\s+de\s+(?:inscripciones|inscripci[oó]n|registro|matr[ií]cula)|fecha\s+l[ií]mite\s+de\s+(?:inscripciones|inscripci[oó]n|registro|matr[ií]cula))(?:\s+(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?))?)\s*$/i;

const firstRunCallToActionDescriptorPrefixPattern =
  /^(?:(?:apply|enroll|register|sign\s*up)\s+(?:for|to|in)\s+(?:the\s+)?(?:course|class|program)|(?:apply|enroll|register|sign\s*up)\s+(?:now|here|for|to|in)\b|(?:start|begin|open|complete|continue)\s+(?:(?:your|the)\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|admission|sign[-\s]?up)(?:\s+(?:form|page|portal))?|(?:aplica|inscr[ií]bete|matric[uú]late|postula|reg[ií]strate)\s+(?:ahora|aqu[ií]|al\s+curso|a\s+la\s+clase|en\s+el\s+curso|para\s+el\s+curso|al\s+programa)|(?:inicia|empieza|comienza|abre|completa|contin[uú]a)\s+(?:tu\s+|la\s+)?(?:pre)?(?:inscripci[oó]n|matr[ií]cula|postulaci[oó]n|aplicaci[oó]n|registro))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunCallToActionDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:apply|enroll|register|sign\s*up)\s+(?:for|to|in)\s+(?:the\s+)?(?:course|class|program)|(?:apply|enroll|register|sign\s*up)\s+(?:now|here|for|to|in)\b|(?:start|begin|open|complete|continue)\s+(?:(?:your|the)\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|admission|sign[-\s]?up)(?:\s+(?:form|page|portal))?|(?:aplica|inscr[ií]bete|matric[uú]late|postula|reg[ií]strate)\s+(?:ahora|aqu[ií]|al\s+curso|a\s+la\s+clase|en\s+el\s+curso|para\s+el\s+curso|al\s+programa)|(?:inicia|empieza|comienza|abre|completa|contin[uú]a)\s+(?:tu\s+|la\s+)?(?:pre)?(?:inscripci[oó]n|matr[ií]cula|postulaci[oó]n|aplicaci[oó]n|registro))\s*$/i;

const firstRunSeatReservationCallToActionDescriptorPrefixPattern =
  /^(?:(?:reserve|save|claim|book)\s+(?:(?:your|my|a|the)\s+)?(?:spot|seat|place)|join\s+(?:(?:the|my)\s+)?(?:course|class|program|cohort|waitlist|waiting\s+list)|(?:reserva|aparta|separa|asegura|guarda)\s+(?:(?:tu|mi|un|el|la)\s+)?(?:cupo|plaza|lugar|asiento)|(?:[uú]nete|unete)\s+(?:al\s+curso|a\s+la\s+clase|al\s+programa|a\s+la\s+cohorte|a\s+la\s+lista))(?:\s+(?:now|here|ahora|aqu[ií]|del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunSeatReservationCallToActionDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:reserve|save|claim|book)\s+(?:(?:your|my|a|the)\s+)?(?:spot|seat|place)|join\s+(?:(?:the|my)\s+)?(?:course|class|program|cohort|waitlist|waiting\s+list)|(?:reserva|aparta|separa|asegura|guarda)\s+(?:(?:tu|mi|un|el|la)\s+)?(?:cupo|plaza|lugar|asiento)|(?:[uú]nete|unete)\s+(?:al\s+curso|a\s+la\s+clase|al\s+programa|a\s+la\s+cohorte|a\s+la\s+lista))(?:\s+(?:now|here|ahora|aqu[ií]))?\s*$/i;

const firstRunOpenEnrollmentDescriptorPrefixPattern =
  /^(?:(?:open|abiertas?)\s+(?:course\s+)?(?:(?:pre[-\s]?)?registrations?|enrollments?|enrolments?|sign[-\s]?ups?)|(?:course\s+)?(?:(?:pre[-\s]?)?registrations?|enrollments?|enrolments?|sign[-\s]?ups?)\s+open|(?:pre[-\s]?)?inscripci[oó]n(?:es)?\s+abiertas?|matr[ií]cula(?:s)?\s+abiertas?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunOpenEnrollmentDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:open|abiertas?)\s+(?:course\s+)?(?:(?:pre[-\s]?)?registrations?|enrollments?|enrolments?|sign[-\s]?ups?)|(?:course\s+)?(?:(?:pre[-\s]?)?registrations?|enrollments?|enrolments?|sign[-\s]?ups?)\s+open|(?:pre[-\s]?)?inscripci[oó]n(?:es)?\s+abiertas?|matr[ií]cula(?:s)?\s+abiertas?)\s*$/i;

const firstRunSalesDescriptorPrefixPattern =
  /^(?:(?:(?:(?:shopify|woo\s*commerce|woocommerce|gum\s*road|gumroad|payhip|samcart|thrivecart|kiwify)\s+)?(?:sales?|purchase|order|tickets?|products?|store(?:front)?s?)\s+(?:pages?|forms?|links?|urls?|portals?)|(?:p[aá]ginas?|formularios?|enlaces?|links?|urls?|portales?)\s+de\s+(?:venta|ventas|compra|compras|productos?|tiendas?|tickets?|entradas?|boletos?)|(?:p[aá]gina|formulario)\s+(?:de\s+)?(?:ventas?|compras?|productos?|tiendas?|tickets?|entradas?|boletos?))(?:\s+(?:del|de|para\s+el|para|for)\s+|\s*[-:/|]\s*)|(?:tickets?|entradas?|boletos?)\s*[-:/|]\s*)/i;

const firstRunSalesDescriptorSuffixPattern =
  /(?:(?:\s*[-:/|]\s*|\s+(?:del|de|para\s+el|para|for)\s+)(?:(?:(?:shopify|woo\s*commerce|woocommerce|gum\s*road|gumroad|payhip|samcart|thrivecart|kiwify)\s+)?(?:sales?|purchase|order|tickets?|products?|store(?:front)?s?)\s+(?:pages?|forms?|links?|urls?|portals?)|(?:p[aá]ginas?|formularios?|enlaces?|links?|urls?|portales?)\s+de\s+(?:venta|ventas|compra|compras|productos?|tiendas?|tickets?|entradas?|boletos?)|(?:p[aá]gina|formulario)\s+(?:de\s+)?(?:ventas?|compras?|productos?|tiendas?|tickets?|entradas?|boletos?))|\s*[-:/|]\s*(?:tickets?|entradas?|boletos?))\s*$/i;

const firstRunPaymentDescriptorPrefixPattern =
  /^(?:(?:(?:stripe|paypal|payphone|datafast|kushki|paymentez|deuna|mercado\s*pago|mercadopago|kiwify|shopify|woo\s*commerce|woocommerce|gum\s*road|gumroad|lemon\s*squeezy|payhip|samcart|thrivecart)\s+)?(?:(?:online\s+)?(?:course\s+)?(?:payment(?:[-\s]+plans?)?|checkout|deposit|down\s*payment|reservation\s+payment)\s+(?:forms?|pages?|links?|urls?|portals?|buttons?)|checkout)|(?:formulario|p[aá]gina|enlaces?|links?|urls?|portal(?:es)?|bot[oó]n(?:es)?)\s+de\s+(?:pago|plan(?:es)?\s+de\s+pagos?|checkout|dep[oó]sito|abono|reserva(?:\s+de\s+cupo)?)(?:\s+(?:en\s+l[ií]nea|online))?|(?:checkout|pago|plan(?:es)?\s+de\s+pagos?|dep[oó]sito|abono|reserva(?:\s+de\s+cupo)?)\s+(?:del?\s+curso|de\s+curso))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunPaymentDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:stripe|paypal|payphone|datafast|kushki|paymentez|deuna|mercado\s*pago|mercadopago|kiwify|shopify|woo\s*commerce|woocommerce|gum\s*road|gumroad|lemon\s*squeezy|payhip|samcart|thrivecart)\s+)?(?:(?:online\s+)?(?:course\s+)?(?:payment(?:[-\s]+plans?)?|checkout|deposit|down\s*payment|reservation\s+payment)\s+(?:forms?|pages?|links?|urls?|portals?|buttons?)|checkout)|(?:formulario|p[aá]gina|enlaces?|links?|urls?|portal(?:es)?|bot[oó]n(?:es)?)\s+de\s+(?:pago|plan(?:es)?\s+de\s+pagos?|checkout|dep[oó]sito|abono|reserva(?:\s+de\s+cupo)?)(?:\s+(?:en\s+l[ií]nea|online))?|(?:checkout|pago|plan(?:es)?\s+de\s+pagos?|dep[oó]sito|abono|reserva(?:\s+de\s+cupo)?)\s+(?:del?\s+curso|de\s+curso))\s*$/i;

const firstRunPaymentEvidenceDescriptorPrefixPattern =
  /^(?:(?:payment\s+(?:receipts?|proof|evidence|confirmations?|invoices?)|proof\s+of\s+payment|receipts?\s+uploads?|invoice\s+uploads?)(?:\s+(?:forms?|pages?|links?|urls?|portals?|uploads?))?|(?:comprobantes?|evidencias?|recibos?|facturas?)\s+(?:de\s+)?pago(?:\s+(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?))?|confirmaci[oó]n(?:es)?\s+(?:de\s+)?pago(?:\s+(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunPaymentEvidenceDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:payment\s+(?:receipts?|proof|evidence|confirmations?|invoices?)|proof\s+of\s+payment|receipts?\s+uploads?|invoice\s+uploads?)(?:\s+(?:forms?|pages?|links?|urls?|portals?|uploads?))?|(?:comprobantes?|evidencias?|recibos?|facturas?)\s+(?:de\s+)?pago(?:\s+(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?))?|confirmaci[oó]n(?:es)?\s+(?:de\s+)?pago(?:\s+(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?))?)\s*$/i;

const firstRunAgreementDescriptorPattern = String.raw`(?:(?:(?:course|student|parent|guardian|minor|enrollment|registration|liability|media|photo)\s+(?:agreements?|contracts?|waivers?|consent|release|permission\s+slips?)(?:\s+(?:forms?|pages?|links?|urls?|portals?|packets?))?)|(?:(?:consent|release)\s+forms?|permission\s+slips?|liability\s+waivers?|media\s+releases?|photo\s+releases?)|(?:(?:forms?|pages?|links?|urls?|portals?|packets?)\s+(?:for\s+)?(?:course|student|parent|guardian|minor|enrollment|registration|liability|media|photo)\s+(?:agreements?|contracts?|waivers?|consent|release|permission\s+slips?))|(?:(?:contratos?|acuerdos?|autorizaciones?|consentimientos?|exoneraciones?|permisos?)\s+(?:de|para)\s+(?:matr[ií]cula|inscripci[oó]n|estudiantes?|alumnos?|curso|imagen|fotos?|responsabilidad|representantes?|tutor(?:es)?|menor(?:es)?|padres?|madres?))|(?:(?:formularios?|p[aá]ginas?|enlaces?|links?|urls?|portales?|paquetes?)\s+de\s+(?:contrato|acuerdo|autorizaci[oó]n|consentimiento|exoneraci[oó]n|permiso)(?:\s+(?:de|para)\s+(?:matr[ií]cula|inscripci[oó]n|estudiantes?|alumnos?|curso|imagen|fotos?|responsabilidad|representantes?|tutor(?:es)?|menor(?:es)?|padres?|madres?))?))`;
const firstRunAgreementDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunAgreementDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for)\s+|\s*[-:/|]\s*)`,
  'i',
);
const firstRunAgreementDescriptorSuffixPattern = new RegExp(
  String.raw`(?:\s*[-:/|]\s*|\s+(?:del|de|para\s+el|para|for)\s+)(?:${firstRunAgreementDescriptorPattern})\s*$`,
  'i',
);

const firstRunSignupSheetDescriptorPrefixPattern =
  /^(?:(?:(?:google|(?:microsoft|ms))\s+)?(?:course\s+)?(?:(?:sign[-\s]?up|(?:pre[-\s]?)?registration|enrollment)\s+(?:sheets?|spreadsheets?))|(?:hoja|planilla)(?:\s+de\s+c[aá]lculo)?\s+de\s+(?:pre)?inscripci[oó]n|(?:hoja|planilla)(?:\s+de\s+c[aá]lculo)?\s+de\s+registro)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunSignupSheetDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:google|(?:microsoft|ms))\s+)?(?:course\s+)?(?:(?:sign[-\s]?up|(?:pre[-\s]?)?registration|enrollment)\s+(?:sheets?|spreadsheets?))|(?:hoja|planilla)(?:\s+de\s+c[aá]lculo)?\s+de\s+(?:pre)?inscripci[oó]n|(?:hoja|planilla)(?:\s+de\s+c[aá]lculo)?\s+de\s+registro)\s*$/i;

const firstRunRosterDescriptorPattern = String.raw`(?:(?:(?:course|class|student|students?)\s+(?:rosters?|lists?|directories?))|(?:(?:rosters?|lists?|directories?)\s+(?:for\s+)?(?:course|class|students?))|(?:(?:lista|listado|directorio|n[oó]mina)\s+(?:de|para)\s+(?:estudiantes|alumnos|curso|clase))|(?:(?:estudiantes|alumnos)\s+(?:del?|de|para\s+el)\s+(?:curso|clase)))`;
const firstRunRosterDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunRosterDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunRosterDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunRosterDescriptorPattern})\s*$`,
  'i',
);

const firstRunAttendanceDescriptorPattern = String.raw`(?:(?:(?:course|class|student|students?)\s+)?(?:attendance|check[-\s]?in|roll\s*call)\s+(?:forms?|pages?|links?|urls?|portals?|sheets?|lists?|trackers?)|(?:(?:forms?|pages?|links?|urls?|portals?|sheets?|lists?|trackers?)\s+(?:for\s+)?(?:(?:course|class|student|students?)\s+)?(?:attendance|check[-\s]?in|roll\s*call))|(?:(?:lista|listado|hoja|planilla|formulario|p[aá]gina|enlace|link|url|portal|registro)\s+(?:de|para)\s+(?:asistencia|check[-\s]?in|ingreso)(?:\s+(?:del?\s+curso|de\s+curso|de\s+clase|de\s+estudiantes?))?))`;
const firstRunAttendanceDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunAttendanceDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunAttendanceDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunAttendanceDescriptorPattern})\s*$`,
  'i',
);

const firstRunWorkshopDescriptorPrefixPattern =
  /^(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?(?:\s+de\s+(?:pre)?inscripci[oó]n)?|(?:pre)?inscripciones?|matr[ií]culas?|admisi[oó]n|landing)\s+(?:del?|de|al|para\s+el|para)\s+taller|workshop\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up)(?:\s+(?:form|page))?)(?:\s*(?:[-:/|]\s*)?)/i;

const firstRunWorkshopDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?(?:\s+de\s+(?:pre)?inscripci[oó]n)?|(?:pre)?inscripciones?|matr[ií]culas?|admisi[oó]n|landing)\s+(?:del?|de|al|para\s+el|para)\s+taller|workshop\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up)(?:\s+(?:form|page))?)\s*$/i;

const firstRunClassDescriptorPrefixPattern =
  /^(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:pre)?inscripci[oó]n\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|(?:pre)?inscripci[oó]n(?:es)?\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|matr[ií]culas?\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|admisi[oó]n\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?|landing)\s+(?:a\s+la|de\s+la|para\s+la)\s+clase|(?:master\s*class|masterclass|class)\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up)(?:\s+(?:form|page))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunClassDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:pre)?inscripci[oó]n\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|(?:pre)?inscripci[oó]n(?:es)?\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|matr[ií]culas?\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|admisi[oó]n\s+(?:a\s+la|de(?:\s+la)?|para\s+la)\s+clase|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?|landing)\s+(?:a\s+la|de\s+la|para\s+la)\s+clase|(?:master\s*class|masterclass|class)\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up)(?:\s+(?:form|page))?)\s*$/i;

const firstRunTrialLessonDescriptorPrefixPattern =
  /^(?:(?:free\s+)?(?:trial|sample|demo)\s+(?:class|lesson|session)\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up|forms?|pages?|portals?)|(?:forms?|pages?|portals?)\s+(?:for\s+)?(?:free\s+)?(?:trial|sample|demo)\s+(?:class|lesson|session)|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?)\s+(?:de|para(?:\s+la)?)\s+(?:clase|lecci[oó]n|sesi[oó]n)\s+de\s+prueba|(?:clase|lecci[oó]n|sesi[oó]n)\s+de\s+prueba\s+(?:formularios?|fichas?|p[aá]ginas?|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunTrialLessonDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:free\s+)?(?:trial|sample|demo)\s+(?:class|lesson|session)\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up|forms?|pages?|portals?)|(?:forms?|pages?|portals?)\s+(?:for\s+)?(?:free\s+)?(?:trial|sample|demo)\s+(?:class|lesson|session)|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?)\s+(?:de|para(?:\s+la)?)\s+(?:clase|lecci[oó]n|sesi[oó]n)\s+de\s+prueba|(?:clase|lecci[oó]n|sesi[oó]n)\s+de\s+prueba\s+(?:formularios?|fichas?|p[aá]ginas?|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?))\s*$/i;

const firstRunProgramDescriptorPrefixPattern =
  /^(?:(?:program(?:me)?|training)\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up)(?:\s+(?:form|page|portal))?|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:pre)?inscripci[oó]n\s+(?:al|del?|de(?:\s+el)?|para\s+el)\s+programa|(?:pre)?inscripci[oó]n(?:es)?\s+(?:al|del?|de(?:\s+el)?|para\s+el)\s+programa|matr[ií]culas?\s+(?:al|del?|de(?:\s+el)?|para\s+el)\s+programa)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunProgramDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:program(?:me)?|training)\s+(?:(?:pre[-\s]?)?registration|enrollment|applications?|sign[-\s]?up)(?:\s+(?:form|page|portal))?|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:pre)?inscripci[oó]n\s+(?:al|del?|de(?:\s+el)?|para\s+el)\s+programa|(?:pre)?inscripci[oó]n(?:es)?\s+(?:al|del?|de(?:\s+el)?|para\s+el)\s+programa|matr[ií]culas?\s+(?:al|del?|de(?:\s+el)?|para\s+el)\s+programa)\s*$/i;

const firstRunWaitlistDescriptorPrefixPattern =
  /^(?:(?:formulario|p[aá]gina|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?)\s+(?:de|para(?:\s+la)?|del?)\s+(?:lista\s+de\s+(?:espera|interesad[oa]s)|interesad[oa]s|(?:captaci[oó]n|captura|generaci[oó]n)\s+de\s+(?:leads?|prospectos|interesad[oa]s)|newsletter|bolet[ií]n|lista\s+de\s+correo)|(?:suscripci[oó]n|registro)\s+(?:al|a\s+la|para\s+el|para\s+la|del?|de)\s+(?:newsletter|bolet[ií]n|lista\s+de\s+correo)|lista\s+de\s+(?:espera|interesad[oa]s)|(?:captaci[oó]n|captura|generaci[oó]n)\s+de\s+(?:leads?|prospectos|interesad[oa]s)|waitlist(?:\s+(?:form|page))?|waiting\s+list(?:\s+(?:form|page))?|interest(?:ed)?\s+list(?:\s+(?:form|page))?|(?:course\s+)?interest(?:ed)?\s+sign[-\s]?up(?:\s+(?:form|page))?|newsletter\s+(?:(?:sign[-\s]?up|signup|subscription|subscribe)(?:\s+(?:forms?|pages?))?|forms?|pages?)|mailing\s+list(?:\s+(?:(?:sign[-\s]?up|signup|subscription|subscribe)(?:\s+(?:forms?|pages?))?|forms?|pages?))?|lead\s+list(?:\s+(?:form|page))?|lead\s+capture(?:\s+(?:form|page))?|lead[-\s]+gen(?:eration)?(?:\s+(?:forms?|pages?))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunWaitlistDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:formulario|p[aá]gina|solicitud(?:es)?|registro(?:s)?|inscripci[oó]n(?:es)?)\s+(?:de|para(?:\s+la)?|del?)\s+(?:lista\s+de\s+(?:espera|interesad[oa]s)|interesad[oa]s|(?:captaci[oó]n|captura|generaci[oó]n)\s+de\s+(?:leads?|prospectos|interesad[oa]s)|newsletter|bolet[ií]n|lista\s+de\s+correo)|(?:suscripci[oó]n|registro)\s+(?:al|a\s+la|para\s+el|para\s+la|del?|de)\s+(?:newsletter|bolet[ií]n|lista\s+de\s+correo)|lista\s+de\s+(?:espera|interesad[oa]s)|(?:captaci[oó]n|captura|generaci[oó]n)\s+de\s+(?:leads?|prospectos|interesad[oa]s)|waitlist(?:\s+(?:form|page))?|waiting\s+list(?:\s+(?:form|page))?|interest(?:ed)?\s+list(?:\s+(?:form|page))?|(?:course\s+)?interest(?:ed)?\s+sign[-\s]?up(?:\s+(?:form|page))?|newsletter\s+(?:(?:sign[-\s]?up|signup|subscription|subscribe)(?:\s+(?:forms?|pages?))?|forms?|pages?)|mailing\s+list(?:\s+(?:(?:sign[-\s]?up|signup|subscription|subscribe)(?:\s+(?:forms?|pages?))?|forms?|pages?))?|lead\s+list(?:\s+(?:form|page))?|lead\s+capture(?:\s+(?:form|page))?|lead[-\s]+gen(?:eration)?(?:\s+(?:forms?|pages?))?)\s*$/i;

const firstRunPriorityWaitlistDescriptorPrefixPattern =
  /^(?:(?:vip|priority|early\s+access|acceso\s+anticipado)\s+(?:waitlist|waiting\s+list|interest(?:ed)?\s+list|list|lista(?:\s+de\s+(?:espera|interesad[oa]s))?)|(?:lista\s+(?:vip|prioritaria|prioritario|de\s+prioridad)|lista\s+de\s+espera\s+(?:vip|prioritaria|prioritario)|(?:waitlist|waiting\s+list|interest(?:ed)?\s+list)\s+(?:vip|priority)))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunPriorityWaitlistDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:vip|priority|early\s+access|acceso\s+anticipado)\s+(?:waitlist|waiting\s+list|interest(?:ed)?\s+list|list|lista(?:\s+de\s+(?:espera|interesad[oa]s))?)|(?:lista\s+(?:vip|prioritaria|prioritario|de\s+prioridad)|lista\s+de\s+espera\s+(?:vip|prioritaria|prioritario)|(?:waitlist|waiting\s+list|interest(?:ed)?\s+list)\s+(?:vip|priority)))\s*$/i;

const firstRunInvitationDescriptorPattern = String.raw`(?:(?:invite[-\s]?only|invitation(?:\s+only)?|private\s+invite|by\s+invitation)(?:\s+(?:(?:course\s+)?(?:registration|enrollment|application|sign[-\s]?up)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?|forms?|pages?|links?|urls?|portals?|access))?|(?:forms?|pages?|links?|urls?|portals?)\s+(?:for\s+)?(?:invite[-\s]?only|invitation(?:\s+only)?|private\s+invite|by\s+invitation)(?:\s+(?:registration|enrollment|application|access))?|(?:formulario|p[aá]gina|registro|inscripci[oó]n|matr[ií]cula|acceso)\s+(?:de|para|por|con)\s+invitaci[oó]n|invitaci[oó]n\s+(?:al|a\s+la|del?|de|para)\s+(?:curso|clase|programa|cohorte|inscripci[oó]n|registro|matr[ií]cula))`;
const firstRunInvitationDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunInvitationDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunInvitationDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunInvitationDescriptorPattern})\s*$`,
  'i',
);

const firstRunLeadMagnetDescriptorPrefixPattern =
  /^(?:(?:lead\s+magnet|freebie|free\s+resource|opt[-\s]?in|squeeze|recurso\s+gratuito|im[aá]n\s+de\s+(?:leads?|prospectos?|interesad[oa]s))\s+(?:forms?|pages?|downloads?(?:\s+pages?)?|sign[-\s]?ups?|registrations?)|(?:formulario|p[aá]gina|registro|descarga)\s+de\s+(?:lead\s+magnet|freebie|free\s+resource|opt[-\s]?in|squeeze|recurso\s+gratuito|im[aá]n\s+de\s+(?:leads?|prospectos?|interesad[oa]s)))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunLeadMagnetDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:lead\s+magnet|freebie|free\s+resource|opt[-\s]?in|squeeze|recurso\s+gratuito|im[aá]n\s+de\s+(?:leads?|prospectos?|interesad[oa]s))\s+(?:forms?|pages?|downloads?(?:\s+pages?)?|sign[-\s]?ups?|registrations?)|(?:formulario|p[aá]gina|registro|descarga)\s+de\s+(?:lead\s+magnet|freebie|free\s+resource|opt[-\s]?in|squeeze|recurso\s+gratuito|im[aá]n\s+de\s+(?:leads?|prospectos?|interesad[oa]s)))\s*$/i;

const firstRunDownloadableResourceDescriptorPattern = String.raw`(?:(?:free\s+)?(?:guide|e-?book|checklist|template|worksheet|resource)\s+(?:(?:download|request|opt[-\s]?in)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?|forms?|pages?|links?|urls?|portals?)|(?:download|request|opt[-\s]?in)\s+(?:forms?|pages?|links?|urls?|portals?)?\s*(?:for\s+)?(?:free\s+)?(?:guide|e-?book|checklist|template|worksheet|resource)|(?:formulario|p[aá]gina|enlace|link|url|portal|descarga|solicitud)\s+de\s+(?:gu[ií]a|e-?book|libro\s+electr[oó]nico|checklist|lista\s+de\s+verificaci[oó]n|plantilla|worksheet|hoja\s+de\s+trabajo|recurso)(?:\s+gratuit[oa])?)`;
const firstRunDownloadableResourceDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunDownloadableResourceDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunDownloadableResourceDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunDownloadableResourceDescriptorPattern})\s*$`,
  'i',
);

const firstRunCourseInfoAssetDescriptorPattern = String.raw`(?:(?:course\s+)?(?:brochure|prospectus|syllabus|info(?:rmation)?\s+packet)\s+(?:(?:download|request)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?|forms?|pages?|links?|urls?|portals?)|(?:forms?|pages?|links?|urls?|portals?)\s+(?:for\s+)?(?:course\s+)?(?:brochure|prospectus|syllabus|info(?:rmation)?\s+packet)|(?:folleto|brochure|prospecto|temario|programa\s+informativo|paquete\s+informativo)\s+(?:del?\s+curso|de\s+curso)?(?:\s+(?:descarga|solicitud))?|(?:descarga|solicitud)\s+de\s+(?:folleto|brochure|prospecto|temario|programa\s+informativo|paquete\s+informativo)(?:\s+(?:del?\s+curso|de\s+curso))?)`;
const firstRunCourseInfoAssetDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunCourseInfoAssetDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunCourseInfoAssetDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunCourseInfoAssetDescriptorPattern})\s*$`,
  'i',
);

const firstRunCourseInfoPageDescriptorPattern = String.raw`(?:(?:course\s+)?(?:details?|information|info)\s+(?:pages?|links?|urls?|portals?)|(?:pages?|links?|urls?|portals?)\s+(?:for\s+)?(?:course\s+)?(?:details?|information|info)|(?:p[aá]gina|enlace|link|url|portal)\s+de\s+(?:informaci[oó]n|detalles?)\s+(?:del?\s+curso|de\s+curso)?|(?:informaci[oó]n|detalles?)\s+(?:del?\s+curso|de\s+curso)(?:\s+(?:p[aá]gina|enlace|link|url|portal))?)`;
const firstRunCourseInfoPageDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunCourseInfoPageDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunCourseInfoPageDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunCourseInfoPageDescriptorPattern})\s*$`,
  'i',
);

const firstRunCampaignDescriptorPrefixPattern =
  /^(?:(?:(?:facebook|fb|meta|instagram|ig|linked\s*in|linkedin|tik\s*tok|tiktok|google|youtube|whats\s*app|e-?mail|correo|newsletter|bolet[ií]n)\s+)?(?:ad\s+|ads?\s+|marketing\s+)?campaign(?:\s+(?:forms?|pages?|landing\s+pages?|links?|urls?|funnels?))?|campañas?\s+(?:de|para)\s+(?:inscripci[oó]n(?:es)?|registro|matr[ií]cula|leads?|prospectos?|interesad[oa]s|captaci[oó]n|publicidad|anuncios?|ads?|e-?mail|correo|newsletter|bolet[ií]n)|(?:formulario|p[aá]gina|landing|enlaces?|links?|urls?)\s+de\s+campaña(?:\s+(?:de|para)\s+(?:inscripci[oó]n(?:es)?|registro|matr[ií]cula|leads?|prospectos?|interesad[oa]s|captaci[oó]n|publicidad|anuncios?|ads?|e-?mail|correo|newsletter|bolet[ií]n))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunCampaignDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:facebook|fb|meta|instagram|ig|linked\s*in|linkedin|tik\s*tok|tiktok|google|youtube|whats\s*app|e-?mail|correo|newsletter|bolet[ií]n)\s+)?(?:ad\s+|ads?\s+|marketing\s+)?campaign(?:\s+(?:forms?|pages?|landing\s+pages?|links?|urls?|funnels?))?|campañas?\s+(?:de|para)\s+(?:inscripci[oó]n(?:es)?|registro|matr[ií]cula|leads?|prospectos?|interesad[oa]s|captaci[oó]n|publicidad|anuncios?|ads?|e-?mail|correo|newsletter|bolet[ií]n)|(?:formulario|p[aá]gina|landing|enlaces?|links?|urls?)\s+de\s+campaña(?:\s+(?:de|para)\s+(?:inscripci[oó]n(?:es)?|registro|matr[ií]cula|leads?|prospectos?|interesad[oa]s|captaci[oó]n|publicidad|anuncios?|ads?|e-?mail|correo|newsletter|bolet[ií]n))?)\s*$/i;

const firstRunLaunchDescriptorPattern = String.raw`(?:course\s+launch(?:\s+(?:campaigns?|pages?|forms?|funnels?|links?|urls?|portals?))?|launch\s+(?:campaigns?|pages?|forms?|funnels?|links?|urls?|portals?)|(?:p[aá]gina|campaña|formulario|enlace|link|url|portal|embudo)\s+de\s+lanzamiento(?:\s+(?:del?\s+curso|de\s+curso))?|lanzamiento\s+(?:del?\s+curso|de\s+curso))`;
const firstRunLaunchDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunLaunchDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunLaunchDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunLaunchDescriptorPattern})\s*$`,
  'i',
);

const firstRunAdAssetDescriptorPattern = String.raw`(?:(?:(?:facebook|fb|meta|instagram|ig|linked\s*in|linkedin|tik\s*tok|tiktok|google|youtube|whats\s*app)\s+)?(?:ad\s*sets?|adsets?|ads?\s+groups?|ad\s+creatives?|ads?)|(?:anuncios?|conjuntos?\s+de\s+anuncios?)(?:\s+de\s+(?:facebook|fb|meta|instagram|ig|linked\s*in|linkedin|tik\s*tok|tiktok|google|youtube|whats\s*app))?)`;
const firstRunAdAssetDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunAdAssetDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for)\s+|\s*[-:/|]\s*)`,
  'i',
);
const firstRunAdAssetDescriptorSuffixPattern = new RegExp(
  String.raw`(?:\s*[-:/|]\s*|\s+(?:del|de|para\s+el|para|for)\s+)(?:${firstRunAdAssetDescriptorPattern})\s*$`,
  'i',
);

const firstRunProviderFormDescriptorPrefixPattern =
  /^(?:(?:google|(?:microsoft|ms))\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?)|(?:formularios?\s+(?:de\s+)?google)|(?:(?:facebook|fb|meta|instagram|ig)\s+lead\s+ads?(?:\s+(?:forms?|pages?|portals?|links?|urls?))?)|(?:(?:facebook|fb|meta|instagram|ig)\s+leads\b)|(?:leads?\b\s+de\s+(?:facebook|fb|meta|instagram|ig))|(?:(?:facebook|fb|meta|instagram|ig)\s+(?:(?:lead|instant|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?))|(?:whats\s*app\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?))|(?:formulario\s+(?:de\s+)?whats\s*app)|(?:formularios?\s+(?:de\s+)?(?:typeform(?:\s*\.?\s*com)?|many\s*chat|manychat|tally(?:\s+forms?|\s*\.?\s*so)?|jot\s*forms?|airtable|coda|hubspot|mail\s*chimp|paper\s*forms?|survey\s*monkey|wufoo|formstack|zoho|gravity\s+forms?|web\s*flow|wix|squarespace|lead\s*pages?|notion|fillout|cognito\s+forms?|forms?\.app|form\s*spree|formsite|123\s*forms?\s*builder|123formbuilder)(?:\s+forms?)?)|typeform(?:\s*\.?\s*com)?|many\s*chat\s+(?:lead\s+)?forms?|manychat\s+(?:lead\s+)?forms?|tally(?:\s+forms?|\s*\.?\s*so)?|jot\s*forms?|airtable(?:\s+forms?)?|coda(?:\s+forms?)?|hubspot\s+forms?|mail\s*chimp\s+(?:sign[-\s]?up\s+)?forms?|paper\s*forms?|survey\s*monkey(?:\s+forms?)?|wufoo(?:\s+forms?)?|formstack(?:\s+forms?)?|zoho\s+forms?|gravity\s+forms?|web\s*flow\s+forms?|wix\s+forms?|squarespace\s+forms?|lead\s*pages?(?:\s+(?:landing\s+)?pages?|forms?|portals?)?|notion\s+forms?|fillout(?:\s+forms?)?|cognito\s+forms?|forms?\.app(?:\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?))?|form\s*spree(?:\s+forms?)?|formsite(?:\s+forms?)?|123\s*forms?\s*builder(?:\s+forms?)?|123formbuilder(?:\s+forms?)?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunProviderFormDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:google|(?:microsoft|ms))\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?)|(?:formularios?\s+(?:de\s+)?google)|(?:(?:facebook|fb|meta|instagram|ig)\s+lead\s+ads?(?:\s+(?:forms?|pages?|portals?|links?|urls?))?)|(?:(?:facebook|fb|meta|instagram|ig)\s+leads\b)|(?:leads?\b\s+de\s+(?:facebook|fb|meta|instagram|ig))|(?:(?:facebook|fb|meta|instagram|ig)\s+(?:(?:lead|instant|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?))|(?:whats\s*app\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?))|(?:formulario\s+(?:de\s+)?whats\s*app)|(?:formularios?\s+(?:de\s+)?(?:typeform(?:\s*\.?\s*com)?|many\s*chat|manychat|tally(?:\s+forms?|\s*\.?\s*so)?|jot\s*forms?|airtable|coda|hubspot|mail\s*chimp|paper\s*forms?|survey\s*monkey|wufoo|formstack|zoho|gravity\s+forms?|web\s*flow|wix|squarespace|lead\s*pages?|notion|fillout|cognito\s+forms?|forms?\.app|form\s*spree|formsite|123\s*forms?\s*builder|123formbuilder)(?:\s+forms?)?)|typeform(?:\s*\.?\s*com)?|many\s*chat\s+(?:lead\s+)?forms?|manychat\s+(?:lead\s+)?forms?|tally(?:\s+forms?|\s*\.?\s*so)?|jot\s*forms?|airtable(?:\s+forms?)?|coda(?:\s+forms?)?|hubspot\s+forms?|mail\s*chimp\s+(?:sign[-\s]?up\s+)?forms?|paper\s*forms?|survey\s*monkey(?:\s+forms?)?|wufoo(?:\s+forms?)?|formstack(?:\s+forms?)?|zoho\s+forms?|gravity\s+forms?|web\s*flow\s+forms?|wix\s+forms?|squarespace\s+forms?|lead\s*pages?(?:\s+(?:landing\s+)?pages?|forms?|portals?)?|notion\s+forms?|fillout(?:\s+forms?)?|cognito\s+forms?|forms?\.app(?:\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?))?|form\s*spree(?:\s+forms?)?|formsite(?:\s+forms?)?|123\s*forms?\s*builder(?:\s+forms?)?|123formbuilder(?:\s+forms?)?)\s*$/i;

const firstRunStaticFormBackendProviderPattern =
  String.raw`(?:netlify(?:\s+forms?)?|form\s*keep|formkeep|form\s*bold|formbold|form\s*spark|formspark|form\s*submit|formsubmit|get\s*form|getform|basin|form\s*carry|formcarry)`;
const firstRunStaticFormBackendDescriptorPattern =
  String.raw`(?:(?:${firstRunStaticFormBackendProviderPattern})(?:\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?))?|(?:formularios?|p[aá]ginas?|portales?|enlaces?|links?|urls?)\s+(?:de\s+)?(?:${firstRunStaticFormBackendProviderPattern}))`;
const firstRunStaticFormBackendDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunStaticFormBackendDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunStaticFormBackendDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunStaticFormBackendDescriptorPattern})\s*$`,
  'i',
);

const firstRunEmergingFormProviderPattern =
  String.raw`(?:hey\s*flow|heyflow|outgrow|interact|land\s*bot|landbot|perspective(?:\s*\.?\s*co)?|feathery|qualtrics|question\s*pro|questionpro|survey\s*sparrow|surveysparrow)`;
const firstRunEmergingFormProviderDescriptorPattern =
  String.raw`(?:(?:${firstRunEmergingFormProviderPattern})\s+(?:(?:lead|quiz|survey|questionnaire|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|portals?|links?|urls?|funnels?|flows?|quiz(?:zes)?|surveys?|questionnaires?)|(?:formularios?|p[aá]ginas?|portales?|enlaces?|links?|urls?|embudos?|flujos?|cuestionarios?|encuestas?)\s+(?:de\s+)?(?:${firstRunEmergingFormProviderPattern}))`;
const firstRunEmergingFormProviderDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunEmergingFormProviderDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunEmergingFormProviderDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunEmergingFormProviderDescriptorPattern})\s*$`,
  'i',
);

const firstRunNoCodeLandingProviderPattern = String.raw`(?:carrd|framer|unbounce|instapage|landingi|web\s*flow|wix|squarespace|notion|google\s+sites?)`;
const firstRunNoCodeLandingDescriptorPattern = String.raw`(?:(?:${firstRunNoCodeLandingProviderPattern})\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|admissions?|waitlist|interest|contact|inquiry|enquiry|booking|reservation)\s+)?(?:forms?|pages?|landing\s+pages?|links?|urls?|portals?)|(?:forms?|pages?|landing\s+pages?|links?|urls?|portals?)\s+(?:for\s+)?(?:${firstRunNoCodeLandingProviderPattern})|(?:formularios?|p[aá]ginas?|landings?|enlaces?|links?|urls?|portales?)\s+(?:de\s+)?(?:${firstRunNoCodeLandingProviderPattern}))`;
const firstRunNoCodeLandingDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunNoCodeLandingDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunNoCodeLandingDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunNoCodeLandingDescriptorPattern})\s*$`,
  'i',
);

const firstRunEmailMarketingFormDescriptorPrefixPattern =
  /^(?:(?:convert\s*kit|kit|brevo|sendinblue|flodesk|mailer\s*lite|mailerlite|klaviyo|active\s*campaign|constant\s+contact|keap|infusionsoft|substack)\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|interest|contact|inquiry|enquiry)\s+)?(?:forms?|pages?|portals?)|(?:formularios?|p[aá]ginas?|portales?)\s+(?:de\s+)?(?:convert\s*kit|kit|brevo|sendinblue|flodesk|mailer\s*lite|mailerlite|klaviyo|active\s*campaign|constant\s+contact|keap|infusionsoft|substack))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunEmailMarketingFormDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:convert\s*kit|kit|brevo|sendinblue|flodesk|mailer\s*lite|mailerlite|klaviyo|active\s*campaign|constant\s+contact|keap|infusionsoft|substack)\s+(?:(?:lead|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake|interest|contact|inquiry|enquiry)\s+)?(?:forms?|pages?|portals?)|(?:formularios?|p[aá]ginas?|portales?)\s+(?:de\s+)?(?:convert\s*kit|kit|brevo|sendinblue|flodesk|mailer\s*lite|mailerlite|klaviyo|active\s*campaign|constant\s+contact|keap|infusionsoft|substack))\s*$/i;

const firstRunCoursePlatformPattern = String.raw`(?:kajabi|teachable|thinkific|moodle|hotmart|podia|learn\s*worlds?|click\s*funnels?|clickfunnels|kartra|systeme\s*\.?\s*io|go\s*high\s*level|gohighlevel|highlevel|lead\s*connector|mighty\s*networks?|skool|circle\s*\.?\s*so|udemy|coursera|skill\s*share|ed\s*x)`;
const firstRunCoursePlatformDescriptorPattern = String.raw`(?:(?:course\s+)?(?:checkout|payment|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake)(?:\s+(?:forms?|pages?|portals?|links?|urls?|checkouts?|funnels?))?|(?:course\s+)?(?:forms?|pages?|portals?|links?|urls?|checkouts?|funnels?))`;
const firstRunCoursePlatformDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunCoursePlatformPattern}(?:\s+${firstRunCoursePlatformDescriptorPattern})?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunCoursePlatformDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunCoursePlatformPattern}(?:\s+${firstRunCoursePlatformDescriptorPattern})?)\s*$`,
  'i',
);

const firstRunCrmFormDescriptorPrefixPattern =
  /^(?:(?:crm|customer\s+relationship\s+management)\s+(?:(?:lead|prospects?|contact|inquiry|enquiry|intake|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up)\s+)?(?:forms?|pages?|portals?)|(?:formularios?|p[aá]ginas?|portales?)\s+crm\s+(?:de\s+)?(?:leads?|prospectos?|contactos?|consultas?|(?:pre)?inscripci[oó]n(?:es)?|registro))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunCrmFormDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:crm|customer\s+relationship\s+management)\s+(?:(?:lead|prospects?|contact|inquiry|enquiry|intake|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up)\s+)?(?:forms?|pages?|portals?)|(?:formularios?|p[aá]ginas?|portales?)\s+crm\s+(?:de\s+)?(?:leads?|prospectos?|contactos?|consultas?|(?:pre)?inscripci[oó]n(?:es)?|registro))\s*$/i;

const firstRunCrmWorkflowDescriptorPrefixPattern =
  /^(?:(?:crm|customer\s+relationship\s+management)\s+(?:pipelines?|funnels?|workflows?|boards?|dashboards?|queues?)|(?:pipeline|funnel|workflow|board|dashboard|queue)\s+crm)(?:\s+(?:del|de|para\s+el|para|for)\s+|\s*[-:/|]\s*)/i;

const firstRunCrmWorkflowDescriptorSuffixPattern =
  /(?:\s+(?:del|de|para\s+el|para|for)\s+|\s*[-:/|]\s*)(?:(?:crm|customer\s+relationship\s+management)\s+(?:pipelines?|funnels?|workflows?|boards?|dashboards?|queues?)|(?:pipeline|funnel|workflow|board|dashboard|queue)\s+crm)\s*$/i;

const firstRunCrmProviderPattern = String.raw`(?:wati|kommo|pipe\s*drive)`;
const firstRunCrmProviderFormDescriptorPattern = String.raw`(?:(?:${firstRunCrmProviderPattern})\s+(?:(?:lead|prospects?|contact|inquiry|enquiry|intake|(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up)\s+)?(?:forms?|pages?|portals?|links?|urls?)|(?:formularios?|p[aá]ginas?|portales?|enlaces?|links?|urls?)\s+(?:de\s+)?(?:${firstRunCrmProviderPattern}))`;
const firstRunCrmProviderFormDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunCrmProviderFormDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunCrmProviderFormDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunCrmProviderFormDescriptorPattern})\s*$`,
  'i',
);

const firstRunEmergingSocialLeadDescriptorPrefixPattern =
  /^(?:(?:linked\s*in|linkedin|tik\s*tok|tiktok)\s+(?:(?:lead(?:\s+gen|\s+ads?)?|instant)\s+)?(?:forms?|pages?|portals?)|(?:(?:linked\s*in|linkedin|tik\s*tok|tiktok)\s+leads?\b)|(?:leads?\b\s+de\s+(?:linked\s*in|linkedin|tik\s*tok|tiktok)))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunEmergingSocialLeadDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:linked\s*in|linkedin|tik\s*tok|tiktok)\s+(?:(?:lead(?:\s+gen|\s+ads?)?|instant)\s+)?(?:forms?|pages?|portals?)|(?:(?:linked\s*in|linkedin|tik\s*tok|tiktok)\s+leads?\b)|(?:leads?\b\s+de\s+(?:linked\s*in|linkedin|tik\s*tok|tiktok)))\s*$/i;

const firstRunMessagingAutomationProviderPattern = String.raw`(?:many\s*chat|manychat|instagram|ig|facebook|fb|meta|messenger|whats\s*app)`;
const firstRunMessagingAutomationChannelPattern = String.raw`(?:dm|direct\s+messages?|messages?|mensaje\s+directo|inbox|chat|messenger)`;
const firstRunMessagingAutomationWorkflowPattern = String.raw`(?:automation|automations|automated\s+reply|bots?|flows?|funnels?|intake|leads?|registration|enrollment|sign[-\s]?up|click[-\s]?to[-\s]?chat|keywords?|keyword\s+triggers?|triggers?|registro|inscripci[oó]n|automatizaci[oó]n|automatizaciones|flujos?|embudos?|palabras?\s+clave|disparadores?|respuestas?(?:\s+autom[aá]ticas?)?)`;
const firstRunMessagingAutomationDescriptorPattern = String.raw`(?:(?:(?:${firstRunMessagingAutomationProviderPattern}\s+)?${firstRunMessagingAutomationChannelPattern}\s+${firstRunMessagingAutomationWorkflowPattern}|${firstRunMessagingAutomationProviderPattern}\s+${firstRunMessagingAutomationWorkflowPattern})(?:\s+(?:forms?|pages?|links?|urls?|flows?|funnels?))?|${firstRunMessagingAutomationWorkflowPattern}\s+(?:de|para)\s+(?:${firstRunMessagingAutomationChannelPattern}|${firstRunMessagingAutomationProviderPattern}))`;
const firstRunMessagingAutomationDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunMessagingAutomationDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for)|\s*[-:/|]\s*)`,
  'i',
);
const firstRunMessagingAutomationDescriptorSuffixPattern = new RegExp(
  String.raw`(?:\s*[-:/|]\s*|\s+)(?:${firstRunMessagingAutomationDescriptorPattern})\s*$`,
  'i',
);

const firstRunAutomationPlumbingProviderPattern = String.raw`(?:zapier|make\s*\.?\s*com|integromat|n8n|pabbly(?:\s+connect)?|integrately)`;
const firstRunAutomationPlumbingWorkflowPattern = String.raw`(?:webhooks?|automation|automations|workflows?|flows?|scenarios?|zaps?|connections?|integrations?|registro|inscripci[oó]n|automatizaci[oó]n|automatizaciones|flujos?|escenarios?|integraciones?)`;
const firstRunAutomationPlumbingDescriptorPattern = String.raw`(?:(?:${firstRunAutomationPlumbingProviderPattern})\s+(?:(?:course\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|intake)\s+)?(?:${firstRunAutomationPlumbingWorkflowPattern})(?:\s+(?:forms?|pages?|links?|urls?|portals?))?|(?:${firstRunAutomationPlumbingWorkflowPattern})\s+(?:de|para|for)\s+(?:${firstRunAutomationPlumbingProviderPattern}))`;
const firstRunAutomationPlumbingDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunAutomationPlumbingDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for)|\s*[-:/|]\s*)`,
  'i',
);
const firstRunAutomationPlumbingDescriptorSuffixPattern = new RegExp(
  String.raw`(?:\s*[-:/|]\s*|\s+)(?:${firstRunAutomationPlumbingDescriptorPattern})\s*$`,
  'i',
);

const firstRunDataSourceDescriptorPattern = String.raw`(?:(?:airtable|notion|coda|google\s*sheets?|google\s*sheet|(?:microsoft\s+)?excel)\s+(?:(?:course\s+)?(?:registration|enrollment|lead|intake|student|sign[-\s]?up)\s+)?(?:tables?|views?|databases?|spreadsheets?|sheets?|workbooks?|rows?|bases?|lists?)|(?:tables?|views?|databases?|spreadsheets?|sheets?|workbooks?|rows?|bases?|lists?)\s+(?:for\s+)?(?:airtable|notion|coda|google\s*sheets?|google\s*sheet|(?:microsoft\s+)?excel)|(?:tabla|vista|base\s+de\s+datos|hoja\s+de\s+c[aá]lculo|lista)\s+(?:de|para)\s+(?:airtable|notion|coda|google|(?:microsoft\s+)?excel)|hoja\s+de\s+c[aá]lculo\s+de\s+google|hojas?\s+de\s+(?:microsoft\s+)?excel)`;
const firstRunDataSourceDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunDataSourceDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for)|\s*[-:/|]\s*)`,
  'i',
);
const firstRunDataSourceDescriptorSuffixPattern = new RegExp(
  String.raw`(?:\s*[-:/|]\s*|\s+)(?:${firstRunDataSourceDescriptorPattern})\s*$`,
  'i',
);

const firstRunCommunityPlatformPattern = String.raw`(?:whats\s*app|discord|slack|telegram|facebook|fb)`;
const firstRunCommunityChannelPattern = String.raw`(?:community|comunidad|group|grupo|chat|channel|canal|servers?|servidor(?:es)?|workspaces?|espacios?\s+de\s+trabajo|broadcast\s+(?:lists?|channels?)|announcement\s+channels?|listas?\s+de\s+difusi[oó]n|canal(?:es)?\s+de\s+(?:difusi[oó]n|anuncios|avisos|novedades)|(?:group|community|chat|channel|servers?)\s+(?:invites?|invite\s+links?|invitation\s+links?)|(?:invites?|invite\s+links?|invitation\s+links?)|(?:invitaci[oó]n|enlace(?:\s+de\s+invitaci[oó]n)?|link|url)\s+(?:al|a\s+la|para\s+el|para\s+la|del?|de)\s+(?:grupo|comunidad|chat|canal|servidor(?:es)?))`;
const firstRunCommunityGroupDescriptorPattern = String.raw`(?:(?:(?:${firstRunCommunityPlatformPattern})\s+(?:${firstRunCommunityChannelPattern}))|(?:(?:${firstRunCommunityChannelPattern})\s+(?:de\s+)?(?:${firstRunCommunityPlatformPattern}))|(?:(?:course|class|students?|members?)\s+(?:group\s+chat|chat\s+group|community|group|chat|channel|broadcast\s+(?:lists?|channels?)|announcement\s+channels?))|(?:(?:group\s+chat|chat\s+group|community|group|chat|channel|broadcast\s+(?:lists?|channels?)|announcement\s+channels?)\s+(?:for\s+)?(?:the\s+)?(?:course|class|students?|members?))|(?:(?:comunidad|grupo|chat|canal|listas?\s+de\s+difusi[oó]n|canal(?:es)?\s+de\s+(?:difusi[oó]n|anuncios|avisos|novedades))\s+(?:de(?:l)?\s+curso|para\s+(?:el\s+)?curso|de\s+(?:estudiantes|alumnos|miembros)|para\s+(?:estudiantes|alumnos|miembros))))`;

const firstRunCommunityGroupDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunCommunityGroupDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for)|\s*[-:/|]\s*)`,
  'i',
);

const firstRunCommunityGroupDescriptorSuffixPattern = new RegExp(
  String.raw`(?:\s*[-:/|]\s*|\s+)(?:${firstRunCommunityGroupDescriptorPattern})\s*$`,
  'i',
);

const firstRunEventPlatformDescriptorPrefixPattern =
  /^(?:(?:event\s*brite|eventbrite|lu\s*\.?\s*ma|luma|meetup|ticket\s*tailor|tickettailor|humanitix|eventzilla|sympla|entradium)(?:\s+(?:(?:event\s+)?(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|booking|reservation)(?:\s+(?:form|page|portal))?(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?|\s+events?(?:\s+(?:page|portal))?(?:\s+(?:del|de|para\s+el|para|for)\s*|\s*[-:/|]\s*)|\s+(?:form|page|portal)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?|\s+(?:del|de|para\s+el|para|for)\s*|\s*[-:/|]\s*)|(?:formulario|p[aá]gina|portal)\s+de\s+(?:event\s*brite|eventbrite|lu\s*\.?\s*ma|luma|meetup|ticket\s*tailor|tickettailor|humanitix|eventzilla|sympla|entradium)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?)/i;

const firstRunEventPlatformDescriptorSuffixPattern =
  /(?:\s*[-:/|]\s*(?:event\s*brite|eventbrite|lu\s*\.?\s*ma|luma|meetup|ticket\s*tailor|tickettailor|humanitix|eventzilla|sympla|entradium)(?:\s+(?:(?:event\s+)?(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|booking|reservation)(?:\s+(?:form|page|portal))?|\s+events?(?:\s+(?:page|portal))?|\s+(?:form|page|portal))?|\s+(?:event\s*brite|eventbrite|lu\s*\.?\s*ma|luma|meetup|ticket\s*tailor|tickettailor|humanitix|eventzilla|sympla|entradium)\s+(?:(?:event\s+)?(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|booking|reservation)(?:\s+(?:form|page|portal))?|\s+(?:event\s*brite|eventbrite|lu\s*\.?\s*ma|luma|meetup|ticket\s*tailor|tickettailor|humanitix|eventzilla|sympla|entradium)\s+events?(?:\s+(?:page|portal))?|\s*(?:[-:/|]\s*)?(?:formulario|p[aá]gina|portal)\s+de\s+(?:event\s*brite|eventbrite|lu\s*\.?\s*ma|luma|meetup|ticket\s*tailor|tickettailor|humanitix|eventzilla|sympla|entradium))\s*$/i;

const firstRunGenericEventDescriptorPrefixPattern =
  /^(?:(?:event|evento)\s+(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|signup|booking|reservation|rsvp)(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|(?:formulario|p[aá]gina|portal|enlace|link|url|registro|inscripci[oó]n|reserva|rsvp)\s+(?:de|para(?:\s+el)?)\s+eventos?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunGenericEventDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:event|evento)\s+(?:(?:pre[-\s]?)?registration|enrollment|application|sign[-\s]?up|signup|booking|reservation|rsvp)(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|(?:formulario|p[aá]gina|portal|enlace|link|url|registro|inscripci[oó]n|reserva|rsvp)\s+(?:de|para(?:\s+el)?)\s+eventos?)\s*$/i;

const firstRunInquiryDescriptorPrefixPattern =
  /^(?:(?:course\s+)?(?:contact|inquiry|enquiry|interest|lead|prospects?)\s+(?:form|page)|(?:formulario|p[aá]gina)\s+de\s+(?:contacto|consulta|inter[eé]s|prospectos?)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunInquiryDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:course\s+)?(?:contact|inquiry|enquiry|interest|lead|prospects?)\s+(?:form|page)|(?:formulario|p[aá]gina)\s+de\s+(?:contacto|consulta|inter[eé]s|prospectos?)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)\s*$/i;

const firstRunSurveyDescriptorPrefixPattern =
  /^(?:(?:course\s+)?(?:survey|questionnaire)\s+(?:forms?|pages?|portals?|requests?)|(?:(?:pre[-\s]?)?registration|enrollment|application|intake)\s+(?:surveys?|questionnaires?)|(?:formulario|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:encuesta|cuestionario)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?|(?:encuesta|cuestionario)\s+de\s+(?:pre)?inscripci[oó]n(?:es)?|(?:encuesta|cuestionario)\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunSurveyDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:course\s+)?(?:survey|questionnaire)\s+(?:forms?|pages?|portals?|requests?)|(?:(?:pre[-\s]?)?registration|enrollment|application|intake)\s+(?:surveys?|questionnaires?)|(?:formulario|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:encuesta|cuestionario)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?|(?:encuesta|cuestionario)\s+de\s+(?:pre)?inscripci[oó]n(?:es)?|(?:encuesta|cuestionario)\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))\s*$/i;

const firstRunInfoSessionDescriptorPrefixPattern =
  /^(?:(?:course\s+)?(?:info(?:rmation)?\s+session|orientation|open\s+house)\s+(?:form|page|signup|sign[-\s]?up|registration)|(?:formulario|p[aá]gina|registro|inscripci[oó]n(?:es)?)\s+de\s+(?:sesi[oó]n\s+informativa|orientaci[oó]n|clase\s+abierta)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunInfoSessionDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:course\s+)?(?:info(?:rmation)?\s+session|orientation|open\s+house)\s+(?:form|page|signup|sign[-\s]?up|registration)|(?:formulario|p[aá]gina|registro|inscripci[oó]n(?:es)?)\s+de\s+(?:sesi[oó]n\s+informativa|orientaci[oó]n|clase\s+abierta)(?:\s+(?:del?\s+curso|de\s+curso|para\s+el\s+curso))?)\s*$/i;

const firstRunLiveSessionDescriptorPrefixPattern =
  /^(?:(?:(?:zoom|google\s+meet|microsoft\s+teams|teams)\s+)?(?:webinar|seminar|meeting|live\s+session|sesi[oó]n\s+en\s+vivo)\s+(?:(?:registration|enrollment|sign[-\s]?up|waitlist|booking|reservation|rsvp)(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|forms?|pages?|portals?|links?|urls?)|(?:zoom|google\s+meet|microsoft\s+teams|teams)\s+(?:(?:registration|enrollment|sign[-\s]?up|waitlist|booking|reservation|rsvp)(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|forms?|pages?|portals?|links?|urls?)|(?:formulario|p[aá]gina|portal|enlace|link|url)\s+de\s+(?:(?:registro|inscripci[oó]n|reserva|rsvp|lista\s+de\s+espera)\s+(?:de|para)\s+)?(?:webinar|seminario|seminar|meeting|sesi[oó]n\s+en\s+vivo))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunLiveSessionDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:zoom|google\s+meet|microsoft\s+teams|teams)\s+)?(?:webinar|seminar|meeting|live\s+session|sesi[oó]n\s+en\s+vivo)\s+(?:(?:registration|enrollment|sign[-\s]?up|waitlist|booking|reservation|rsvp)(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|forms?|pages?|portals?|links?|urls?)|(?:zoom|google\s+meet|microsoft\s+teams|teams)\s+(?:(?:registration|enrollment|sign[-\s]?up|waitlist|booking|reservation|rsvp)(?:\s+(?:forms?|pages?|portals?|links?|urls?))?|forms?|pages?|portals?|links?|urls?)|(?:formulario|p[aá]gina|portal|enlace|link|url)\s+de\s+(?:(?:registro|inscripci[oó]n|reserva|rsvp|lista\s+de\s+espera)\s+(?:de|para)\s+)?(?:webinar|seminario|seminar|meeting|sesi[oó]n\s+en\s+vivo))\s*$/i;

const firstRunPublicRegistrationDescriptorPrefixPattern =
  /^(?:(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:pre)?inscripci[oó]n\s+p[uú]blica)|(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+p[uú]blic[oa]s?\s+(?:de|para(?:\s+la)?)\s+(?:pre)?inscripci[oó]n(?:es)?)|(?:(?:pre)?inscripci[oó]n(?:es)?\s+p[uú]blicas?)|(?:public\s+(?:course\s+)?(?:registration|enrollment)\s+(?:form|page|portal)))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunPublicRegistrationDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:pre)?inscripci[oó]n\s+p[uú]blica)|(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+p[uú]blic[oa]s?\s+(?:de|para(?:\s+la)?)\s+(?:pre)?inscripci[oó]n(?:es)?)|(?:(?:pre)?inscripci[oó]n(?:es)?\s+p[uú]blicas?)|(?:public\s+(?:course\s+)?(?:registration|enrollment)\s+(?:form|page|portal)))\s*$/i;

const firstRunSpanishPortalDescriptorPrefixPattern =
  /^(?:portal(?:es)?\s+(?:p[uú]blic[oa]s?\s+)?(?:de|para(?:\s+la)?)\s+(?:pre)?inscripci[oó]n(?:es)?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|portal(?:es)?\s+(?:del?\s+curso|de\s+curso))(?:\s+(?:del|de|para\s+el|para))?\s*(?:[-:/|]\s*)?/i;

const firstRunSpanishPortalDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:portal(?:es)?\s+(?:p[uú]blic[oa]s?\s+)?(?:de|para(?:\s+la)?)\s+(?:pre)?inscripci[oó]n(?:es)?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|portal(?:es)?\s+(?:del?\s+curso|de\s+curso))\s*$/i;

const firstRunOnlineRegistrationDescriptorPrefixPattern =
  /^(?:(?:online\s+(?:course\s+)?(?:registration|enrollment|application|sign[-\s]?up)(?:\s+(?:form|page|portal))?)|(?:(?:pre)?inscripci[oó]n|registro|matr[ií]cula)\s+(?:en\s+l[ií]nea|online)(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunOnlineRegistrationDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:online\s+(?:course\s+)?(?:registration|enrollment|application|sign[-\s]?up)(?:\s+(?:form|page|portal))?)|(?:(?:pre)?inscripci[oó]n|registro|matr[ií]cula)\s+(?:en\s+l[ií]nea|online)(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)\s*$/i;

const firstRunPostSubmitDescriptorPattern = String.raw`(?:(?:thank[-\s]?you|thanks|success)\s+(?:pages?|screens?|links?|urls?|portals?)|(?:registration|enrollment|sign[-\s]?up|signup)\s+confirmation(?:\s+(?:pages?|screens?|links?|urls?|portals?))?|confirmation\s+(?:pages?|screens?|links?|urls?|portals?)|(?:p[aá]ginas?|pantallas?|enlaces?|links?|urls?|portales?)\s+de\s+(?:gracias|[eé]xito|confirmaci[oó]n)|confirmaci[oó]n\s+de\s+(?:inscripci[oó]n|registro|matr[ií]cula)(?:\s+(?:p[aá]ginas?|pantallas?|enlaces?|links?|urls?|portales?))?)`;
const firstRunPostSubmitDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunPostSubmitDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunPostSubmitDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunPostSubmitDescriptorPattern})\s*$`,
  'i',
);

const firstRunLandingPageDescriptorPrefixPattern =
  /^(?:(?:p[aá]gina\s+landing|landing)(?:\s+(?:del?\s+curso|de\s+curso))?)\s*[-:/|]\s*/i;

const firstRunLandingPageDescriptorSuffixPattern =
  /\s*[-:/|]\s*(?:(?:p[aá]gina\s+landing|landing)(?:\s+(?:del?\s+curso|de\s+curso))?)\s*$/i;

const firstRunStandalonePublicPageDescriptorPrefixPattern =
  /^(?:(?:p[aá]gina|portal|formulario|ficha)\s+p[uú]blic[oa]s?|public\s+(?:page|form|portal))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunStandalonePublicPageDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:p[aá]gina|portal|formulario|ficha)\s+p[uú]blic[oa]s?|public\s+(?:page|form|portal))\s*$/i;

const firstRunBioLinkDescriptorPrefixPattern =
  /^(?:(?:link\s*tree|linktree|beacons(?:\s*\.?\s*ai)?|stan\s*store|koji(?:\s*\.?\s*to)?\s+(?:pages?|links?|urls?|portals?|forms?)|milkshake(?:\s*\.?\s*app)?|tap\s*link|solo\s*\.?\s*to|bio\s*\.?\s*site|bento\s*\.?\s*me|campsite\s*\.?\s*bio|bio\s*\.?\s*link|bio\s+links?|bio\s+pages?|profile\s+links?|profile\s+pages?|link\s+in\s+bio|enlace\s+en\s+bio|link\s+en\s+bio|enlace\s+de\s+perfil|link\s+de\s+perfil|p[aá]gina\s+de\s+bio)(?:\s+(?:pages?|links?|urls?|portals?|forms?))?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunBioLinkDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:link\s*tree|linktree|beacons(?:\s*\.?\s*ai)?|stan\s*store|koji(?:\s*\.?\s*to)?\s+(?:pages?|links?|urls?|portals?|forms?)|milkshake(?:\s*\.?\s*app)?|tap\s*link|solo\s*\.?\s*to|bio\s*\.?\s*site|bento\s*\.?\s*me|campsite\s*\.?\s*bio|bio\s*\.?\s*link|bio\s+links?|bio\s+pages?|profile\s+links?|profile\s+pages?|link\s+in\s+bio|enlace\s+en\s+bio|link\s+en\s+bio|enlace\s+de\s+perfil|link\s+de\s+perfil|p[aá]gina\s+de\s+bio)(?:\s+(?:pages?|links?|urls?|portals?|forms?))?)\s*$/i;

const firstRunCourseWebsiteDescriptorPrefixPattern =
  /^(?:(?:(?:course\s+)?(?:website|web\s+page|site|micro\s*site)|course\s+portal|web\s+portal|micrositio(?:\s+(?:del?\s+curso|de\s+curso))?)(?:\s+(?:del|de|para\s+el|para|for)\s+|\s*[-:/|]\s*)|(?:p[aá]gina|sitio|portal)\s+web\s+(?:del?\s+curso|de\s+curso)(?:\s*[-:/|]\s*)?|(?:p[aá]gina|sitio|portal)\s+web(?:\s+(?:del|de|para\s+el|para)\s+|\s*[-:/|]\s*))/i;

const firstRunCourseWebsiteDescriptorSuffixPattern =
  /(?:\s*(?:[-:/|]\s*)?(?:(?:course\s+)?(?:website|web\s+page|site|micro\s*site)|course\s+portal|web\s+portal|micrositio(?:\s+(?:del?\s+curso|de\s+curso))?|(?:p[aá]gina|sitio|portal)\s+web(?:\s+(?:del?\s+curso|de\s+curso))?)|\s+(?:del|de|para\s+el|para|for)\s+(?:(?:course\s+)?(?:website|web\s+page|site|micro\s*site)|course\s+portal|web\s+portal|micrositio|(?:p[aá]gina|sitio|portal)\s+web))\s*$/i;

const firstRunLearningPortalDescriptorPrefixPattern =
  /^(?:(?:student|learner|member|learning|lms)\s+(?:portal|dashboard|hub|area|cent(?:er|re))|(?:learning\s+management\s+system|lms)\s+(?:(?:enrollment|registration|sign[-\s]?up|student|learner)\s+)?(?:portal|dashboard|hub|area|cent(?:er|re))|(?:portal|dashboard|hub|area|cent(?:er|re))\s+(?:for\s+)?(?:students?|learners?|members?)|(?:campus|aula|sal[oó]n)\s+virtual|digital\s+campus|campus\s+digital|(?:portal|centro)\s+(?:de\s+)?(?:estudiantes|alumnos|miembros|aprendizaje))(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunLearningPortalDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:student|learner|member|learning|lms)\s+(?:portal|dashboard|hub|area|cent(?:er|re))|(?:learning\s+management\s+system|lms)\s+(?:(?:enrollment|registration|sign[-\s]?up|student|learner)\s+)?(?:portal|dashboard|hub|area|cent(?:er|re))|(?:portal|dashboard|hub|area|cent(?:er|re))\s+(?:for\s+)?(?:students?|learners?|members?)|(?:campus|aula|sal[oó]n)\s+virtual|digital\s+campus|campus\s+digital|(?:portal|centro)\s+(?:de\s+)?(?:estudiantes|alumnos|miembros|aprendizaje))\s*$/i;

const firstRunCourseCatalogDescriptorPrefixPattern =
  /^(?:(?:course\s+)?(?:catalog|catalogue|listing|directory)\s+(?:pages?|links?|urls?|portals?)?|(?:cat[aá]logo|listado|directorio)\s+(?:de\s+)?cursos?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i;

const firstRunCourseCatalogDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:course\s+)?(?:catalog|catalogue|listing|directory)\s+(?:pages?|links?|urls?|portals?)?|(?:cat[aá]logo|listado|directorio)\s+(?:de\s+)?cursos?)\s*$/i;

const firstRunScheduleDescriptorPattern = String.raw`(?:(?:course\s+)?(?:schedule|calendar|timetable)\s+(?:pages?|links?|urls?|portals?)|(?:class|lesson)\s+(?:schedule|calendar|timetable)\s+(?:pages?|links?|urls?|portals?)|(?:horarios?|calendarios?|cronogramas?)\s+(?:del?\s+curso|de\s+curso|de\s+clases?)(?:\s+(?:p[aá]ginas?|enlaces?|links?|urls?|portales?))?|(?:p[aá]ginas?|enlaces?|links?|urls?|portales?)\s+(?:de|para)\s+(?:horarios?|calendarios?|cronogramas?)(?:\s+(?:del?\s+curso|de\s+curso|de\s+clases?))?)`;
const firstRunScheduleDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunScheduleDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for)\s+|\s*[-:/|]\s*)`,
  'i',
);
const firstRunScheduleDescriptorSuffixPattern = new RegExp(
  String.raw`(?:\s*[-:/|]\s*|\s+(?:del|de|para\s+el|para|for)\s+)(?:${firstRunScheduleDescriptorPattern})\s*$`,
  'i',
);

const firstRunSchedulingProviderPattern = String.raw`(?:calendly|acuity(?:\s+scheduling)?|cal\s*\.?\s*com|simply\s*book|simplybook|setmore|you\s*can\s*book\s*\.?\s*me|youcanbookme|tidy\s*cal|savvy\s*cal|savvycal|once\s*hub|oncehub|appointlet|book\s*like\s*a\s*boss|booklikeaboss|microsoft\s+bookings?|ms\s+bookings?|google\s+calendar)`;
const firstRunSchedulingProviderIntentPattern = String.raw`(?:(?:booking|reservation|rsvp|appointments?|scheduling)\s+)?`;
const firstRunSchedulingProviderLinkDescriptorPattern = String.raw`(?:(?:${firstRunSchedulingProviderPattern})\s+${firstRunSchedulingProviderIntentPattern}(?:links?|urls?|pages?|forms?|portals?|schedules?)|(?:links?|urls?|pages?|forms?|portals?|schedules?)\s+(?:for\s+)?(?:${firstRunSchedulingProviderPattern})|(?:enlace|link|url|portal|formulario|p[aá]gina)\s+(?:de|para)\s+(?:${firstRunSchedulingProviderPattern}))`;
const firstRunSchedulingProviderStandalonePrefixPattern = new RegExp(
  String.raw`^(?:${firstRunSchedulingProviderPattern})\s*(?:[-:/|]\s*)`,
  'i',
);
const firstRunSchedulingProviderLinkDescriptorPrefixPattern = new RegExp(
  String.raw`^(?:${firstRunSchedulingProviderLinkDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
  'i',
);
const firstRunSchedulingProviderLinkDescriptorSuffixPattern = new RegExp(
  String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunSchedulingProviderLinkDescriptorPattern})\s*$`,
  'i',
);
const firstRunReservationDescriptorPattern = String.raw`(?:booking|reservation|rsvp|appointments?(?:\s+schedules?)?|scheduling)(?:\s+(?:forms?|pages?|links?|urls?|portals?|schedules?))?`;

const firstRunReservationDescriptorPrefixPattern =
  new RegExp(
    String.raw`^(?:(?:${firstRunSchedulingProviderPattern}\s+)?(?:course\s+)?${firstRunReservationDescriptorPattern}|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:reserva(?:\s+de\s+cupos?)?|cupos?|rsvp)(?:\s+(?:de|en)\s+${firstRunSchedulingProviderPattern})?|reservas?\s+de\s+cupos?(?:\s+(?:de|en)\s+${firstRunSchedulingProviderPattern})?)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
    'i',
  );

const firstRunReservationDescriptorSuffixPattern =
  new RegExp(
    String.raw`\s*(?:[-:/|]\s*)?(?:(?:${firstRunSchedulingProviderPattern}\s+)?(?:course\s+)?${firstRunReservationDescriptorPattern}|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+(?:reserva(?:\s+de\s+cupos?)?|cupos?|rsvp)(?:\s+(?:de|en)\s+${firstRunSchedulingProviderPattern})?|reservas?\s+de\s+cupos?(?:\s+(?:de|en)\s+${firstRunSchedulingProviderPattern})?)\s*$`,
    'i',
  );

const firstRunConsultationCallDescriptorPattern = String.raw`(?:(?:discovery|consultation|intro(?:ductory)?|strategy)\s+calls?\s+(?:(?:booking|registration|sign[-\s]?up|requests?)(?:\s+(?:forms?|pages?|links?|urls?|portals?))?|forms?|pages?|links?|urls?|portals?)|(?:formulario|ficha|p[aá]gina|solicitud(?:es)?|enlace|link|url|portal)\s+de\s+llamada\s+de\s+(?:consulta|diagn[oó]stico))`;

const firstRunConsultationCallDescriptorPrefixPattern =
  new RegExp(
    String.raw`^(?:${firstRunConsultationCallDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
    'i',
  );

const firstRunConsultationCallDescriptorSuffixPattern =
  new RegExp(
    String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunConsultationCallDescriptorPattern})\s*$`,
    'i',
  );

const firstRunAdminWorkflowDescriptorPattern = String.raw`(?:(?:(?:admin|ops|operations|internal|back\s*office|backoffice)\s+)?(?:review|approval|moderation|triage)\s+(?:queues?|boards?|trackers?|dashboards?|inboxes?|workspaces?)|(?:admin|ops|operations|internal|back\s*office|backoffice)\s+(?:queues?|boards?|trackers?|dashboards?|inboxes?|workspaces?)|(?:colas?|bandejas?|tableros?|panel(?:es)?|seguimiento)\s+(?:de|para)\s+(?:revisi[oó]n|aprobaci[oó]n|moderaci[oó]n|triaje|admin|operaci[oó]n|operaciones|intern[oa]s?))`;

const firstRunAdminWorkflowDescriptorPrefixPattern =
  new RegExp(
    String.raw`^(?:${firstRunAdminWorkflowDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
    'i',
  );

const firstRunAdminWorkflowDescriptorSuffixPattern =
  new RegExp(
    String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunAdminWorkflowDescriptorPattern})\s*$`,
    'i',
  );

const firstRunRegistrationDashboardDescriptorPattern = String.raw`(?:(?:admin\s+)?(?:course\s+|student\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|admissions?|intake|waitlist)\s+(?:dashboards?|trackers?|boards?|workspaces?)|(?:admin\s+)?(?:dashboards?|trackers?|boards?|workspaces?)\s+(?:for\s+)?(?:course\s+|student\s+)?(?:(?:pre[-\s]?)?registration|enrollment|application|admissions?|intake|waitlist|students?)|(?:panel(?:es)?|tableros?|seguimiento)\s+(?:de|para)\s+(?:pre)?inscripci[oó]n(?:es)?|(?:panel(?:es)?|tableros?|seguimiento)\s+(?:de|para)\s+(?:matr[ií]cula|admisiones|solicitudes|estudiantes?|alumnos?))`;

const firstRunRegistrationDashboardDescriptorPrefixPattern =
  new RegExp(
    String.raw`^(?:${firstRunRegistrationDashboardDescriptorPattern})(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?`,
    'i',
  );

const firstRunRegistrationDashboardDescriptorSuffixPattern =
  new RegExp(
    String.raw`\s*(?:[-:/|]\s*)?(?:${firstRunRegistrationDashboardDescriptorPattern})\s*$`,
    'i',
  );

const firstRunCourseEnrollmentConnectorPrefixPattern =
  /^(?:(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+)?(?:pre)?inscripci[oó]n(?:es)?\s+(?:al|del?|de|para(?:\s+el)?)\s+curso)(?:\s*(?:[-:/|]\s*)?)/i;

const firstRunCourseEnrollmentConnectorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+)?(?:pre)?inscripci[oó]n(?:es)?\s+(?:al|del?|de|para(?:\s+el)?)\s+curso)\s*$/i;

const firstRunPreMatriculaDescriptorPrefixPattern =
  /^(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+pre[-\s]?matr[ií]cula|pre[-\s]?matr[ií]culas?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)(?:\s+(?:del|de|para\s+el|para))?\s*(?:[-:/|]\s*)?/i;

const firstRunPreMatriculaDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+pre[-\s]?matr[ií]cula|pre[-\s]?matr[ií]culas?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)\s*$/i;

const firstRunMatriculacionDescriptorPrefixPattern =
  /^(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+matriculaci[oó]n|matriculaci[oó]n(?:es)?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)(?:\s+(?:del|de|para\s+el|para))?\s*(?:[-:/|]\s*)?/i;

const firstRunMatriculacionDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+de\s+matriculaci[oó]n|matriculaci[oó]n(?:es)?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?)\s*$/i;

const firstRunSpanishAdmissionsDescriptorPrefixPattern =
  /^(?:(?:formulario|ficha|p[aá]gina|portal(?:es)?|solicitud(?:es)?)\s+de\s+(?:admisiones|ingreso)|admisiones(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|ingreso\s+(?:del?\s+curso|de\s+curso|al\s+curso))(?:\s+(?:del|de|para\s+el|para))?\s*(?:[-:/|]\s*)?/i;

const firstRunSpanishAdmissionsDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:formulario|ficha|p[aá]gina|portal(?:es)?|solicitud(?:es)?)\s+de\s+(?:admisiones|ingreso)|admisiones(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|ingreso\s+(?:del?\s+curso|de\s+curso|al\s+curso))\s*$/i;

const firstRunLooseEnrollmentDescriptorPrefixPattern =
  /^(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+para\s+(?:la\s+)?(?:pre)?inscripci[oó]n(?:es)?)(?:\s*(?:[-:/|]\s*))/i;

const firstRunLooseEnrollmentDescriptorSuffixPattern =
  /\s*(?:[-:/|]\s*)?(?:(?:formulario|ficha|p[aá]gina|solicitud(?:es)?)\s+para\s+(?:la\s+)?(?:pre)?inscripci[oó]n(?:es)?)\s*$/i;

const firstRunSpanishRegistrationNounPrefixPattern =
  /^(?:(?:[Pp]re[-\s]?)?[Ii]nscripci[oóÓ]n(?:[Ee]s)?|[Rr]egistro(?:[Ss])?|[Mm]atr[iíÍ]cula(?:[Ss])?|[Ss]olicitud(?:[Ee]s)?)(?:\s+(?:del?|para(?:\s+el)?|al)\s+curso)?(?:\s*[-:/|]\s*|\s+(?=[A-ZÁÉÍÓÚÑ0-9]))/;

const unwrapFirstRunDescriptorWrappedTitle = (title: string) => {
  const trimmedTitle = title.trim();
  const parenthesizedTitle = /^\(([^()]+)\)$/.exec(trimmedTitle);
  if (parenthesizedTitle?.[1]?.trim()) return parenthesizedTitle[1].trim();

  const bracketedTitle = /^\[([^[\]]+)\]$/.exec(trimmedTitle);
  if (bracketedTitle?.[1]?.trim()) return bracketedTitle[1].trim();

  const quotedTitle = /^(?:"([^"\n]+)"|'([^'\n]+)'|“([^”\n]+)”|‘([^’\n]+)’|«([^»\n]+)»)$/.exec(trimmedTitle);
  const unquotedTitle = quotedTitle?.slice(1).find(Boolean)?.trim();
  if (unquotedTitle) return unquotedTitle;

  return trimmedTitle;
};

const firstRunResponseSheetSuffixPattern =
  /\s*(?:\(|\[)\s*(?:(?:form\s+)?responses?|respuestas?(?:\s+del\s+formulario)?)(?:\s+\d+)?\s*(?:\)|\])\s*$/i;
const firstRunResponseSheetPrefixPattern =
  /^(?:(?:form\s+)?responses?|respuestas?(?:\s+del\s+formulario)?)(?:\s+\d+)?\s*(?:[-:/|]\s*)/i;

const stripFirstRunResponseSheetSuffix = (title: string) =>
  title.replace(firstRunResponseSheetSuffixPattern, '').trim();

const stripFirstRunCohortDescriptorPrefixOnce = (title: string) => {
  const trimmedTitle = title.trim();
  const normalizedTitle = normalizeFirstRunDescriptorSeparators(trimmedTitle);
  const strippedTitle = normalizedTitle
    .replace(firstRunResponseSheetPrefixPattern, '')
    .replace(firstRunUrlDescriptorPrefixPattern, '')
    .replace(firstRunShortlinkDescriptorPrefixPattern, '')
    .replace(firstRunQrRegistrationDescriptorPrefixPattern, '')
    .replace(firstRunDataSourceDescriptorPrefixPattern, '')
    .replace(firstRunProviderFormDescriptorPrefixPattern, '')
    .replace(firstRunStaticFormBackendDescriptorPrefixPattern, '')
    .replace(firstRunEmergingFormProviderDescriptorPrefixPattern, '')
    .replace(firstRunRegistrationLinkDescriptorPrefixPattern, '')
    .replace(firstRunEnrollmentFlowDescriptorPrefixPattern, '')
    .replace(firstRunOfferDescriptorPrefixPattern, '')
    .replace(firstRunUrgencyOfferDescriptorPrefixPattern, '')
    .replace(firstRunCallToActionDescriptorPrefixPattern, '')
    .replace(firstRunSeatReservationCallToActionDescriptorPrefixPattern, '')
    .replace(firstRunOpenEnrollmentDescriptorPrefixPattern, '')
    .replace(firstRunSalesDescriptorPrefixPattern, '')
    .replace(firstRunPaymentDescriptorPrefixPattern, '')
    .replace(firstRunPaymentEvidenceDescriptorPrefixPattern, '')
    .replace(firstRunAgreementDescriptorPrefixPattern, '')
    .replace(firstRunSignupSheetDescriptorPrefixPattern, '')
    .replace(firstRunRosterDescriptorPrefixPattern, '')
    .replace(firstRunAttendanceDescriptorPrefixPattern, '')
    .replace(firstRunWorkshopDescriptorPrefixPattern, '')
    .replace(firstRunClassDescriptorPrefixPattern, '')
    .replace(firstRunTrialLessonDescriptorPrefixPattern, '')
    .replace(firstRunProgramDescriptorPrefixPattern, '')
    .replace(firstRunLiveSessionDescriptorPrefixPattern, '')
    .replace(firstRunPriorityWaitlistDescriptorPrefixPattern, '')
    .replace(firstRunInvitationDescriptorPrefixPattern, '')
    .replace(firstRunWaitlistDescriptorPrefixPattern, '')
    .replace(firstRunLeadMagnetDescriptorPrefixPattern, '')
    .replace(firstRunDownloadableResourceDescriptorPrefixPattern, '')
    .replace(firstRunCourseInfoAssetDescriptorPrefixPattern, '')
    .replace(firstRunCourseInfoPageDescriptorPrefixPattern, '')
    .replace(firstRunCampaignDescriptorPrefixPattern, '')
    .replace(firstRunLaunchDescriptorPrefixPattern, '')
    .replace(firstRunAdAssetDescriptorPrefixPattern, '')
    .replace(firstRunEmergingSocialLeadDescriptorPrefixPattern, '')
    .replace(firstRunMessagingAutomationDescriptorPrefixPattern, '')
    .replace(firstRunAutomationPlumbingDescriptorPrefixPattern, '')
    .replace(firstRunCommunityGroupDescriptorPrefixPattern, '')
    .replace(firstRunEventPlatformDescriptorPrefixPattern, '')
    .replace(firstRunGenericEventDescriptorPrefixPattern, '')
    .replace(firstRunEmailMarketingFormDescriptorPrefixPattern, '')
    .replace(firstRunNoCodeLandingDescriptorPrefixPattern, '')
    .replace(firstRunCoursePlatformDescriptorPrefixPattern, '')
    .replace(firstRunCrmFormDescriptorPrefixPattern, '')
    .replace(firstRunCrmWorkflowDescriptorPrefixPattern, '')
    .replace(firstRunCrmProviderFormDescriptorPrefixPattern, '')
    .replace(firstRunInquiryDescriptorPrefixPattern, '')
    .replace(firstRunSurveyDescriptorPrefixPattern, '')
    .replace(firstRunInfoSessionDescriptorPrefixPattern, '')
    .replace(firstRunPublicRegistrationDescriptorPrefixPattern, '')
    .replace(firstRunSpanishPortalDescriptorPrefixPattern, '')
    .replace(firstRunOnlineRegistrationDescriptorPrefixPattern, '')
    .replace(firstRunPostSubmitDescriptorPrefixPattern, '')
    .replace(firstRunLandingPageDescriptorPrefixPattern, '')
    .replace(firstRunStandalonePublicPageDescriptorPrefixPattern, '')
    .replace(firstRunBioLinkDescriptorPrefixPattern, '')
    .replace(firstRunCourseWebsiteDescriptorPrefixPattern, '')
    .replace(firstRunLearningPortalDescriptorPrefixPattern, '')
    .replace(firstRunCourseCatalogDescriptorPrefixPattern, '')
    .replace(firstRunScheduleDescriptorPrefixPattern, '')
    .replace(firstRunSchedulingProviderLinkDescriptorPrefixPattern, '')
    .replace(firstRunReservationDescriptorPrefixPattern, '')
    .replace(firstRunSchedulingProviderStandalonePrefixPattern, '')
    .replace(firstRunConsultationCallDescriptorPrefixPattern, '')
    .replace(firstRunAdminWorkflowDescriptorPrefixPattern, '')
    .replace(firstRunRegistrationDashboardDescriptorPrefixPattern, '')
    .replace(firstRunCourseEnrollmentConnectorPrefixPattern, '')
    .replace(firstRunPreMatriculaDescriptorPrefixPattern, '')
    .replace(firstRunMatriculacionDescriptorPrefixPattern, '')
    .replace(firstRunSpanishAdmissionsDescriptorPrefixPattern, '')
    .replace(firstRunLooseEnrollmentDescriptorPrefixPattern, '')
    .replace(firstRunSpanishRegistrationNounPrefixPattern, '')
    .replace(firstRunUntitledDescriptorPrefixPattern, '')
    .replace(firstRunOnboardingDescriptorPrefixPattern, '')
    .replace(firstRunOrientationDescriptorPrefixPattern, '')
    .replace(firstRunCohortDescriptorPrefixPattern, '')
    .replace(firstRunVariantDescriptorPrefixPattern, '')
    .replace(firstRunTemplateDescriptorPrefixPattern, '')
    .replace(firstRunCopyDescriptorPrefixPattern, '')
    .replace(firstRunAuditionDescriptorPrefixPattern, '')
    .replace(firstRunAssessmentDescriptorPrefixPattern, '')
    .replace(firstRunApplicationDescriptorPrefixPattern, '')
    .replace(firstRunFinancialAidDescriptorPrefixPattern, '')
    .replace(
      /^(?:formulario\s+(?:p[uú]blico|del?\s+curso|de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+reserva\s+de\s+cupos?|de\s+admisi[oó]n|de\s+matr[ií]cula|de\s+contacto|de\s+consulta|de\s+inter[eé]s|para\s+el|para)|ficha\s+(?:de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+admisi[oó]n|de\s+matr[ií]cula|del?\s+curso|de\s+curso|para\s+el|para)|p[aá]gina\s+(?:de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+admisi[oó]n|de\s+matr[ií]cula|(?:p[uú]blica\s+)?del?\s+curso)|solicitud(?:es)?\s+(?:de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+admisi[oó]n|de\s+matr[ií]cula|de\s+cupos?|del?\s+curso|de\s+curso)|inscripciones?\s+(?:del?\s+curso|de\s+curso)|pre[-\s]?registros?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|registros?\s+(?:del?\s+curso|de\s+curso|al\s+curso)|reservas?\s+de\s+cupos?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|preinscripciones?(?:\s+(?:del?\s+curso|de\s+curso))?|matr[ií]culas?(?:\s+(?:del?\s+curso|de\s+curso|de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro))?|(?:pre)?inscripci[oó]n(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|admisi[oó]n(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|public\s+form|course\s+form|contact\s+form|inquiry\s+form|enquiry\s+form|lead\s+form|(?:course\s+)?sign[-\s]?up(?:\s+(?:form|page|portal))?|(?:public\s+)?course\s+page|(?:(?:course|student)\s+)?(?:intake|submissions?|inscriptions?|(?:pre[-\s]?)?registration|enrollment|admissions?)(?:\s+(?:form|page|portal|request))?|landing\s+(?:del\s+curso|de\s+curso|de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|para\s+el|para|del|de)|course\s+landing(?:\s+page)?|landing\s+page)(?:\s+(?:del|de|para\s+el|para|for))?\s*(?:[-:/|]\s*)?/i,
      '',
    )
    .replace(firstRunCourseNounDescriptorPrefixPattern, '')
    .trim();
  if (strippedTitle === normalizedTitle) return trimmedTitle;

  const strippedCourseNoun = strippedTitle
    .replace(/^(?:curso|course)\s*(?:[-:/|]\s*)?/i, '')
    .replace(/^(?:de|del|para(?:\s+el)?)\s+/, '')
    .trim();

  return unwrapFirstRunDescriptorWrappedTitle(strippedCourseNoun || strippedTitle);
};

const stripFirstRunCohortDescriptorPrefix = (title: string) => {
  let currentTitle = title.trim();

  for (let pass = 0; pass < 4; pass += 1) {
    const nextTitle = stripFirstRunCohortDescriptorPrefixOnce(currentTitle);
    if (nextTitle === currentTitle) return currentTitle;
    currentTitle = nextTitle;
  }

  return currentTitle;
};

const stripFirstRunCohortDescriptorSuffixOnce = (title: string) => {
  const trimmedTitle = title.trim();
  const normalizedTitle = normalizeFirstRunDescriptorSeparators(trimmedTitle);
  const strippedTitle = normalizedTitle
    .replace(firstRunUrlDescriptorSuffixPattern, '')
    .replace(firstRunShortlinkDescriptorSuffixPattern, '')
    .replace(firstRunQrRegistrationDescriptorSuffixPattern, '')
    .replace(firstRunDataSourceDescriptorSuffixPattern, '')
    .replace(firstRunProviderFormDescriptorSuffixPattern, '')
    .replace(firstRunStaticFormBackendDescriptorSuffixPattern, '')
    .replace(firstRunEmergingFormProviderDescriptorSuffixPattern, '')
    .replace(firstRunRegistrationLinkDescriptorSuffixPattern, '')
    .replace(firstRunEnrollmentFlowDescriptorSuffixPattern, '')
    .replace(firstRunOfferDescriptorSuffixPattern, '')
    .replace(firstRunUrgencyOfferDescriptorSuffixPattern, '')
    .replace(firstRunCallToActionDescriptorSuffixPattern, '')
    .replace(firstRunSeatReservationCallToActionDescriptorSuffixPattern, '')
    .replace(firstRunOpenEnrollmentDescriptorSuffixPattern, '')
    .replace(firstRunSalesDescriptorSuffixPattern, '')
    .replace(firstRunPaymentDescriptorSuffixPattern, '')
    .replace(firstRunPaymentEvidenceDescriptorSuffixPattern, '')
    .replace(firstRunAgreementDescriptorSuffixPattern, '')
    .replace(firstRunSignupSheetDescriptorSuffixPattern, '')
    .replace(firstRunRosterDescriptorSuffixPattern, '')
    .replace(firstRunAttendanceDescriptorSuffixPattern, '')
    .replace(firstRunWorkshopDescriptorSuffixPattern, '')
    .replace(firstRunClassDescriptorSuffixPattern, '')
    .replace(firstRunTrialLessonDescriptorSuffixPattern, '')
    .replace(firstRunProgramDescriptorSuffixPattern, '')
    .replace(firstRunLiveSessionDescriptorSuffixPattern, '')
    .replace(firstRunPriorityWaitlistDescriptorSuffixPattern, '')
    .replace(firstRunInvitationDescriptorSuffixPattern, '')
    .replace(firstRunWaitlistDescriptorSuffixPattern, '')
    .replace(firstRunLeadMagnetDescriptorSuffixPattern, '')
    .replace(firstRunDownloadableResourceDescriptorSuffixPattern, '')
    .replace(firstRunCourseInfoAssetDescriptorSuffixPattern, '')
    .replace(firstRunCourseInfoPageDescriptorSuffixPattern, '')
    .replace(firstRunCampaignDescriptorSuffixPattern, '')
    .replace(firstRunLaunchDescriptorSuffixPattern, '')
    .replace(firstRunAdAssetDescriptorSuffixPattern, '')
    .replace(firstRunEmergingSocialLeadDescriptorSuffixPattern, '')
    .replace(firstRunMessagingAutomationDescriptorSuffixPattern, '')
    .replace(firstRunAutomationPlumbingDescriptorSuffixPattern, '')
    .replace(firstRunCommunityGroupDescriptorSuffixPattern, '')
    .replace(firstRunEventPlatformDescriptorSuffixPattern, '')
    .replace(firstRunGenericEventDescriptorSuffixPattern, '')
    .replace(firstRunEmailMarketingFormDescriptorSuffixPattern, '')
    .replace(firstRunNoCodeLandingDescriptorSuffixPattern, '')
    .replace(firstRunCoursePlatformDescriptorSuffixPattern, '')
    .replace(firstRunCrmFormDescriptorSuffixPattern, '')
    .replace(firstRunCrmWorkflowDescriptorSuffixPattern, '')
    .replace(firstRunCrmProviderFormDescriptorSuffixPattern, '')
    .replace(firstRunInquiryDescriptorSuffixPattern, '')
    .replace(firstRunSurveyDescriptorSuffixPattern, '')
    .replace(firstRunInfoSessionDescriptorSuffixPattern, '')
    .replace(firstRunPublicRegistrationDescriptorSuffixPattern, '')
    .replace(firstRunSpanishPortalDescriptorSuffixPattern, '')
    .replace(firstRunOnlineRegistrationDescriptorSuffixPattern, '')
    .replace(firstRunPostSubmitDescriptorSuffixPattern, '')
    .replace(firstRunLandingPageDescriptorSuffixPattern, '')
    .replace(firstRunStandalonePublicPageDescriptorSuffixPattern, '')
    .replace(firstRunBioLinkDescriptorSuffixPattern, '')
    .replace(firstRunCourseWebsiteDescriptorSuffixPattern, '')
    .replace(firstRunLearningPortalDescriptorSuffixPattern, '')
    .replace(firstRunCourseCatalogDescriptorSuffixPattern, '')
    .replace(firstRunScheduleDescriptorSuffixPattern, '')
    .replace(firstRunSchedulingProviderLinkDescriptorSuffixPattern, '')
    .replace(firstRunReservationDescriptorSuffixPattern, '')
    .replace(firstRunConsultationCallDescriptorSuffixPattern, '')
    .replace(firstRunAdminWorkflowDescriptorSuffixPattern, '')
    .replace(firstRunRegistrationDashboardDescriptorSuffixPattern, '')
    .replace(firstRunCourseEnrollmentConnectorSuffixPattern, '')
    .replace(firstRunPreMatriculaDescriptorSuffixPattern, '')
    .replace(firstRunMatriculacionDescriptorSuffixPattern, '')
    .replace(firstRunSpanishAdmissionsDescriptorSuffixPattern, '')
    .replace(firstRunLooseEnrollmentDescriptorSuffixPattern, '')
    .replace(firstRunUntitledDescriptorSuffixPattern, '')
    .replace(firstRunOnboardingDescriptorSuffixPattern, '')
    .replace(firstRunOrientationDescriptorSuffixPattern, '')
    .replace(firstRunCohortDescriptorSuffixPattern, '')
    .replace(firstRunVariantDescriptorSuffixPattern, '')
    .replace(firstRunTemplateDescriptorSuffixPattern, '')
    .replace(firstRunCopyDescriptorSuffixPattern, '')
    .replace(firstRunAuditionDescriptorSuffixPattern, '')
    .replace(firstRunAssessmentDescriptorSuffixPattern, '')
    .replace(firstRunFinancialAidDescriptorSuffixPattern, '')
    .replace(firstRunApplicationDescriptorSuffixPattern, '')
    .replace(
      /\s*(?:[-:/|]\s*)?(?:formulario\s+(?:p[uú]blico|del?\s+curso|de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+reserva\s+de\s+cupos?|de\s+admisi[oó]n|de\s+matr[ií]cula|de\s+contacto|de\s+consulta|de\s+inter[eé]s)|ficha\s+(?:de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+admisi[oó]n|de\s+matr[ií]cula|del?\s+curso|de\s+curso)|p[aá]gina\s+(?:de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+admisi[oó]n|de\s+matr[ií]cula|(?:p[uú]blica\s+)?del?\s+curso)|solicitud(?:es)?\s+(?:de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro|de\s+admisi[oó]n|de\s+matr[ií]cula|de\s+cupos?|del?\s+curso|de\s+curso)|inscripciones?\s+(?:del?\s+curso|de\s+curso)|pre[-\s]?registros?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|registros?\s+(?:del?\s+curso|de\s+curso|al\s+curso)|reservas?\s+de\s+cupos?(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|preinscripciones?(?:\s+(?:del?\s+curso|de\s+curso))?|matr[ií]culas?(?:\s+(?:del?\s+curso|de\s+curso|de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro))?|(?:pre)?inscripci[oó]n(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|admisi[oó]n(?:\s+(?:del?\s+curso|de\s+curso|al\s+curso))?|public\s+form|course\s+form|contact\s+form|inquiry\s+form|enquiry\s+form|lead\s+form|(?:course\s+)?sign[-\s]?up(?:\s+(?:form|page|portal))?|(?:public\s+)?course\s+page|(?:(?:course|student)\s+)?(?:intake|submissions?|inscriptions?|(?:pre[-\s]?)?registration|enrollment|admissions?)(?:\s+(?:form|page|portal|request))?|landing\s+(?:del\s+curso|de\s+curso|de\s+(?:pre)?inscripci[oó]n|de\s+pre[-\s]?registro|de\s+registro)|course\s+landing(?:\s+page)?|landing\s+page)\s*$/i,
      '',
    )
    .replace(firstRunCourseNounDescriptorSuffixPattern, '')
    .trim();

  return strippedTitle === normalizedTitle ? trimmedTitle : unwrapFirstRunDescriptorWrappedTitle(strippedTitle);
};

const stripFirstRunCohortDescriptorSuffix = (title: string) => {
  let currentTitle = title.trim();

  for (let pass = 0; pass < 4; pass += 1) {
    const nextTitle = stripFirstRunCohortDescriptorSuffixOnce(currentTitle);
    if (nextTitle === currentTitle) return currentTitle;
    currentTitle = nextTitle;
  }

  return currentTitle;
};

const stripFirstRunCohortDescriptorFallback = (label: string) => {
  const strippedLabel = stripFirstRunCohortDescriptorSuffix(
    stripFirstRunCohortDescriptorPrefix(label),
  );

  return strippedLabel || label.trim();
};

const readableFirstRunCohortFallbackLabel = (slug: string) =>
  stripFirstRunCohortDescriptorFallback(readableCohortFallbackLabel(slug));

const isGenericFirstRunCohortLabel = (label: string) => {
  const strippedLabel = stripFirstRunCohortDescriptorSuffix(
    stripFirstRunCohortDescriptorPrefix(label),
  );

  return strippedLabel.trim() === '';
};

const cohortOptionLabel = (cohort: CourseCohortOptionDTO) => {
  const slug = cohort.ccSlug.trim();
  const title = cohort.ccTitle?.trim();
  const fallbackLabel = readableCohortFallbackLabel(slug);
  if (!title) return fallbackLabel;

  const strippedTitle = stripFirstRunCohortDescriptorSuffix(
    stripFirstRunCohortDescriptorPrefix(stripTrailingCohortSlug(title, slug)),
  );
  const displayTitle = unwrapFirstRunDescriptorWrappedTitle(
    stripFirstRunResponseSheetSuffix(dedupeRepeatedCohortTitleSegments(strippedTitle)),
  ) || fallbackLabel;
  if (!slug) return displayTitle;

  const displayKey = normalizeCohortLabelKey(displayTitle);
  const slugKey = normalizeCohortLabelKey(slug);
  const fallbackKey = normalizeCohortLabelKey(fallbackLabel);
  if (displayKey === slugKey || displayKey === fallbackKey) return fallbackLabel;

  return `${displayTitle} (${slug})`;
};

const hasCohortTitleFormattingWorthPreserving = (label: string) => (
  Array.from(label).some((character) => character.charCodeAt(0) > 0x7f) || /\b[A-Z]{2,}\b/.test(label)
);

const cohortFirstRunLabel = (cohort: CourseCohortOptionDTO) => {
  const slug = cohort.ccSlug.trim();
  const fallbackLabel = stripFirstRunCohortDescriptorFallback(humanizeCohortSlug(slug) || slug);
  const title = stripFirstRunCohortPresentationMarkers(cohort.ccTitle ?? '');
  if (!title) return fallbackLabel;
  const strippedLabel = stripFirstRunCohortDescriptorSuffix(
    stripFirstRunCohortDescriptorPrefix(stripTrailingCohortSlug(title, slug)),
  );
  const displayLabel = unwrapFirstRunDescriptorWrappedTitle(
    stripFirstRunResponseSheetSuffix(dedupeRepeatedCohortTitleSegments(strippedLabel)),
  );
  if (!displayLabel) return fallbackLabel;
  if (normalizeCohortLabelKey(displayLabel) === normalizeCohortLabelKey(slug)) {
    if (hasCohortTitleFormattingWorthPreserving(displayLabel)) return displayLabel;
    return fallbackLabel;
  }
  return displayLabel;
};

const cohortSummaryLabel = (cohort: CourseCohortOptionDTO) => {
  const summaryLabel = cohortFirstRunLabel(cohort);
  return summaryLabel || cohortOptionLabel(cohort);
};

const sourceLabelSpecialWords = new Map([
  ['api', 'API'],
  ['crm', 'CRM'],
  ['facebook', 'Facebook'],
  ['fb', 'Facebook'],
  ['instagram', 'Instagram'],
  ['ig', 'Instagram'],
  ['linkedin', 'LinkedIn'],
  ['meta', 'Meta'],
  ['ms', 'MS'],
  ['qr', 'QR'],
  ['sms', 'SMS'],
  ['tiktok', 'TikTok'],
  ['utm', 'UTM'],
  ['wati', 'WATI'],
  ['whatsapp', 'WhatsApp'],
  ['youtube', 'YouTube'],
]);

const formatDelimitedSourceWord = (word: string, index: number) => {
  const lowerWord = word.toLocaleLowerCase('es');
  const acronym = sourceLabelSpecialWords.get(lowerWord);
  if (acronym) return acronym;
  if (index > 0) return lowerWord;
  return `${lowerWord.charAt(0).toLocaleUpperCase('es')}${lowerWord.slice(1)}`;
};

const sourceWordComparisonKey = (word: string) => {
  const lowerWord = word.toLocaleLowerCase('es');
  return (sourceLabelSpecialWords.get(lowerWord) ?? lowerWord).toLocaleLowerCase('es');
};

const dedupeAdjacentSourceWords = (words: readonly string[]) => {
  const dedupedWords: string[] = [];

  words.forEach((word) => {
    const wordKey = sourceWordComparisonKey(word);
    const previousWord = dedupedWords[dedupedWords.length - 1];
    const previousWordKey = previousWord == null ? '' : sourceWordComparisonKey(previousWord);

    if (wordKey && wordKey === previousWordKey) return;

    dedupedWords.push(word);
  });

  return dedupedWords;
};

const humanizeDelimitedSourceLabel = (source: string) => {
  const hasDelimitedParts = /[_./-]/.test(source);
  const hasCamelCaseParts = /[a-z0-9][A-Z]/.test(source);
  if (!hasDelimitedParts && !hasCamelCaseParts) {
    return sourceLabelSpecialWords.get(source.trim().toLocaleLowerCase('es')) ?? source;
  }
  const normalized = source
    .replace(/([A-Z]+)([A-Z][a-z])/g, '$1 $2')
    .replace(/([a-z0-9])([A-Z])/g, '$1 $2')
    .replace(/[_./-]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
  if (!normalized) return source;
  return dedupeAdjacentSourceWords(normalized.split(' ')).map(formatDelimitedSourceWord).join(' ');
};

const normalizeSourceAliasKey = (source: string) =>
  normalizeLocalSearchText(humanizeDelimitedSourceLabel(source));

const sourceAliasKeyVariants = (sourceKey: string) => {
  const variants = new Set([sourceKey]);
  const pluralSuffixes = [
    ['form', 'forms'],
    ['link', 'links'],
    ['page', 'pages'],
    ['portal', 'portals'],
    ['button', 'buttons'],
    ['url', 'urls'],
  ] as const;

  const addPluralVariants = (key: string) => {
    for (const [singularSuffix, pluralSuffix] of pluralSuffixes) {
      if (key.endsWith(` ${pluralSuffix}`)) {
        variants.add(key.replace(new RegExp(`\\s+${pluralSuffix}$`), ` ${singularSuffix}`));
        variants.add(key.replace(new RegExp(`\\s+${pluralSuffix}$`), ''));
        variants.add(key.replace(new RegExp(`\\s+${pluralSuffix}$`), singularSuffix));
      } else if (key.endsWith(` ${singularSuffix}`)) {
        variants.add(key.replace(new RegExp(`\\s+${singularSuffix}$`), ` ${pluralSuffix}`));
        variants.add(key.replace(new RegExp(`\\s+${singularSuffix}$`), ''));
        variants.add(key.replace(new RegExp(`\\s+${singularSuffix}$`), singularSuffix));
      } else if (key.endsWith(pluralSuffix)) {
        variants.add(key.replace(new RegExp(`${pluralSuffix}$`), singularSuffix));
      }
    }
  };

  addPluralVariants(sourceKey);

  for (const containerSuffix of ['response', 'responses', 'submission', 'submissions']) {
    if (sourceKey.endsWith(` ${containerSuffix}`)) {
      const baseSourceKey = sourceKey.replace(new RegExp(`\\s+${containerSuffix}$`), '');
      variants.add(baseSourceKey);
      addPluralVariants(baseSourceKey);
    }
  }

  return variants;
};

const placeholderMetadataValueKeys = new Set([
  '-',
  'unknown',
  'desconocido',
  'desconocida',
  'pendiente',
  'pendiente por validar',
  'por actualizar',
  'por confirmar',
  'por definir',
  'por validar',
  'sin dato',
  'sin datos',
  'sin actualizar',
  'sin campana',
  'sin contenido',
  'sin fuente',
  'sin importacion',
  'sin medio',
  'sin origen',
  'sin utm',
  'n/a',
  'na',
  'n.d.',
  'nd',
  'none',
  'null',
  'undefined',
  'not set',
  'not provided',
  'not available',
  'not tracked',
  'no campaign',
  'no informa',
  'no informada',
  'no informado',
  'no registra',
  'no registrada',
  'no registrado',
  'no reportada',
  'no reportado',
  'no aplica',
  'no disponible',
  'sin registrar',
  'sin registro',
  'sin migracion',
  'tbd',
].map(normalizeSourceAliasKey));

const isPlaceholderMetadataValue = (value: string | null | undefined) => {
  const valueKey = normalizeSourceAliasKey(value ?? '');
  if (!valueKey) return true;

  return Array.from(sourceAliasKeyVariants(valueKey)).some((variant) =>
    placeholderMetadataValueKeys.has(variant)
  );
};

const defaultPublicFormSourceKeys = new Set([
  defaultPublicFormSource,
  '-',
  'unknown',
  'desconocido',
  'desconocida',
  'sin fuente',
  'sin origen',
  'n/a',
  'na',
  'none',
  'null',
  'undefined',
  'not set',
  'not provided',
  'default',
  'direct',
  'direct web',
  'direct website',
  'manual',
  'manual entry',
  'manual import',
  'manual upload',
  'manual data entry',
  'admin entry',
  'admin import',
  'backend import',
  'bulk import',
  'bulk upload',
  'csv',
  'csv import',
  'csv upload',
  'excel import',
  'excel upload',
  'xlsx import',
  'xlsx upload',
  'spreadsheet import',
  'spreadsheet upload',
  'sheet import',
  'sheet upload',
  'legacy import',
  'legacy migration',
  'data import',
  'data migration',
  'database import',
  'database migration',
  'seed data',
  'seed import',
  'system import',
  'system migration',
  'carga manual',
  'carga masiva',
  'carga csv',
  'importacion manual',
  'importacion masiva',
  'importacion csv',
  'importacion de datos',
  'migracion',
  'migracion de datos',
  'datos migrados',
  'organic',
  'organic web',
  'organic website',
  'website organic',
  'web organic',
  'organico',
  'organica',
  'trafico directo',
  'trafico organico',
  'homepage',
  'homepage form',
  'home page',
  'home page form',
  'home page signup',
  'pagina de inicio',
  'formulario de pagina de inicio',
  'landing page',
  'web',
  'website',
  'web form',
  'website form',
  'course website',
  'public website',
  'course landing',
  'course landing page',
  'public landing',
  'public landing page',
  'public form',
  'public course form',
  'public course page',
  'public signup',
  'public signup form',
  'public course signup',
  'public course signup form',
  'public registration',
  'public course registration',
  'public registration form',
  'public course registration form',
  'public enrollment',
  'public course enrollment',
  'public enrollment form',
  'public course enrollment form',
  'facebook instant form',
  'facebook instant forms',
  'fb instant form',
  'fb instant forms',
  'ig instant form',
  'ig instant forms',
  'instagram instant form',
  'instagram instant forms',
  'meta instant form',
  'meta instant forms',
  'facebook lead ad',
  'facebook lead ads',
  'facebook lead ad form',
  'facebook lead ads form',
  'facebook lead form',
  'facebook leadgen',
  'facebook leadgen form',
  'facebook leads',
  'facebook ads lead form',
  'facebook ads leadgen',
  'facebook ads leads',
  'fb lead ad',
  'fb lead ads',
  'fb lead ad form',
  'fb lead ads form',
  'fb lead form',
  'fb leadgen',
  'fb leadgen form',
  'fb leads',
  'fb ads lead form',
  'fb ads leadgen',
  'fb ads leads',
  'instagram lead ad',
  'instagram lead ads',
  'instagram lead ad form',
  'instagram lead ads form',
  'instagram lead form',
  'instagram leadgen',
  'instagram leadgen form',
  'instagram leads',
  'instagram ads lead form',
  'instagram ads leadgen',
  'instagram ads leads',
  'ig lead ad',
  'ig lead ads',
  'ig lead ad form',
  'ig lead ads form',
  'ig lead form',
  'ig leadgen',
  'ig leadgen form',
  'ig leads',
  'ig ads lead form',
  'ig ads leadgen',
  'ig ads leads',
  'meta lead ad',
  'meta lead ads',
  'meta lead ad form',
  'meta lead ads form',
  'meta lead form',
  'meta leadgen',
  'meta leadgen form',
  'meta leads',
  'meta ads lead form',
  'meta ads leadgen',
  'meta ads leads',
  'linkedin lead gen form',
  'linkedin lead form',
  'linkedin leadgen',
  'linkedin leadgen form',
  'linkedin leads',
  'tiktok lead form',
  'tiktok leadgen',
  'tiktok leadgen form',
  'tiktok leads',
  'whatsapp lead form',
  'whatsapp leadgen',
  'whatsapp leadgen form',
  'whatsapp leads',
  'whatsapp form',
  'whatsapp forms',
  'whatsapp registration',
  'whatsapp registration form',
  'whatsapp signup',
  'whatsapp signup form',
  'lead capture',
  'lead generation',
  'lead gen',
  'leadgen',
  'leadgen form',
  'leadgen forms',
  'leads de facebook',
  'leads de instagram',
  'leads de meta',
  'leads de linkedin',
  'leads de tiktok',
  'leads de whatsapp',
  'captacion de leads',
  'captura de leads',
  'generacion de leads',
  'captacion de prospectos',
  'captura de prospectos',
  'generacion de prospectos',
  'captacion de interesados',
  'captura de interesados',
  'generacion de interesados',
  'formulario de captacion de leads',
  'formulario de captura de prospectos',
  'formulario de generacion de interesados',
  'course signup',
  'course signup page',
  'course signup form',
  'signup form',
  'pre registration',
  'pre registration form',
  'pre registration page',
  'pre registration portal',
  'course pre registration',
  'course pre registration form',
  'course pre registration page',
  'course pre registration portal',
  'course registration',
  'course registration form',
  'course registration page',
  'registration portal',
  'course registration portal',
  'course enrollment',
  'course enrollment form',
  'course enrollment page',
  'enrollment form',
  'enrollment portal',
  'course enrollment portal',
  'student enrollment portal',
  'student registration form',
  'admission form',
  'admissions form',
  'admission page',
  'admissions page',
  'course admission page',
  'course admissions page',
  'application form',
  'application page',
  'course application page',
  'intake form',
  'intake page',
  'course intake',
  'course intake form',
  'course intake page',
  'student intake form',
  'student intake page',
  'waitlist',
  'waitlist form',
  'waitlist page',
  'waitlist portal',
  'course waitlist',
  'course waitlist form',
  'course waitlist page',
  'course waitlist portal',
  'waiting list',
  'waiting list form',
  'waiting list page',
  'lista de espera',
  'lista de espera del curso',
  'formulario de lista de espera',
  'pagina de lista de espera',
  'portal de lista de espera',
  'payment form',
  'payment page',
  'payment link',
  'payment portal',
  'payment button',
  'online payment form',
  'online payment page',
  'online payment link',
  'checkout',
  'checkout form',
  'checkout page',
  'checkout link',
  'checkout portal',
  'course payment form',
  'course payment page',
  'course payment link',
  'course payment portal',
  'course checkout',
  'course checkout form',
  'course checkout page',
  'course checkout link',
  'stripe checkout',
  'stripe payment link',
  'paypal checkout',
  'paypal payment link',
  'payphone checkout',
  'payphone payment link',
  'payphone payment button',
  'datafast checkout',
  'datafast payment link',
  'kushki checkout',
  'kushki payment link',
  'paymentez checkout',
  'paymentez payment link',
  'deuna checkout',
  'deuna payment link',
  'mercado pago checkout',
  'mercado pago payment link',
  'mercadopago checkout',
  'mercadopago payment link',
  'hotmart checkout',
  'hotmart payment link',
  'hotmart payment page',
  'kiwify checkout',
  'kiwify payment link',
  'kiwify payment page',
  'lemon squeezy checkout',
  'lemon squeezy payment link',
  'gumroad checkout',
  'gumroad payment link',
  'payhip checkout',
  'payhip payment link',
  'samcart checkout',
  'samcart payment link',
  'thrivecart checkout',
  'thrivecart payment link',
  'shopify checkout',
  'shopify payment link',
  'woocommerce checkout',
  'woocommerce payment link',
  'calendly',
  'calendly link',
  'calendly booking link',
  'calendly booking page',
  'acuity',
  'acuity scheduling',
  'acuity booking link',
  'acuity scheduling page',
  'cal com',
  'cal com link',
  'cal com booking link',
  'cal com booking page',
  'simplybook',
  'simply book',
  'simplybook page',
  'setmore',
  'setmore booking link',
  'youcanbookme',
  'you can book me',
  'you can book me link',
  'tidycal',
  'tidy cal',
  'tidycal link',
  'microsoft bookings',
  'microsoft bookings link',
  'microsoft bookings booking link',
  'microsoft bookings booking page',
  'microsoft bookings reservation link',
  'microsoft bookings appointment schedule',
  'ms bookings',
  'ms bookings link',
  'ms bookings booking link',
  'ms bookings booking page',
  'google calendar booking link',
  'google calendar appointment schedule',
  'discovery call booking',
  'discovery call form',
  'consultation call booking',
  'consultation call form',
  'intro call link',
  'strategy call booking',
  'formulario de llamada de consulta',
  'enlace de llamada de consulta',
  'formulario de llamada de diagnostico',
  'enlace de llamada de diagnostico',
  'eventbrite',
  'eventbrite event',
  'eventbrite event page',
  'eventbrite registration',
  'eventbrite registration page',
  'eventbrite signup page',
  'luma',
  'luma event',
  'luma event page',
  'luma registration page',
  'luma signup page',
  'lu ma event page',
  'meetup',
  'meetup event',
  'meetup event page',
  'meetup event registration',
  'meetup registration page',
  'ticket tailor',
  'ticket tailor event page',
  'ticket tailor registration page',
  'tickettailor',
  'tickettailor event page',
  'humanitix',
  'humanitix event page',
  'humanitix registration page',
  'eventzilla',
  'eventzilla event page',
  'eventzilla registration page',
  'sympla',
  'sympla event page',
  'sympla registration page',
  'entradium',
  'entradium event page',
  'entradium registration page',
  'event registration',
  'event registration page',
  'event signup page',
  'event rsvp link',
  'formulario de eventbrite',
  'pagina de eventbrite',
  'registro de evento',
  'formulario publico',
  'formulario publico del curso',
  'formulario publico de curso',
  'formulario publico de inscripcion',
  'formulario de inscripcion publica',
  'preinscripcion',
  'preinscripcion del curso',
  'preinscripcion de curso',
  'formulario de preinscripcion',
  'pagina de preinscripcion',
  'portal de preinscripcion',
  'pre registro',
  'pre registro del curso',
  'pre registro de curso',
  'formulario de pre registro',
  'pagina de pre registro',
  'portal de pre registro',
  'formulario online',
  'formulario en linea',
  'inscripcion publica',
  'inscripcion publica del curso',
  'inscripcion publica de curso',
  'inscripcion online',
  'inscripcion en linea',
  'registro publico',
  'registro publico del curso',
  'registro publico de curso',
  'registro online',
  'registro en linea',
  'formulario web',
  'sitio web',
  'sitio web del curso',
  'pagina web',
  'pagina web del curso',
  'pagina publica del curso',
  'pagina publica de curso',
  'pagina de inscripcion',
  'pagina de registro',
  'portal de inscripcion',
  'portal de registro',
  'registration form',
  'formulario de inscripcion',
  'formulario de registro',
  'formulario de ingreso',
  'pagina de admision',
  'pagina de admisiones',
  'pagina de ingreso',
  'formulario de pago',
  'pagina de pago',
  'enlace de pago',
  'portal de pago',
  'boton de pago',
  'botones de pago',
  'formulario de checkout',
  'pagina de checkout',
  'enlace de checkout',
  'portal de checkout',
  'solicitud de inscripcion',
  'solicitud de ingreso',
  'ficha de inscripcion',
  'google form',
  'google forms',
  'typeform',
  'tally',
  'tally form',
  'jot form',
  'jotform',
  'microsoft forms',
  'ms form',
  'airtable',
  'airtable form',
  'coda',
  'coda form',
  'coda forms',
  'hubspot',
  'hubspot forms',
  'go highlevel',
  'go highlevel form',
  'go high level',
  'go high level form',
  'gohighlevel',
  'gohighlevel form',
  'highlevel',
  'highlevel form',
  'high level',
  'high level form',
  'mailchimp',
  'mailchimp form',
  'manychat',
  'manychat form',
  'manychat forms',
  'paper form',
  'paper forms',
  'paperform',
  'survey monkey',
  'survey monkey form',
  'survey monkey forms',
  'surveymonkey',
  'surveymonkey form',
  'qualtrics',
  'qualtrics form',
  'qualtrics survey',
  'question pro',
  'question pro form',
  'questionpro',
  'questionpro form',
  'surveysparrow',
  'surveysparrow form',
  'surveysparrow survey',
  'survey sparrow',
  'survey sparrow form',
  'survey sparrow survey',
  'fillout form',
  'fillout forms',
  'cognito form',
  'cognito forms',
  'convert kit',
  'convert kit form',
  'convertkit',
  'convertkit form',
  'brevo',
  'brevo form',
  'sendinblue',
  'sendinblue form',
  'flodesk',
  'flodesk form',
  'mailer lite',
  'mailer lite form',
  'mailerlite',
  'mailerlite form',
  'klaviyo',
  'klaviyo form',
  'active campaign',
  'active campaign form',
  'activecampaign',
  'activecampaign form',
  'constant contact',
  'constant contact form',
  'keap',
  'keap form',
  'infusionsoft',
  'infusionsoft form',
  'substack form',
  'substack signup page',
  'crm lead form',
  'crm registration page',
  'crm intake form',
  'wati lead form',
  'kommo registration page',
  'pipedrive lead form',
  'wufoo',
  'wufoo form',
  'formstack',
  'formstack form',
  'zoho',
  'zoho form',
  'zoho forms',
  'gravity form',
  'gravity forms',
  'webflow',
  'webflow form',
  'webflow forms',
  'wix',
  'wix form',
  'wix forms',
  'squarespace',
  'squarespace form',
  'squarespace forms',
  'leadpages',
  'lead pages',
  'leadpages form',
  'lead pages form',
  'leadpages page',
  'lead pages page',
  'leadpages landing page',
  'lead pages landing page',
  'kajabi form',
  'kajabi page',
  'kajabi landing page',
  'teachable form',
  'teachable page',
  'teachable landing page',
  'thinkific form',
  'thinkific page',
  'thinkific landing page',
  'podia form',
  'podia page',
  'podia landing page',
  'click funnels form',
  'click funnels page',
  'clickfunnels form',
  'clickfunnels page',
  'kartra form',
  'kartra page',
  'systeme io form',
  'systeme io page',
  'carrd',
  'carrd form',
  'carrd page',
  'carrd landing page',
  'framer',
  'framer form',
  'framer page',
  'framer landing page',
  'unbounce',
  'unbounce form',
  'unbounce page',
  'unbounce landing page',
  'instapage',
  'instapage form',
  'instapage page',
  'instapage landing page',
  'landingi',
  'landingi form',
  'landingi page',
  'landingi landing page',
  'linktree',
  'link tree',
  'beacons',
  'beacons ai',
  'stan store',
  'koji',
  'koji to',
  'milkshake',
  'milkshake app',
  'bio link',
  'bio page',
  'profile link',
  'profile page',
  'link in bio',
  'enlace en bio',
  'link en bio',
  'enlace de perfil',
  'link de perfil',
  'pagina de bio',
  'notion',
  'notion form',
  'notion forms',
  'forms app',
  'forms app form',
  'forms app forms',
  'formspree',
  'formspree form',
  'formspree forms',
  'formsite',
  'formsite form',
  'formsite forms',
  '123 form builder',
  '123 form builder form',
  '123 form builder forms',
  '123 forms builder',
  '123 forms builder form',
  '123 forms builder forms',
  '123formbuilder',
  '123formbuilder form',
  '123formbuilder forms',
  'netlify',
  'netlify form',
  'netlify forms',
  'formkeep',
  'form keep',
  'formkeep form',
  'form keep form',
  'formbold',
  'formbold form',
  'formspark',
  'formspark form',
  'formsubmit',
  'formsubmit form',
  'getform',
  'get form',
  'getform form',
  'get form form',
  'basin',
  'basin form',
  'formcarry',
  'form carry',
  'formcarry form',
  'form carry form',
  'heyflow',
  'heyflow form',
  'heyflow registration form',
  'heyflow lead form',
  'outgrow',
  'outgrow form',
  'outgrow quiz',
  'interact',
  'interact quiz',
  'interact survey',
  'landbot',
  'landbot flow',
  'landbot registration flow',
  'perspective',
  'perspective co',
  'perspective co form',
  'perspective funnel',
  'feathery',
  'feathery form',
  'feathery intake form',
  'webhook',
  'registration webhook',
  'zapier',
  'zapier webhook',
  'zapier automation',
  'zapier registration automation',
  'make com',
  'make com scenario',
  'make com workflow',
  'integromat',
  'integromat scenario',
  'n8n',
  'n8n workflow',
  'n8n registration workflow',
  'pabbly connect',
  'pabbly connect integration',
  'integrately',
  'integrately automation',
].map(normalizeSourceAliasKey));

const registrationSourceLabel = (source: string | null | undefined) => {
  const trimmed = source?.trim() ?? '';
  return trimmed === '' ? 'Sin fuente' : humanizeDelimitedSourceLabel(trimmed);
};

const normalizeRegistrationSourceKey = (sourceLabel: string) =>
  registrationSourceLabel(sourceLabel).toLocaleLowerCase('es');

const isDefaultPublicFormSource = (sourceLabel: string) =>
  isPlaceholderMetadataValue(sourceLabel)
  || Array.from(sourceAliasKeyVariants(normalizeSourceAliasKey(sourceLabel))).some((sourceKey) =>
    defaultPublicFormSourceKeys.has(sourceKey)
  );

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
      if (!trimmedValue || isPlaceholderMetadataValue(trimmedValue)) return '';
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

const localSearchTextMatches = (value: string | null | undefined, localSearchKey: string) => {
  if (!localSearchKey) return false;

  const normalizedValue = normalizeLocalSearchText(value ?? '');
  if (normalizedValue.includes(localSearchKey)) return true;
  if (/^\d+$/.test(localSearchKey) && localSearchKey.length < MIN_PHONE_SEARCH_DIGITS) {
    return false;
  }

  const compactSearchKey = normalizeCompactLocalSearchText(localSearchKey);
  return Boolean(compactSearchKey)
    && normalizeCompactLocalSearchText(normalizedValue).includes(compactSearchKey);
};

const registrationVisibleSearchText = (
  reg: CourseRegistrationDTO,
  cohortLabelsBySlug: ReadonlyMap<string, string>,
) => {
  const courseSlug = reg.crCourseSlug.trim();
  return [
    normalizeRegistrationNameValue(reg.crFullName),
    normalizeRegistrationContactValue(reg.crEmail),
    `registro #${reg.crId}`,
    String(reg.crId),
    courseSlug,
    cohortLabelsBySlug.get(courseSlug),
    registrationStatusLabel(reg.crStatus),
    ...registrationContactStateSearchValues(reg),
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

  return phoneDigitsMatchLocalSearch(reg.crPhoneE164, localSearchDigitsKey);
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

const hasUsableRegistrationRecordId = (registrationId: number | null | undefined) => (
  typeof registrationId === 'number' && Number.isInteger(registrationId) && registrationId > 0
);

const registrationRecordLabel = (registrationId: number | null | undefined, lowercase = false) => {
  const labelPrefix = lowercase ? 'registro' : 'Registro';
  if (hasUsableRegistrationRecordId(registrationId)) {
    return `${labelPrefix} #${registrationId}`;
  }
  return `${labelPrefix} sin número`;
};

const registrationIdentityDisplay = (
  fullName: string | null | undefined,
  email: string | null | undefined,
  phone: string | null | undefined,
  registrationId?: number | null,
) => {
  const trimmedName = normalizeRegistrationNameValue(fullName) ?? '';
  const trimmedEmail = normalizeRegistrationContactValue(email) ?? '';
  const trimmedPhone = normalizeRegistrationContactValue(phone) ?? '';
  const fallbackIdentity = registrationRecordLabel(registrationId);

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
  if (normalizeRegistrationNameValue(reg.crFullName)) return 'name';
  if (normalizeRegistrationContactValue(reg.crEmail)) return 'email';
  if (normalizeRegistrationContactValue(reg.crPhoneE164)) return 'phone';
  return 'record';
};

const registrationNeedsContact = (
  reg: Pick<CourseRegistrationDTO, 'crEmail' | 'crPhoneE164'>,
) => (
  !normalizeRegistrationContactValue(reg.crEmail)
  && !normalizeRegistrationContactValue(reg.crPhoneE164)
);

const registrationContactStateSearchValues = (
  reg: Pick<CourseRegistrationDTO, 'crEmail' | 'crPhoneE164'>,
) => {
  const needsEmail = !normalizeRegistrationContactValue(reg.crEmail);
  const needsPhone = !normalizeRegistrationContactValue(reg.crPhoneE164);
  const values: string[] = [];

  if (needsEmail && needsPhone) {
    values.push(
      'contacto pendiente',
      'contactos pendientes',
      'inscripciones con contacto pendiente',
      'inscripciones visibles con contacto pendiente',
      'sin contacto',
      'sin correo ni teléfono',
      'sin correo ni telefono',
      missingContactSummary,
    );
  }

  if (needsEmail) {
    values.push(
      'correo pendiente',
      'correos pendientes',
      'email pendiente',
      'emails pendientes',
      'sin correo',
      'sin correos',
      'sin email',
      'sin emails',
    );
  }

  if (needsPhone) {
    values.push(
      'teléfono pendiente',
      'teléfonos pendientes',
      'telefono pendiente',
      'telefonos pendientes',
      'sin teléfono',
      'sin teléfonos',
      'sin telefono',
      'sin telefonos',
    );
  }

  return values;
};

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
  const orderedTargetLabels = (['name', 'email', 'phone', 'record'] as const)
    .filter((kind) => identityKinds.has(kind))
    .map((kind) => {
      if (kind === 'email') return 'el correo';
      if (kind === 'phone') return 'el teléfono';
      if (kind === 'record') {
        const recordIdentities = registrations.filter((reg) => registrationIdentityKind(reg) === 'record');
        return recordIdentities.some((reg) => hasUsableRegistrationRecordId(reg.crId))
          ? 'el número de registro'
          : 'el registro';
      }
      return 'el nombre';
    });

  if (orderedTargetLabels.length === 0) return 'el nombre';
  if (orderedTargetLabels.length === 1) return orderedTargetLabels[0] ?? 'el nombre';
  const lastTargetLabel = orderedTargetLabels[orderedTargetLabels.length - 1] ?? 'el número de registro';
  if (orderedTargetLabels.length === 2) return `${orderedTargetLabels[0] ?? 'el nombre'} o ${lastTargetLabel}`;
  return `${orderedTargetLabels.slice(0, -1).join(', ')} o ${lastTargetLabel}`;
};

const registrationContactSummary = (
  email: string | null | undefined,
  phone: string | null | undefined,
  visibleIdentityValues: readonly string[] = [],
) => {
  const trimmedEmail = normalizeRegistrationContactValue(email) ?? '';
  const trimmedPhone = normalizeRegistrationContactValue(phone) ?? '';
  const parts = [trimmedEmail, trimmedPhone].filter((value) => value !== '');
  if (parts.length === 0) return missingContactSummary;
  return visibleRegistrationContactParts(parts, visibleIdentityValues).join(' · ');
};

const registrationActionTargetLabel = (
  reg: Pick<CourseRegistrationDTO, 'crId' | 'crFullName' | 'crEmail' | 'crPhoneE164'>,
) => {
  const trimmedName = normalizeRegistrationNameValue(reg.crFullName) ?? '';
  if (trimmedName) return trimmedName;
  const trimmedEmail = normalizeRegistrationContactValue(reg.crEmail) ?? '';
  if (trimmedEmail) return trimmedEmail;
  const trimmedPhone = normalizeRegistrationContactValue(reg.crPhoneE164) ?? '';
  if (trimmedPhone) return trimmedPhone;
  return registrationRecordLabel(reg.crId, true);
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
      ? `${secondary} · ${registrationRecordLabel(reg.crId, true)}`
      : secondary;
    return `${baseLabel} (${disambiguatingContext})`;
  }

  return `${baseLabel} (${registrationRecordLabel(reg.crId, true)})`;
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

const buildLocalSearchPlaceholder = (
  registrations: readonly CourseRegistrationDTO[],
  { includeCourseTerm = true }: { includeCourseTerm?: boolean } = {},
) => {
  const sourceKeys = new Set<string>();
  const cohortKeys = new Set<string>();
  let hasNameIdentity = false;
  let hasEmailIdentity = false;
  let hasPhoneIdentity = false;
  let hasGeneratedRegistrationIdentity = false;
  let hasRowsWithoutNotes = false;
  let hasHiddenDefaultOrEmptySource = false;
  let hasHiddenAcquisitionContext = false;
  const hasRecordDisambiguatorSearch =
    getRegistrationIdsRequiringActionRecordDisambiguator(registrations).size > 0;
  const noteKeys = new Set<string>();
  const acquisitionContextKeys = new Set<string>();

  registrations.forEach((reg) => {
    const name = normalizeRegistrationNameValue(reg.crFullName);
    const hasName = Boolean(name);
    const email = normalizeRegistrationContactValue(reg.crEmail);
    const phone = normalizeRegistrationContactValue(reg.crPhoneE164);
    const hasEmail = Boolean(email);
    const hasPhone = Boolean(phone);
    const hasContact = hasEmail || hasPhone;
    const hasDistinctEmail = hasEmail && (!hasName || !contactComparisonValuesMatch(name, email));
    const hasDistinctPhone = hasPhone && (!hasName || !contactComparisonValuesMatch(name, phone));

    if (hasName) hasNameIdentity = true;
    if (hasDistinctEmail) hasEmailIdentity = true;
    if (hasDistinctPhone) hasPhoneIdentity = true;
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

  const identityTerms: string[] = [];
  if (hasNameIdentity) identityTerms.push('Nombre');
  if (hasEmailIdentity || hasPhoneIdentity) {
    const contactTerm = hasEmailIdentity && hasPhoneIdentity
      ? 'contacto'
      : hasEmailIdentity
        ? 'correo'
        : 'teléfono';
    const capitalizedContactTerm = `${contactTerm.charAt(0).toLocaleUpperCase('es')}${contactTerm.slice(1)}`;
    identityTerms.push(hasNameIdentity ? contactTerm : capitalizedContactTerm);
  }

  const contextTerms: string[] = [];
  if (noteKeys.size > 1 || (noteKeys.size === 1 && hasRowsWithoutNotes)) contextTerms.push('nota');
  const hasVariableAcquisitionContext =
    acquisitionContextKeys.size > 1 || (acquisitionContextKeys.size === 1 && hasHiddenAcquisitionContext);
  const hasVariableSource =
    sourceKeys.size > 1 || (sourceKeys.size === 1 && hasHiddenDefaultOrEmptySource);
  if (hasVariableAcquisitionContext) {
    contextTerms.push(identityTerms.length === 0 && !hasGeneratedRegistrationIdentity ? 'Origen' : 'origen');
  }
  if (hasSearchableCustomRegistrationStatus(registrations)) contextTerms.push('estado');
  if (hasVariableSource && !hasVariableAcquisitionContext) contextTerms.push('fuente');
  if (includeCourseTerm && cohortKeys.size > 1) contextTerms.push('curso');

  const visibleContextTerms = contextTerms.length > 1 ? ['otros datos'] : contextTerms;

  const hasRecordSearchTerm = hasGeneratedRegistrationIdentity || hasRecordDisambiguatorSearch;

  if (
    hasRecordSearchTerm
    && identityTerms.length > 0
    && identityTerms.length + 1 + contextTerms.length > MAX_LOCAL_SEARCH_PLACEHOLDER_TERMS
  ) {
    return formatLocalSearchPlaceholder([...identityTerms, 'registro', 'otros datos']);
  }

  const terms = [...identityTerms];
  if (hasRecordSearchTerm) terms.push(terms.length === 0 ? 'Número de registro' : 'número de registro');
  terms.push(...visibleContextTerms);

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

const preferContactText = (primary?: string | null, fallback?: string | null) =>
  normalizeRegistrationContactValue(primary) ?? normalizeRegistrationContactValue(fallback);

const preferMeaningfulRegistrationSource = (primary?: string | null, fallback?: string | null) => {
  const trimmedPrimary = primary?.trim() ?? '';
  const trimmedFallback = fallback?.trim() ?? '';

  if (!trimmedPrimary) return trimmedFallback || null;
  if (!trimmedFallback) return trimmedPrimary;

  const primaryHasMeaningfulSource = Boolean(getSearchableRegistrationSource(trimmedPrimary));
  const fallbackHasMeaningfulSource = Boolean(getSearchableRegistrationSource(trimmedFallback));

  if (!primaryHasMeaningfulSource && fallbackHasMeaningfulSource) {
    return trimmedFallback;
  }

  return trimmedPrimary;
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
  crEmail: preferContactText(primary.crEmail, fallback.crEmail),
  crPhoneE164: preferContactText(primary.crPhoneE164, fallback.crPhoneE164),
  crStatus: preferNonEmptyText(primary.crStatus, fallback.crStatus) ?? primary.crStatus,
  crSource: preferMeaningfulRegistrationSource(primary.crSource, fallback.crSource),
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

const mergeCourseRegistrationFollowUpRecords = (
  primary: CourseRegistrationFollowUpDTO,
  fallback: CourseRegistrationFollowUpDTO,
): CourseRegistrationFollowUpDTO => ({
  ...primary,
  crfRegistrationId: preferPositiveId(primary.crfRegistrationId, fallback.crfRegistrationId) ?? primary.crfRegistrationId,
  crfPartyId: preferPositiveId(primary.crfPartyId, fallback.crfPartyId),
  crfEntryType: preferNonEmptyText(primary.crfEntryType, fallback.crfEntryType) ?? primary.crfEntryType,
  crfSubject: preferNonEmptyText(primary.crfSubject, fallback.crfSubject),
  crfNotes: preferNonEmptyText(primary.crfNotes, fallback.crfNotes) ?? primary.crfNotes,
  crfAttachmentUrl: preferNonEmptyText(primary.crfAttachmentUrl, fallback.crfAttachmentUrl),
  crfAttachmentName: preferNonEmptyText(primary.crfAttachmentName, fallback.crfAttachmentName),
  crfNextFollowUpAt: preferNonEmptyText(primary.crfNextFollowUpAt, fallback.crfNextFollowUpAt),
  crfCreatedBy: preferPositiveId(primary.crfCreatedBy, fallback.crfCreatedBy),
  crfCreatedAt: preferNonEmptyText(primary.crfCreatedAt, fallback.crfCreatedAt) ?? primary.crfCreatedAt,
  crfUpdatedAt: preferNonEmptyText(primary.crfUpdatedAt, fallback.crfUpdatedAt) ?? primary.crfUpdatedAt,
});

const dedupeCourseRegistrationFollowUps = (followUps: readonly CourseRegistrationFollowUpDTO[]) => {
  const followUpsById = new Map<number, CourseRegistrationFollowUpDTO>();

  followUps.forEach((entry) => {
    const existingEntry = followUpsById.get(entry.crfId);
    if (!existingEntry) {
      followUpsById.set(entry.crfId, entry);
      return;
    }

    followUpsById.set(entry.crfId, mergeCourseRegistrationFollowUpRecords(existingEntry, entry));
  });

  return [...followUpsById.values()];
};

const mergeCourseEmailEventRecords = (
  primary: CourseEmailEventDTO,
  fallback: CourseEmailEventDTO,
): CourseEmailEventDTO => ({
  ...primary,
  ceCourseSlug: preferNonEmptyText(primary.ceCourseSlug, fallback.ceCourseSlug) ?? primary.ceCourseSlug,
  ceRegistrationId: preferPositiveId(primary.ceRegistrationId, fallback.ceRegistrationId),
  ceRecipientEmail: preferNonEmptyText(primary.ceRecipientEmail, fallback.ceRecipientEmail) ?? primary.ceRecipientEmail,
  ceRecipientName: preferNonEmptyText(primary.ceRecipientName, fallback.ceRecipientName),
  ceEventType: preferNonEmptyText(primary.ceEventType, fallback.ceEventType) ?? primary.ceEventType,
  ceStatus: preferNonEmptyText(primary.ceStatus, fallback.ceStatus) ?? primary.ceStatus,
  ceMessage: preferNonEmptyText(primary.ceMessage, fallback.ceMessage),
  ceCreatedAt: preferNonEmptyText(primary.ceCreatedAt, fallback.ceCreatedAt) ?? primary.ceCreatedAt,
});

const dedupeCourseEmailEvents = (events: readonly CourseEmailEventDTO[]) => {
  const eventsById = new Map<number, CourseEmailEventDTO>();

  events.forEach((event) => {
    const existingEvent = eventsById.get(event.ceId);
    if (!existingEvent) {
      eventsById.set(event.ceId, event);
      return;
    }

    eventsById.set(event.ceId, mergeCourseEmailEventRecords(existingEvent, event));
  });

  return [...eventsById.values()];
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
      bySlug.set(selectedSlug, readableFirstRunCohortFallbackLabel(selectedSlug));
    }
    return bySlug;
  }, [cohortsQuery.data, selectedSlug]);
  const cohortSummaryLabelsBySlug = useMemo(() => {
    const bySlug = new Map<string, string>();
    for (const cohort of cohortsQuery.data ?? []) {
      const cohortSlug = cohort.ccSlug.trim();
      if (!cohortSlug || bySlug.has(cohortSlug)) continue;
      bySlug.set(cohortSlug, cohortSummaryLabel(cohort));
    }
    if (selectedSlug && !bySlug.has(selectedSlug)) {
      bySlug.set(selectedSlug, readableFirstRunCohortFallbackLabel(selectedSlug));
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
  const hasMultipleAvailableCohorts = !cohortsQuery.isError && configuredCohortOptions.length > 1;

  const activeCohortLabel = selectedSlug
    ? (
      cohortSummaryLabelsBySlug.get(selectedSlug)
      ?? cohortLabelsBySlug.get(selectedSlug)
      ?? readableCohortFallbackLabel(selectedSlug)
    )
    : '';

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
    const summaryLabel = cohortSummaryLabelsBySlug.get(singleAvailableCohort.value) ?? singleAvailableCohort.label;
    if (!hasVisibleRegistrations || selectedSlug === singleAvailableCohort.value) {
      return summaryLabel;
    }
    return visibleCohortSlugs.size === 1 && visibleCohortSlugs.has(singleAvailableCohort.value)
      ? summaryLabel
      : '';
  }, [cohortSummaryLabelsBySlug, hasVisibleRegistrations, selectedSlug, singleAvailableCohort, visibleCohortSlugs]);

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
  const hasVisibleCustomStatuses = useMemo(
    () => registrations.some((reg) => !normalizeKnownRegistrationStatus(reg.crStatus)),
    [registrations],
  );
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
    if (hasVisibleCustomStatuses) return null;
    if (!hasVisibleRegistrations) return null;
    const realStatuses = visibleStatusFilters.filter((value): value is Exclude<StatusFilter, 'all'> => value !== 'all');
    return realStatuses.length === 1 ? (realStatuses[0] ?? null) : null;
  }, [hasVisibleCustomStatuses, hasVisibleRegistrations, visibleStatusFilters]);
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
  const hasUnconfiguredSlugFilter = hasSlugFilter
    && !cohortsQuery.isLoading
    && !cohortsQuery.isError
    && selectedConfiguredCohort == null;
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
  const showUnconfiguredCourseFirstRunLimitEmptyState = !hasVisibleRegistrations
    && !cohortsQuery.isLoading
    && !cohortsQuery.isError
    && configuredCohortOptions.length === 0
    && hasCustomLimit
    && !hasManualFilters;
  const showUnconfiguredCourseFirstRunFilteredEmptyState = !hasVisibleRegistrations
    && !cohortsQuery.isLoading
    && !cohortsQuery.isError
    && configuredCohortOptions.length === 0
    && !hasSlugFilter
    && hasStatusFilter;
  const showMultiCohortFirstRunLimitEmptyState = !hasVisibleRegistrations
    && hasMultipleAvailableCohorts
    && hasCustomLimit
    && !hasManualFilters;
  const showCohortFilterUnavailableSummary = cohortsQuery.isError && hasVisibleRegistrations && !hasSlugFilter;
  const activeFilterSummary = useMemo(
    () => summarizeActiveFilters({
      cohortLabel: hasEffectiveSlugFilter ? activeCohortLabel : '',
      status,
      limit,
    }),
    [activeCohortLabel, hasEffectiveSlugFilter, status, limit],
  );
  const showSingleCustomStatusSummary = singleVisibleCustomStatus != null && actionableStatusFilters.length === 0;
  const combinedSingleChoiceStatusLabel = singleAvailableCohortLabel
    ? showSingleStatusSummary && singleVisibleStatus
      ? statusFilterLabels[singleVisibleStatus]
      : showSingleCustomStatusSummary && singleVisibleCustomStatus != null
        ? customRegistrationStatusLabel(singleVisibleCustomStatus)
        : ''
    : '';
  const combinedSingleChoiceSummary = singleAvailableCohortLabel && combinedSingleChoiceStatusLabel
    ? `${singleAvailableCohortLabel} · ${combinedSingleChoiceStatusLabel}`
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
        normalizeRegistrationNameValue(reg.crFullName),
        normalizeRegistrationContactValue(reg.crEmail),
        `registro #${reg.crId}`,
        String(reg.crId),
        reg.crAdminNotes,
        courseSlug,
        cohortLabelsBySlug.get(courseSlug),
        ...registrationStatusSearchValues(reg.crStatus),
        ...registrationContactStateSearchValues(reg),
        getSearchableRegistrationSource(reg.crSource),
        getSearchableRegistrationAcquisitionContext(reg),
      ].join(' ');
      if (localSearchTextMatches(haystack, localSearchKey)) return true;

      if (localSearchDigitsKey.length >= MIN_PHONE_SEARCH_DIGITS) {
        return phoneDigitsMatchLocalSearch(reg.crPhoneE164, localSearchDigitsKey);
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
    return cohortSummaryLabelsBySlug.get(cohortSlug)
      ?? cohortLabelsBySlug.get(cohortSlug)
      ?? readableCohortFallbackLabel(cohortSlug);
  }, [cohortLabelsBySlug, cohortSummaryLabelsBySlug, searchedRegistrations, selectedSlug]);
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
  const singleSearchedKnownStatus = useMemo<RegistrationStatus | null>(() => {
    if (searchedRegistrations.length < 2) return null;
    const statusKeys = new Set<RegistrationStatus>();

    searchedRegistrations.forEach((reg) => {
      const knownStatus = normalizeKnownRegistrationStatus(reg.crStatus.trim());
      if (knownStatus) statusKeys.add(knownStatus);
    });

    const [onlyStatus] = Array.from(statusKeys.values());
    return statusKeys.size === 1 && onlyStatus ? onlyStatus : null;
  }, [searchedRegistrations]);
  const sharedVisibleSourceSummary = hasNamedVisibleSource
    ? `Fuente visible: ${singleVisibleSourceLabel}.`
    : '';
  const showEmptyLocalSearchResults = hasLocalSearch
    && loadedRegistrationCount > 0
    && searchedRegistrations.length === 0;
  const hidePassiveFiltersDuringEmptyLocalSearch = showEmptyLocalSearchResults
    && !hasManualFilters
    && !cohortsQuery.isError;
  const showDefaultEmptyLocalSearchFocus = showEmptyLocalSearchResults
    && !hasCustomFilters
    && !viewHitsCurrentLimit;
  const showCappedEmptyLocalSearchLimitEditor = showEmptyLocalSearchResults
    && viewHitsCurrentLimit
    && showAdvancedFilters;
  const showFilteredEmptyLocalSearchFocus = showEmptyLocalSearchResults
    && hasManualFilters
    && !cohortsQuery.isError
    && Boolean(activeFilterSummary)
    && !showCappedEmptyLocalSearchLimitEditor;
  const showFocusedEmptyLocalSearchState = showDefaultEmptyLocalSearchFocus || showFilteredEmptyLocalSearchFocus;
  const defaultEmptyLocalSearchScopeSummary = showDefaultEmptyLocalSearchFocus
    ? [
      combinedSingleChoiceSummary
        || singleAvailableCohortLabel
        || (showSingleStatusSummary && singleVisibleStatus ? statusFilterLabels[singleVisibleStatus] : ''),
      hasNamedVisibleSource ? `Fuente visible: ${singleVisibleSourceLabel}` : '',
    ].filter(Boolean).join(' · ')
    : '';
  const filteredEmptyLocalSearchScopeSummary = showFilteredEmptyLocalSearchFocus
    ? `Vista filtrada: ${activeFilterSummary}`
    : '';
  const emptyLocalSearchScopeSummary = filteredEmptyLocalSearchScopeSummary || defaultEmptyLocalSearchScopeSummary;
  const localSearchNarrowsRegistrations = hasLocalSearch && searchedRegistrations.length < loadedRegistrationCount;
  const singleVisibleRegistrationNeedsContact = searchedRegistrations.length === 1
    && searchedRegistrations[0] != null
    && registrationNeedsContact(searchedRegistrations[0]);
  const singleVisibleMissingContactSummary = singleVisibleRegistrationNeedsContact
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
  const hasSearchablePhoneContacts = useMemo(
    () => registrations.some((reg) => Boolean(normalizeRegistrationContactValue(reg.crPhoneE164))),
    [registrations],
  );
  const shortPhoneSearchHint = hasSearchablePhoneContacts
    ? shortPhoneSearchHintFor(localSearchTerm, localSearchDigitsKey)
    : '';
  const showEmptyLocalSearchLimitGuidance = showEmptyLocalSearchResults
    && viewHitsCurrentLimit
    && !shortPhoneSearchHint;
  const showEmptyLocalSearchLimitRecoveryAction = showEmptyLocalSearchLimitGuidance
    && !showAdvancedFilters;
  const showLocalSearchControl = loadedRegistrationCount >= MIN_LOCAL_SEARCH_REGISTRATIONS || Boolean(localSearchKey);
  const showBusyListSearchOnboarding = showLocalSearchControl && !hasLocalSearch;
  const statusFiltersSummarizeBusyListRows = showBusyListSearchOnboarding
    && status === 'all'
    && actionableStatusFilters.length > 1
    && !hasVisibleCustomStatuses;
  const showCohortSelect = !combinedSingleChoiceSummary && !singleAvailableCohortLabel;
  const hasDedicatedCohortFilterControl = showCohortSelect
    && !cohortsQuery.isError
    && cohortOptions.length > 1;
  const hasDedicatedCohortFilterPath = showCohortSelect
    && !cohortsQuery.isError
    && (cohortsQuery.isLoading || cohortOptions.length > 1);
  const localSearchPlaceholder = useMemo(
    () => buildLocalSearchPlaceholder(registrations, {
      includeCourseTerm: !hasDedicatedCohortFilterPath,
    }),
    [hasDedicatedCohortFilterPath, registrations],
  );
  const localSearchInputTitle = localSearchPlaceholder.includes('otros datos')
    ? LOCAL_SEARCH_COMPACT_CONTEXT_TITLE
    : undefined;
  const hasCustomStatusSearch = useMemo(
    () => hasSearchableCustomRegistrationStatus(registrations),
    [registrations],
  );
  const statusAlreadyVisibleInBusySearchOnboarding = Boolean(combinedSingleChoiceSummary)
    && showLocalSearchControl
    && !hasLocalSearch
    && !hasCustomFilters
    && !viewHitsCurrentLimit
    && limit === DEFAULT_LIMIT;
  const allVisibleRowsCanOpenPaymentWorkflow = searchedRegistrations.length > 0
    && searchedRegistrations.every((reg) => canOpenPaymentWorkflowFromStatus(reg.crStatus));
  const allVisibleRowsUseBusyListPaidRecoveryAction = statusAlreadyVisibleInBusySearchOnboarding
    && searchedRegistrations.length > 0
    && searchedRegistrations.every((reg) => normalizeKnownRegistrationStatus(reg.crStatus) === 'paid');
  const allVisibleRowsUseDirectPendingRecoveryAction = searchedRegistrations.length > 0
    && searchedRegistrations.every((reg) => shouldUseDirectPendingRecoveryAction(
      reg.crStatus,
      allVisibleRowsUseBusyListPaidRecoveryAction,
    ));
  const allVisibleRowsUseCustomStatusActions = searchedRegistrations.length > 0
    && searchedRegistrations.every((reg) => !normalizeKnownRegistrationStatus(reg.crStatus));
  const localSearchOnboardingActionText = allVisibleRowsUseDirectPendingRecoveryAction
    ? allVisibleRowsUseBusyListPaidRecoveryAction
      ? buildPaidRecoveryScopeHint(dossierIdentityTargetLabel)
      : buildPendingRecoveryScopeHint(dossierIdentityTargetLabel)
    : allVisibleRowsCanOpenPaymentWorkflow
      ? buildPaymentWorkflowScopeHint(dossierIdentityTargetLabel)
      : allVisibleRowsUseCustomStatusActions
        ? buildCustomStatusNormalizationScopeHint(dossierIdentityTargetLabel)
      : statusAlreadyVisibleInBusySearchOnboarding || statusFiltersSummarizeBusyListRows
        ? buildCompactDossierScopeHint(dossierIdentityTargetLabel)
        : buildDossierOnlyScopeHint(dossierIdentityTargetLabel);
  const localSearchOnboardingActionHint = showFilterOnboardingCopy
    ? ` ${localSearchOnboardingActionText}`
    : '';
  const localSearchSingleResult =
    localSearchNarrowsRegistrations && searchedRegistrations.length === 1
      ? (searchedRegistrations[0] ?? null)
      : null;
  const localSearchSingleResultKnownStatus = localSearchSingleResult
    ? normalizeKnownRegistrationStatus(localSearchSingleResult.crStatus)
    : null;
  const singleResultUnrelatedStatusFilterCount = localSearchSingleResultKnownStatus
    ? actionableStatusFilters.filter((value) => value !== localSearchSingleResultKnownStatus).length
    : 0;
  const hideSingleResultLocalSearchPassiveFilterPanel = Boolean(
    localSearchSingleResult
    && !hasCustomFilters
    && !cohortsQuery.isError
    && singleResultUnrelatedStatusFilterCount > 0,
  );
  const localSearchSingleResultUsesDirectPaidRecovery =
    localSearchSingleResultKnownStatus === 'paid';
  const localSearchSingleResultTargetLabel = localSearchSingleResult
    ? registrationIdentityTargetLabel(searchedRegistrations)
    : dossierIdentityTargetLabel;
  const localSearchSingleResultActionHint = localSearchSingleResult
    ? ` ${
      localSearchSingleResultKnownStatus === 'cancelled'
        ? buildPendingRecoveryScopeHint(localSearchSingleResultTargetLabel)
        : localSearchSingleResultUsesDirectPaidRecovery
          ? buildPaidRecoveryScopeHint(localSearchSingleResultTargetLabel)
        : canOpenPaymentWorkflowFromStatus(localSearchSingleResult.crStatus)
          ? buildPaymentWorkflowScopeHint(localSearchSingleResultTargetLabel)
        : !localSearchSingleResultKnownStatus
          ? buildCustomStatusNormalizationScopeHint(localSearchSingleResultTargetLabel)
          : buildDossierOnlyScopeHint(localSearchSingleResultTargetLabel)
    }`
    : '';
  const loadedSearchScopeHint = buildLoadedSearchScopeHint(loadedRegistrationCount);
  const localSearchLoadedScopeHint = hasCustomFilters
    ? `${loadedSearchScopeHint.replace(/\.$/, '')} sin cambiar filtros.`
    : loadedSearchScopeHint;
  const baseLocalSearchHelperText = localSearchKey
    ? showEmptyLocalSearchResults
      ? undefined
      : localSearchNarrowsRegistrations
        ? [
          formatLocalSearchResultSummary(searchedRegistrations.length, loadedRegistrationCount),
          singleVisibleMissingContactSummary,
          hiddenLocalSearchMatchSummary,
          localSearchSingleResultActionHint.trim(),
        ].filter(Boolean).join(' ')
        : loadedRegistrationCount > 0
          ? [
            buildFullLocalSearchMatchHint(loadedRegistrationCount),
            hiddenLocalSearchMatchSummary,
          ].filter(Boolean).join(' ')
          : undefined
    : viewHitsCurrentLimit
      ? `${buildLoadedSearchScopeHint(loadedRegistrationCount)}${localSearchOnboardingActionHint}`
      : `${localSearchLoadedScopeHint}${localSearchOnboardingActionHint}`;
  const emptyLocalSearchResultsMessage = showEmptyLocalSearchResults
    ? [
      emptyLocalSearchScopeSummary ? `${emptyLocalSearchScopeSummary}.` : '',
      shortPhoneSearchHint
        || `No hay coincidencias para "${localSearchSummary}" en las ${formatRegistrationCountLabel(loadedRegistrationCount)} cargadas.`,
      shortPhoneSearchHint || showEmptyLocalSearchLimitRecoveryAction
        ? ''
        : viewHitsCurrentLimit
          ? cappedLocalSearchEmptyHint
          : '',
    ].filter(Boolean).join(' ')
    : '';
  const emptyLocalSearchResultsAccessibleLabel = showEmptyLocalSearchResults
    && !shortPhoneSearchHint
    && localSearchSummary !== localSearchTerm
    ? [
      emptyLocalSearchScopeSummary ? `${emptyLocalSearchScopeSummary}.` : '',
      `No hay coincidencias para "${localSearchTerm}" en las ${formatRegistrationCountLabel(loadedRegistrationCount)} cargadas.`,
      showEmptyLocalSearchLimitRecoveryAction
        ? ''
        : viewHitsCurrentLimit
          ? cappedLocalSearchEmptyHint
          : '',
    ].filter(Boolean).join(' ')
    : undefined;
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
  const combinedSingleChoicePassiveContextSummary = [
    combinedSingleChoiceSourceSummary,
    combinedSingleChoiceLimitSummary,
    combinedSingleChoiceCountSummary,
  ].filter(Boolean).join(' ');
  const combinedSingleChoiceCustomStatusGuidance = combinedSingleChoiceSummary && showSingleCustomStatusSummary
    ? customStatusFilterUnavailableMessage
    : '';
  const showSingleStatusSummaryBlock = showSingleStatusSummary
    && !(showCohortFilterUnavailableSummary && loadedRegistrationCount === 1);
  const standaloneSingleChoiceInlineSourceSummary = !combinedSingleChoiceSummary
    && (singleAvailableCohortLabel || showSingleStatusSummaryBlock)
    && hasNamedVisibleSource
    ? `Fuente: ${singleVisibleSourceLabel}`
    : '';
  const standaloneSingleChoiceBusySearchSourceSummary = standaloneSingleChoiceInlineSourceSummary
    ? `Fuente visible: ${singleVisibleSourceLabel}`
    : '';
  const resetViewLabel = getResetViewLabel({
    hasCustomLimit,
    hasSlugFilter: hasEffectiveSlugFilter,
    hasStatusFilter,
    hasUnconfiguredSlugFilter,
  });
  const showCohortFilterLoadingSummary = showCohortSelect
    && cohortsQuery.isLoading
    && hasVisibleRegistrations
    && !hasSlugFilter;
  const hideBusyListPassiveCohortLoadingSummary = showBusyListSearchOnboarding
    && showCohortFilterLoadingSummary
    && !hasCustomFilters
    && !hasVisibleCustomStatuses
    && actionableStatusFilters.length <= 1;
  const hideBusyListPassiveCohortUnavailableSummary = showBusyListSearchOnboarding
    && showCohortFilterUnavailableSummary
    && !hasCustomFilters
    && !hasVisibleCustomStatuses
    && actionableStatusFilters.length <= 1;
  const showEmptyCohortFilterSummary = showCohortSelect
    && !cohortsQuery.isLoading
    && !cohortsQuery.isError
    && hasVisibleRegistrations
    && cohortOptions.length === 0
    && !hasSlugFilter;
  const cohortFilterCanSelfReset = showCohortSelect && hasSlugFilter && !hasStatusFilter && !hasCustomLimit;
  const filteredEmptyStateRecoveryHint = hasManualFilters || cohortsQuery.isError
    ? ''
    : 'Usa refrescar si esperabas resultados.';
  const filteredEmptyStateScope = hasManualFilters
    ? hasCustomLimit
      ? 'en la vista actual'
      : 'con los filtros actuales'
    : 'con el límite actual';
  const filteredEmptyStateBaseMessage =
    hasUnconfiguredSlugFilter && !hasStatusFilter && !hasCustomLimit
      ? `No hay inscripciones para ${activeCohortLabel}.`
      : !hasManualFilters && hasCustomLimit
      ? `No hay inscripciones con el límite actual de hasta ${limit} inscripci${limit === 1 ? 'ón' : 'ones'}.`
      : activeFilterSummary
        ? `No hay inscripciones ${filteredEmptyStateScope}: ${activeFilterSummary}.`
        : `No hay inscripciones ${filteredEmptyStateScope}.`;
  const filteredEmptyStateMessage = [
    filteredEmptyStateBaseMessage,
    filteredEmptyStateRecoveryHint,
  ].filter(Boolean).join(' ');
  const hasTinyLimitOnlyView = hasCustomLimit
    && !hasManualFilters
    && !hasLocalSearch
    && loadedRegistrationCount > 1
    && loadedRegistrationCount <= MIN_DEFAULT_CSV_EXPORT_ROWS;
  const hasTinyManualFilterView = hasManualFilters
    && !hasLocalSearch
    && searchedRegistrations.length > 1
    && searchedRegistrations.length < MIN_DEFAULT_CSV_EXPORT_ROWS;
  const hasBroadLocalSearch = hasLocalSearch && !localSearchNarrowsRegistrations;
  const hasExplicitCsvExportScope = !hasBroadLocalSearch && (hasManualFilters
    ? !hasTinyManualFilterView
    : localSearchNarrowsRegistrations || (hasCustomLimit && !hasTinyLimitOnlyView));
  const canCopyCsv = searchedRegistrations.length > 1 && hasExplicitCsvExportScope;
  const showCopyCsvAction = canCopyCsv && !copyMessage;
  const showLocalSearchInlineClearAction = hasLocalSearch && !showEmptyLocalSearchResults;
  const showEmptyLocalSearchAlertClearAction = showEmptyLocalSearchResults
    && !showLocalSearchInlineClearAction;
  const showLocalSearchUtilityRow = hasLocalSearch && localSearchNarrowsRegistrations && (
    showCopyCsvAction
    || Boolean(copyMessage)
  );
  const showScopedCopyCsvAction = showCopyCsvAction && !showLocalSearchUtilityRow;
  const showScopedCopyMessage = Boolean(copyMessage) && !showLocalSearchUtilityRow;
  const hideTinyDefaultListRowDates = !hasCustomFilters && loadedRegistrationCount < MIN_DEFAULT_CSV_EXPORT_ROWS;
  const shouldShowSharedCohortSummary = Boolean(singleVisibleCohortLabel)
    && !singleAvailableCohortLabel
    && !hasEffectiveSlugFilter;
  const hasSharedVisibleSource = Boolean(singleVisibleSourceLabel);
  const shouldShowSharedSourceSummary = hasNamedVisibleSource
    && !combinedSingleChoiceSourceSummary
    && !standaloneSingleChoiceInlineSourceSummary;
  const activeStatusFilterIsOnlyStatusOption = hasStatusFilter
    && actionableStatusFilters.length === 1
    && actionableStatusFilters[0] === status;
  const showActiveStatusFilterSummary = hasVisibleRegistrations
    && hasStatusFilter
    && !hasVisibleCustomStatuses
    && (
      hasEffectiveSlugFilter
      || hasCustomLimit
      || loadedRegistrationCount === 1
      || activeStatusFilterIsOnlyStatusOption
    );
  const statusAlreadyVisibleInFilterStrip = hasStatusFilter && !showSingleStatusSummary && !showActiveStatusFilterSummary;
  const shouldShowSharedStatusSummary = Boolean(singleSearchedStatusLabel)
    && !showSingleStatusSummary
    && !statusAlreadyVisibleInFilterStrip
    && !showActiveStatusFilterSummary
    && !showSingleCustomStatusSummary;
  const sharedVisibleStatusSummary = shouldShowSharedStatusSummary
    ? `Estado visible: ${singleSearchedStatusLabel}.`
    : '';
  const visibleRegistrationsMissingContactCount =
    searchedRegistrations.filter(registrationNeedsContact).length;
  const sharedVisibleMissingContactSummary = formatVisibleMissingContactSummary(
    visibleRegistrationsMissingContactCount,
    searchedRegistrations.length,
  );
  const showSharedVisibleMissingContactSummary = Boolean(sharedVisibleMissingContactSummary)
    && !showBusyListSearchOnboarding;
  const sharedVisibleCreatedAtLabel = useMemo(() => {
    if (searchedRegistrations.length < 2) return '';
    const createdLabels = searchedRegistrations.map((reg) => formatOptionalDate(reg.crCreatedAt));
    if (createdLabels.some((label) => label === '')) return '';
    const [firstLabel] = createdLabels;
    return firstLabel && createdLabels.every((label) => label === firstLabel) ? firstLabel : '';
  }, [searchedRegistrations]);
  const sharedVisibleCreatedAtSummary = sharedVisibleCreatedAtLabel
    && localSearchNarrowsRegistrations
    ? `Misma fecha de registro: ${sharedVisibleCreatedAtLabel}.`
    : '';
  const shouldHideSharedCreatedAtContext = Boolean(sharedVisibleCreatedAtLabel)
    && (hasCustomFilters || searchedRegistrations.length > 1);
  const allVisibleRegistrationsHaveNotes = searchedRegistrations.length > 1
    && searchedRegistrations.every((reg) => Boolean(reg.crAdminNotes?.trim()));
  const sharedVisibleNotesSummary = allVisibleRegistrationsHaveNotes && !hiddenLocalSearchMatchSummary
    ? 'Notas internas en todas las inscripciones visibles.'
    : '';
  const foldSharedNotesIntoBusySearch = showBusyListSearchOnboarding
    && Boolean(sharedVisibleNotesSummary)
    && !hasCustomFilters
    && !viewHitsCurrentLimit
    && limit === DEFAULT_LIMIT;
  const showSharedNotesSummaryInList = Boolean(sharedVisibleNotesSummary)
    && !foldSharedNotesIntoBusySearch;
  const sharedListContextSummaries = [
    sharedVisibleStatusSummary,
    shouldShowSharedCohortSummary ? `Mostrando una sola cohorte: ${singleVisibleCohortLabel}.` : '',
    shouldShowSharedSourceSummary ? `Fuente visible: ${singleVisibleSourceLabel}.` : '',
    sharedVisibleCreatedAtSummary,
    showSharedVisibleMissingContactSummary ? sharedVisibleMissingContactSummary : '',
    showSharedNotesSummaryInList ? sharedVisibleNotesSummary : '',
  ].filter(Boolean);
  const combinedSharedListContextSummary = sharedListContextSummaries.length > 1
    ? sharedListContextSummaries.join(' ')
    : '';
  const copyCsvButtonLabel = showLocalSearchUtilityRow
    ? 'Copiar CSV'
    : `Copiar CSV (${formatCsvRegistrationCountLabel(searchedRegistrations.length)})`;
  const copyCsvButtonAccessibleLabel =
    `Copiar ${formatCsvRegistrationCountLabel(searchedRegistrations.length)} visibles como CSV`;
  const copyCsvButtonTitle = showLocalSearchUtilityRow
    ? 'Copia solo los resultados visibles de la búsqueda.'
    : 'Copia solo las inscripciones visibles de esta vista.';
  const visibleCsvScopeKey = useMemo(
    () => JSON.stringify(
      searchedRegistrations.map((reg) => [
        reg.crId,
        reg.crCourseSlug,
        reg.crFullName ?? '',
        reg.crEmail ?? '',
        reg.crPhoneE164 ?? '',
        reg.crStatus,
        reg.crCreatedAt,
        reg.crUpdatedAt,
      ]),
    ),
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
    && !hasTinyManualFilterView
    && !hasTinyLimitOnlyView
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
  const showLimitAdjustmentAction = showAdvancedLimitControl && (
    viewHitsCurrentLimit
    || showAdvancedFilters
  );
  const hideCohortFilterForSingleLocalSearchResult = Boolean(localSearchSingleResult)
    && !hasCustomFilters
    && !showAdvancedLimitControl
    && !cohortsQuery.isError;
  const showSingleResultWithoutHiddenLimit = loadedRegistrationCount === 1 && !showAdvancedLimitControl;
  const showSingleResultWithOnlyPassiveFilterContext = showSingleResultWithoutHiddenLimit
    && !hasCustomFilters
    && !hasEffectiveSlugFilter
    && !showCohortFilterUnavailableSummary
    && (Boolean(combinedSingleChoiceSummary) || showSingleStatusSummary);
  const showBusyListNonNarrowingSearch = showLocalSearchControl
    && hasLocalSearch
    && !localSearchNarrowsRegistrations
    && loadedRegistrationCount > 1;
  const hideSingleResultLocalSearchPassiveCurrentView = Boolean(
    localSearchSingleResult
    && combinedSingleChoiceSummary
    && !combinedSingleChoiceSourceSummary
    && !hasCustomFilters
    && !showAdvancedLimitControl,
  );
  const showSingleStatusSummaryInPageChrome = showSingleStatusSummaryBlock
    && !showSingleResultWithOnlyPassiveFilterContext
    && !hideSingleResultLocalSearchPassiveCurrentView;
  const localSearchSharedPaidStatusSummary = hasLocalSearch
    && shouldShowSharedStatusSummary
    && singleSearchedKnownStatus === 'paid';
  const localSearchSharedPendingPaymentStatusSummary = hasLocalSearch
    && shouldShowSharedStatusSummary
    && singleSearchedKnownStatus === 'pending_payment';
  const singleStatusSummarySupportsPaidRecovery =
    showSingleStatusSummaryInPageChrome && !showCohortFilterUnavailableSummary;
  const useCompactStatusActionLabel = showSingleStatusSummaryInPageChrome
    || statusAlreadyVisibleInBusySearchOnboarding
    || statusFiltersSummarizeBusyListRows
    || statusAlreadyVisibleInFilterStrip
    || showActiveStatusFilterSummary
    || showSingleCustomStatusSummary
    || shouldShowSharedStatusSummary;
  const shouldCompactRepeatedPaymentStatusActions = searchedRegistrations.length >= 2;
  const showRepeatedPaymentStatusIconActions = Boolean(
    combinedSingleChoiceSummary
    || showSingleStatusSummaryInPageChrome
    || showActiveStatusFilterSummary
    || localSearchSharedPendingPaymentStatusSummary
  )
    && (!hasLocalSearch || localSearchSharedPendingPaymentStatusSummary)
    && !showBusyListSearchOnboarding
    && allVisibleRowsCanOpenPaymentWorkflow
    && useCompactStatusActionLabel
    && shouldCompactRepeatedPaymentStatusActions;
  const hasSharedPendingPaymentStatusContext = Boolean(combinedSingleChoiceSummary)
    || showSingleStatusSummaryInPageChrome;
  const showInlinePaymentWorkflowRowLabel = hasSharedPendingPaymentStatusContext
    && !hasLocalSearch
    && !showBusyListSearchOnboarding
    && allVisibleRowsCanOpenPaymentWorkflow
    && !showRepeatedPaymentStatusIconActions
    && useCompactStatusActionLabel
    && searchedRegistrations.length > 1;
  const allVisibleRowsUsePaidRecoveryAction = searchedRegistrations.length > 0
    && searchedRegistrations.every((reg) => normalizeKnownRegistrationStatus(reg.crStatus) === 'paid')
    && (
      allVisibleRowsUseBusyListPaidRecoveryAction
      || showActiveStatusFilterSummary
      || localSearchSharedPaidStatusSummary
      || (
        localSearchSingleResultUsesDirectPaidRecovery
        && searchedRegistrations.length === 1
      )
      || singleStatusSummarySupportsPaidRecovery
    );
  const allVisibleRowsUseDossierRecoveryAction = searchedRegistrations.length > 0
    && searchedRegistrations.every((reg) => shouldUseDirectPendingRecoveryAction(
      reg.crStatus,
      allVisibleRowsUsePaidRecoveryAction,
    ));
  const showRepeatedDirectRecoveryIconActions = allVisibleRowsUseDossierRecoveryAction
    && useCompactStatusActionLabel
    && searchedRegistrations.length >= MIN_REPEATED_DIRECT_RECOVERY_ICON_ACTIONS;
  const showRepeatedCustomStatusIconActions = allVisibleRowsUseCustomStatusActions
    && useCompactStatusActionLabel
    && searchedRegistrations.length >= MIN_REPEATED_CUSTOM_STATUS_ICON_ACTIONS;
  const showBusyDirectRecoveryIconActions = showBusyListSearchOnboarding
    && allVisibleRowsUseDirectPendingRecoveryAction
    && useCompactStatusActionLabel
    && searchedRegistrations.length >= MIN_LOCAL_SEARCH_REGISTRATIONS;
  const showBusyStatusIconActions = (
    showBusyListSearchOnboarding
    && useCompactStatusActionLabel
    && !allVisibleRowsUseDirectPendingRecoveryAction
  ) || showRepeatedPaymentStatusIconActions
    || showRepeatedCustomStatusIconActions;
  const dossierScopeHint = [
    allVisibleRowsUsePaidRecoveryAction
      ? buildPaidRecoveryScopeHint(dossierIdentityTargetLabel)
      : allVisibleRowsUseDossierRecoveryAction
      ? buildPendingRecoveryScopeHint(dossierIdentityTargetLabel)
      : showInlinePaymentWorkflowRowLabel
      ? buildDossierLinkScopeHint(dossierIdentityTargetLabel)
      : allVisibleRowsCanOpenPaymentWorkflow
      ? buildPaymentWorkflowScopeHint(dossierIdentityTargetLabel)
      : allVisibleRowsUseCustomStatusActions
      ? buildCustomStatusNormalizationScopeHint(dossierIdentityTargetLabel)
      : useCompactStatusActionLabel
      ? buildCompactDossierScopeHint(dossierIdentityTargetLabel)
      : buildDossierOnlyScopeHint(dossierIdentityTargetLabel),
    singleVisibleMissingContactSummary,
  ].filter(Boolean).join(' ');
  const showInlineCurrentViewDossierHint = Boolean(combinedSingleChoiceSummary)
    && showFilterOnboardingCopy
    && !hideSingleResultLocalSearchPassiveCurrentView
    && loadedRegistrationCount > 1
    && (!showLocalSearchControl || hasLocalSearch);
  const combinedSingleChoiceContextSummary = [
    combinedSingleChoicePassiveContextSummary,
    showInlineCurrentViewDossierHint ? dossierScopeHint : '',
  ].filter(Boolean).join(' ');
  const canFoldSharedSourceIntoBusySearch = Boolean(
    showBusyListSearchOnboarding
    && combinedSingleChoiceSummary
    && combinedSingleChoiceSourceSummary
    && combinedSingleChoicePassiveContextSummary === combinedSingleChoiceSourceSummary
    && !combinedSingleChoiceCustomStatusGuidance
    && !hasCustomFilters
    && !showAdvancedLimitControl,
  );
  const hasSharedListContextSummary = Boolean(
    !canFoldSharedSourceIntoBusySearch
    && (
      combinedSharedListContextSummary
      || shouldShowSharedStatusSummary
      || shouldShowSharedCohortSummary
      || shouldShowSharedSourceSummary
      || sharedVisibleCreatedAtSummary
      || showSharedVisibleMissingContactSummary
      || showSharedNotesSummaryInList
    ),
  );
  const hideBusyListPassiveCurrentViewPanel = (
    showBusyListSearchOnboarding
    || showBusyListNonNarrowingSearch
    || hideSingleResultLocalSearchPassiveCurrentView
  )
    && Boolean(combinedSingleChoiceSummary)
    && !hasCustomFilters
    && !showAdvancedLimitControl
    && (
      hideSingleResultLocalSearchPassiveCurrentView
      || canFoldSharedSourceIntoBusySearch
      || !combinedSingleChoiceContextSummary
    )
    && !hasSharedListContextSummary;
  const hideBusyListPassiveSingleCohortSummary = showBusyListSearchOnboarding
    && Boolean(singleAvailableCohortLabel)
    && !combinedSingleChoiceSummary
    && actionableStatusFilters.length > 0
    && !hasCustomFilters
    && !showAdvancedLimitControl
    && !hasSharedListContextSummary;
  const hideSingleResultSearchPassiveCohortSummary = Boolean(localSearchSingleResult)
    && !hasManualFilters
    && !hasEffectiveSlugFilter
    && !cohortsQuery.isError;
  const hasBusyListPassiveSingleStatusSummary = showSingleStatusSummaryBlock || showSingleCustomStatusSummary;
  const hideBusyListPassiveSingleStatusSummary = showBusyListSearchOnboarding
    && hasBusyListPassiveSingleStatusSummary
    && !singleAvailableCohortLabel
    && !combinedSingleChoiceSummary
    && !hasCustomFilters
    && !showAdvancedLimitControl
    && !hasSharedListContextSummary;
  const hiddenBusyListMissingContactContext = showBusyListSearchOnboarding
    ? sharedVisibleMissingContactSummary.replace(/\.$/, '')
    : '';
  const hiddenBusyListSharedNotesContext = foldSharedNotesIntoBusySearch
    ? sharedVisibleNotesSummary.replace(/\.$/, '')
    : '';
  const hiddenBusyListCohortLoadingContext = hideBusyListPassiveCohortLoadingSummary
    ? cohortFilterLoadingMessage.replace(/\.$/, '')
    : '';
  const hiddenBusyListBaseContextSummary = hideBusyListPassiveCurrentViewPanel
    && !hideSingleResultLocalSearchPassiveCurrentView
    ? [
      combinedSingleChoiceSummary,
      canFoldSharedSourceIntoBusySearch && combinedSingleChoiceSourceSummary
        ? combinedSingleChoiceSourceSummary.replace(/\.$/, '')
        : '',
    ].filter(Boolean).join('. ')
    : hideBusyListPassiveSingleCohortSummary
      ? [singleAvailableCohortLabel, standaloneSingleChoiceBusySearchSourceSummary].filter(Boolean).join(' · ')
      : hideBusyListPassiveSingleStatusSummary
        ? [
          singleVisibleStatus
            ? statusFilterLabels[singleVisibleStatus]
            : singleVisibleCustomStatus != null
              ? customRegistrationStatusLabel(singleVisibleCustomStatus)
              : '',
          standaloneSingleChoiceBusySearchSourceSummary,
        ].filter(Boolean).join(' · ')
      : '';
  const hiddenBusyListBaseIncludesSharedSource = Boolean(
    hiddenBusyListBaseContextSummary.includes('Fuente visible:'),
  );
  const hiddenBusyListCohortUnavailableScopeContext = hideBusyListPassiveCohortUnavailableSummary
    ? [
      singleVisibleCohortLabel || singleAvailableCohortLabel,
      hiddenBusyListBaseContextSummary
        ? ''
        : showSingleStatusSummaryBlock && singleVisibleStatus
          ? statusFilterLabels[singleVisibleStatus]
          : '',
    ].filter(Boolean).join(' · ')
    : '';
  const hiddenBusyListCohortUnavailableContext = hideBusyListPassiveCohortUnavailableSummary
    ? [
      hiddenBusyListCohortUnavailableScopeContext,
      busyCohortFilterUnavailableMessage.replace(/\.$/, ''),
    ].filter(Boolean).join('. ')
    : '';
  const busyListSharedCreatedAtContext = showBusyListSearchOnboarding
    && !hasCustomFilters
    && Boolean(sharedVisibleCreatedAtLabel)
    && (!hasSharedVisibleSource || hiddenBusyListBaseIncludesSharedSource)
    ? `Misma fecha de registro: ${sharedVisibleCreatedAtLabel}`
    : '';
  const hiddenBusyListContextSummary = [
    hiddenBusyListBaseContextSummary,
    hiddenBusyListCohortLoadingContext,
    hiddenBusyListCohortUnavailableContext,
    busyListSharedCreatedAtContext,
    hiddenBusyListMissingContactContext,
    hiddenBusyListSharedNotesContext,
  ].filter(Boolean).join('. ');
  const localSearchHelperText = !localSearchKey
    && hiddenBusyListContextSummary
    && baseLocalSearchHelperText
    ? `${hiddenBusyListContextSummary}. ${baseLocalSearchHelperText.replace(' sin cambiar filtros.', '.')}`
    : baseLocalSearchHelperText;
  const showDossierScopeHint = loadedRegistrationCount > 0
    && !hasUsedRowAction
    && !hasUsedFilterControl
    && !showBusyListSearchOnboarding
    && !showInlineCurrentViewDossierHint;
  const showFirstRunFilterHelper = showFilterOnboardingCopy
    && !showSingleResultWithoutHiddenLimit
    && !showBusyListSearchOnboarding
    && !hidePassiveFiltersDuringEmptyLocalSearch;
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
    && !hasLocalSearch
    && !showEmptyLocalSearchResults,
  );
  const showInlineSingleChoiceLimitToggle = showLimitAdjustmentAction
    && !hidePassiveFiltersDuringEmptyLocalSearch
    && !showEmptyLocalSearchLimitGuidance
    && Boolean(combinedSingleChoiceSummary || singleAvailableCohortLabel || showSingleStatusSummaryBlock);
  const statusFilterCanSelfReset = statusAlreadyVisibleInFilterStrip && !hasEffectiveSlugFilter && !hasCustomLimit;
  const showFilteredResetActionCandidate = !showEmptyLocalSearchResults
    && !hasLocalSearch
    && !showInlineSummaryResetAction
    && !cohortFilterCanSelfReset
    && !statusFilterCanSelfReset;
  const showInlineActiveStatusResetAction = showActiveStatusFilterSummary
    && showFilteredResetActionCandidate
    && !activeViewSummaryMessage
    && !showUtilityCountSummary
    && !showScopedCopyCsvAction
    && !showScopedCopyMessage;
  const showFilteredResetAction = showFilteredResetActionCandidate && !showInlineActiveStatusResetAction;
  const showFilteredEmptyStateResetAction = hasManualFilters;
  const showFilteredEmptyStateRefreshAction = !hasManualFilters && !cohortsQuery.isError;
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
    && !showUnconfiguredCourseFirstRunLimitEmptyState
    && !showUnconfiguredCourseFirstRunFilteredEmptyState
    && !showMultiCohortFirstRunLimitEmptyState
    && !hasVisibleRegistrations;
  const showInitialCohortErrorState = !regsQuery.isLoading
    && !regsQuery.isError
    && cohortsQuery.isError
    && (!hasCustomFilters || (hasCustomLimit && !hasManualFilters))
    && !hasVisibleRegistrations;
  const showRegistrationErrorInlineRetry = regsQuery.isError;
  const showInlineCohortRetryAction = showCohortFilterUnavailableSummary
    && !hideBusyListPassiveCohortUnavailableSummary
    && !regsQuery.isError;
  const showHeaderRefreshAction = !showInitialCohortErrorState
    && !showRegistrationErrorInlineRetry
    && !showInlineCohortRetryAction
    && (regsQuery.isError || cohortsQuery.isError);
  const headerRefreshLabel = cohortsQuery.isError
    ? regsQuery.isError
      ? 'Reintentar datos'
      : unavailableCohortFilterRetryLabel
    : regsQuery.isError
      ? 'Reintentar inscripciones'
      : 'Refrescar lista';
  const headerRefreshTitle = cohortsQuery.isError && !regsQuery.isError
    ? unavailableCohortFilterRetryTitle
    : undefined;
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
    && (
      !hasCustomFilters
      || showPassiveSingleCohortLimitEmptyState
      || showSelectedCohortFirstRunEmptyState
      || showUnconfiguredCourseFirstRunLimitEmptyState
      || showUnconfiguredCourseFirstRunFilteredEmptyState
      || showMultiCohortFirstRunLimitEmptyState
    )
    && !hasVisibleRegistrations;
  const showInitialCohortResolutionState = !regsQuery.isLoading
    && !regsQuery.isError
    && cohortsQuery.isLoading
    && (!hasCustomFilters || (hasCustomLimit && !hasManualFilters))
    && !hasVisibleRegistrations;
  const showInitialRegistrationLoading = regsQuery.isLoading && !regsQuery.data;
  const hidePassiveFiltersDuringCappedEmptyLocalSearch =
    showEmptyLocalSearchLimitGuidance && !showAdvancedFilters;
  const showRegistrationFilterPanel = !showInitialRegistrationLoading
    && !showInitialCohortResolutionState
    && !showInitialCohortErrorState
    && !showFilteredEmptyState
    && !showFocusedEmptyLocalSearchState
    && !hideBusyListPassiveCohortLoadingSummary
    && !hideBusyListPassiveCohortUnavailableSummary
    && !hidePassiveFiltersDuringCappedEmptyLocalSearch
    && !showSingleResultWithOnlyPassiveFilterContext
    && !hideSingleResultLocalSearchPassiveFilterPanel
    && (!regsQuery.isError || hasVisibleRegistrations);
  const showRegistrationResultsPanel = !showInitialRegistrationLoading
    && !showInitialFilterGuidance
    && !showInitialCohortResolutionState
    && !showInitialCohortErrorState;
  const limitToggleLabel = showAdvancedFilters
    ? 'Ocultar límite'
    : limit !== DEFAULT_LIMIT
      ? `Ajustar límite (${limit})`
      : 'Ajustar límite';
  const limitToggleAccessibleLabel = showAdvancedFilters
    ? 'Ocultar límite de carga'
    : limit !== DEFAULT_LIMIT
      ? `Ajustar límite de carga (${limit})`
      : 'Ajustar límite de carga';
  const limitToggleTitle = showAdvancedFilters
    ? 'Ocultar el campo de límite de carga.'
    : 'Mostrar el campo de límite de carga para revisar un lote distinto.';
  const singleVisibleStatusHelperText = showCohortFilterUnavailableSummary
    ? ''
    : 'Estado único en esta vista.';
  const filtersHelpText = buildAutomaticFilterHelpText({
    combinedSingleChoiceSummary,
    hasCohortFilterControl: hasDedicatedCohortFilterControl,
    hasStatusFilterControl: actionableStatusFilters.length > 0,
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
  const showStatusFilterCounts = hasVisibleRegistrations && !hasLocalSearch;
  const statusFilterGroupLabel = statusFilterCanSelfReset
    ? `Filtro de estado activo: ${statusFilterLabels[status]}`
    : 'Filtros de estado de inscripciones';
  const hideCustomStatusFilterSummaryForSearch = showLocalSearchControl
    && hasCustomStatusSearch
    && actionableStatusFilters.length === 0
    && !showSingleStatusSummary;
  const redundantSearchStatusFilter = hasLocalSearch
    && !hasManualFilters
    && !cohortsQuery.isError
    ? (
      shouldShowSharedStatusSummary
        ? singleSearchedKnownStatus
        : localSearchSingleResultKnownStatus
    )
    : null;
  const hideUnrelatedStatusFiltersForNarrowSearch = Boolean(
    hasLocalSearch
    && localSearchNarrowsRegistrations
    && searchedRegistrations.length > 1
    && searchedRegistrations.length < MIN_LOCAL_SEARCH_REGISTRATIONS
    && !hasManualFilters
    && !hasCustomLimit
    && !viewHitsCurrentLimit
    && !cohortsQuery.isError
    && singleSearchedKnownStatus,
  );
  const displayedActionableStatusFilters = useMemo(
    () => (hideUnrelatedStatusFiltersForNarrowSearch
      ? []
      : redundantSearchStatusFilter == null
      ? actionableStatusFilters
      : actionableStatusFilters.filter((value) => value !== redundantSearchStatusFilter)),
    [actionableStatusFilters, hideUnrelatedStatusFiltersForNarrowSearch, redundantSearchStatusFilter],
  );
  const showCustomStatusFilterUnavailableSummary = hasVisibleRegistrations
    && !showSingleStatusSummary
    && actionableStatusFilters.length === 0
    && !hideCustomStatusFilterSummaryForSearch;
  const showStatusFilterColumn = !hideCustomStatusFilterSummaryForSearch
    && !hidePassiveFiltersDuringEmptyLocalSearch
    && !hideBusyListPassiveSingleStatusSummary
    && (
      showSingleStatusSummaryBlock
      || showActiveStatusFilterSummary
      || showSingleCustomStatusSummary
      || showCustomStatusFilterUnavailableSummary
      || displayedActionableStatusFilters.length > 0
    );
  const showPassiveSingleCohortSummary = Boolean(singleAvailableCohortLabel)
    && !hidePassiveFiltersDuringEmptyLocalSearch
    && !hideBusyListPassiveSingleCohortSummary
    && !hideSingleResultSearchPassiveCohortSummary;
  const showInlineEmptyCohortFilterGuidance = showEmptyCohortFilterSummary && showStatusFilterColumn;
  const showEmptyCohortFilterSummaryBlock = showEmptyCohortFilterSummary && !showInlineEmptyCohortFilterGuidance;
  const showCohortSelectControl = showCohortSelect && !showInlineEmptyCohortFilterGuidance;
  const showCohortFilterColumn = !hidePassiveFiltersDuringEmptyLocalSearch
    && !hideCohortFilterForSingleLocalSearchResult
    && (
      showCohortSelectControl
      || showCohortFilterUnavailableSummary
      || showCohortFilterLoadingSummary
      || showEmptyCohortFilterSummaryBlock
      || showPassiveSingleCohortSummary
    );
  const filterGridColumns = showStatusFilterColumn ? 6 : 12;
  const statusFilterGridColumns = showCohortFilterColumn ? 6 : 12;
  const customStatusFilterGuidance = customStatusFilterUnavailableMessage;
  const combinedSingleChoiceHelperText = showAdvancedLimitControl
    ? 'Vista única por ahora: una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.'
    : '';
  const emailEvents = dedupeCourseEmailEvents(emailEventsQuery.data ?? []);
  const canReviewSystemEmails = selectedDossier?.intent !== 'markPaid';
  const hasSystemEmailHistory = canReviewSystemEmails && emailEvents.length > 0;
  const showSystemEmailHistoryAction = canReviewSystemEmails
    && (showEmailHistory || hasSystemEmailHistory || emailEventsQuery.isError);
  const showSystemEmailHistoryRetryAction = showSystemEmailHistoryAction
    && emailEventsQuery.isError;
  const showCollapsedSystemEmailRetryAction = showSystemEmailHistoryRetryAction && !showEmailHistory;
  const systemEmailHistoryActionLabel = showEmailHistory
    ? hideSystemEmailsLabel
    : showCollapsedSystemEmailRetryAction
      ? retrySystemEmailsLabel
      : showSystemEmailsLabel;
  const firstRunCohort = singleAvailableCohort ?? (showSelectedCohortFirstRunEmptyState ? selectedConfiguredCohort : null);
  const firstRunCohortLabel = firstRunCohort?.firstRunLabel.trim() ?? '';
  const specificFirstRunCohortLabel = firstRunCohortLabel && !isGenericFirstRunCohortLabel(firstRunCohortLabel)
    ? firstRunCohortLabel
    : '';
  const configuredCohortFirstRunLabels = configuredCohortOptions.map((option) => option.firstRunLabel);
  const hasSingleCourseFirstRunVariants =
    hasMultipleAvailableCohorts && countInitialCohortPreviewLabels(configuredCohortFirstRunLabels) === 1;
  const initialEmptyStateMessage = firstRunCohort
    ? buildSingleCohortInitialEmptyStateMessage(specificFirstRunCohortLabel)
    : hasMultipleAvailableCohorts
      ? buildInitialEmptyStateMultiCohortMessage(
        configuredCohortOptions.length,
        configuredCohortFirstRunLabels,
      )
    : initialEmptyStateConfigMessage;
  const initialEmptyStateAction = firstRunCohort
    ? {
      label: initialEmptyStateFormActionLabel,
      to: `/inscripcion/${encodeURIComponent(firstRunCohort.value)}`,
      ariaLabel: specificFirstRunCohortLabel
        ? `Abrir formulario público de ${specificFirstRunCohortLabel}`
        : initialEmptyStateFormActionLabel,
      title: specificFirstRunCohortLabel
        ? `Abrir formulario público de ${specificFirstRunCohortLabel} en una pestaña nueva`
        : `${initialEmptyStateFormActionLabel} en una pestaña nueva`,
      target: '_blank',
      rel: 'noopener noreferrer',
    }
    : {
      label: hasSingleCourseFirstRunVariants
        ? initialEmptyStateChooseFormActionLabel
        : hasMultipleAvailableCohorts
          ? initialEmptyStateChooseFormActionLabel
          : initialEmptyStateConfigActionLabel,
      to: '/configuracion/cursos',
      ariaLabel: hasSingleCourseFirstRunVariants
        ? initialEmptyStateSingleCourseVariantActionAriaLabel
        : hasMultipleAvailableCohorts
          ? initialEmptyStateMultiCohortActionAriaLabel
          : initialEmptyStateConfigActionAriaLabel,
      title: hasSingleCourseFirstRunVariants
        ? buildInitialEmptyStateSingleCourseVariantActionTitle(
          configuredCohortOptions.length,
          configuredCohortFirstRunLabels,
        )
        : hasMultipleAvailableCohorts
          ? buildInitialEmptyStateMultiCohortActionTitle(
            configuredCohortOptions.length,
            configuredCohortFirstRunLabels,
          )
          : initialEmptyStateConfigActionAriaLabel,
      target: undefined,
      rel: undefined,
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
    setFollowUpMenuTarget(null);
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

  const handleSystemEmailHistoryAction = () => {
    if (showEmailHistory) {
      setShowEmailHistory(false);
      return;
    }

    setShowEmailHistory(true);
    if (emailEventsQuery.isError) {
      handleRefreshSystemEmails();
    }
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
      setCopyMessage('CSV copiado');
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

  const handleClearLocalSearch = () => {
    setLocalSearch('');
    if (showAdvancedFilters && hasLocalSearch && limit === DEFAULT_LIMIT) {
      setShowAdvancedFilters(false);
    }
  };

  const handleToggleAdvancedFilters = () => {
    setHasUsedFilterControl(true);
    setShowAdvancedFilters((current) => !current);
  };

  const handleOpenStatusMenu = (anchorEl: HTMLElement, reg: CourseRegistrationDTO) => {
    setHasUsedRowAction(true);
    setPageFlash(null);
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
    setHasUsedRowAction(true);
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
    setPageFlash(null);
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
  const sharedReceiptNotes = getSharedReceiptNotes(receipts);
  const sharedReceiptSummary = sharedReceiptCreatedLabel && sharedReceiptNotes
    ? `Resumen: subidos ${sharedReceiptCreatedLabel} · nota: ${sharedReceiptNotes}`
    : '';
  const sharedReceiptCreatedIsInSummary = Boolean(sharedReceiptSummary && sharedReceiptCreatedLabel);
  const sharedReceiptNotesIsInSummary = Boolean(sharedReceiptSummary && sharedReceiptNotes);
  const sharedFollowUpCreatedLabel = getSharedOptionalDateLabel(followUps.map((entry) => entry.crfCreatedAt));
  const sharedFollowUpTypeLabel = getSharedFollowUpTypeLabel(followUps);
  const sharedFollowUpNextLabel = getSharedOptionalDateLabel(followUps.map((entry) => entry.crfNextFollowUpAt));
  const sharedFollowUpSummaryParts = [
    sharedFollowUpTypeLabel,
    sharedFollowUpNextLabel ? `próximo ${sharedFollowUpNextLabel}` : '',
    sharedFollowUpCreatedLabel ? `registrados ${sharedFollowUpCreatedLabel}` : '',
  ].filter(Boolean);
  const sharedFollowUpSummary = sharedFollowUpSummaryParts.length > 1
    ? `Resumen: ${sharedFollowUpSummaryParts.join(' · ')}`
    : '';
  const sharedFollowUpCreatedIsInSummary = Boolean(sharedFollowUpSummary && sharedFollowUpCreatedLabel);
  const sharedFollowUpTypeIsInSummary = Boolean(sharedFollowUpSummary && sharedFollowUpTypeLabel);
  const sharedFollowUpNextIsInSummary = Boolean(sharedFollowUpSummary && sharedFollowUpNextLabel);
  const sharedEmailEventCreatedLabel = getSharedOptionalDateLabel(emailEvents.map((entry) => entry.ceCreatedAt));
  const sharedEmailEventTypeLabel = getSharedEmailEventTypeLabel(emailEvents);
  const sharedEmailEventStatusLabel = getSharedEmailEventStatusLabel(emailEvents);
  const sharedEmailEventSummary = sharedEmailEventCreatedLabel && sharedEmailEventTypeLabel
    ? `Resumen: ${[
      sharedEmailEventTypeLabel,
      sharedEmailEventStatusLabel,
      sharedEmailEventCreatedLabel,
    ].filter(Boolean).join(' · ')}`
    : '';
  const sharedEmailEventStatusIsInSummary = Boolean(sharedEmailEventSummary && sharedEmailEventStatusLabel);
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
  const showActiveRegistrationStatusChip = Boolean(activeRegistrationStatus)
    && !(isMarkPaidIntent && showMarkPaidAction);
  const hasOpenDossierComposer = showNotesComposer || showReceiptComposer || showFollowUpComposer;
  const showInlineEmptyNotesAction = !isMarkPaidIntent
    && !showReceiptComposer
    && !showNotesComposer
    && !showFollowUpComposer
    && !hasSavedNotes;
  const showInlineEmptyFollowUpAction = !isMarkPaidIntent
    && !showReceiptComposer
    && !showFollowUpComposer
    && !showNotesComposer
    && followUps.length === 0;
  const hasPrimaryDossierAction = showMarkPaidAction || showSystemEmailHistoryAction;
  const hasAnyInlineDossierContextAction = showInlineEmptyNotesAction || showInlineEmptyFollowUpAction;
  const hasMultipleInlineDossierContextActions = showInlineEmptyNotesAction && showInlineEmptyFollowUpAction;
  const showGroupedDossierContextActions = hasAnyInlineDossierContextAction
    && (hasMultipleInlineDossierContextActions || hasPrimaryDossierAction);
  const showDirectInlineEmptyNotesAction = showInlineEmptyNotesAction && !showGroupedDossierContextActions;
  const showDirectInlineEmptyFollowUpAction = showInlineEmptyFollowUpAction && !showGroupedDossierContextActions;
  const showDossierActionRow = hasPrimaryDossierAction
    || showGroupedDossierContextActions
    || showDirectInlineEmptyNotesAction
    || showDirectInlineEmptyFollowUpAction;
  const groupedDossierContextActionsExpandedLabel = formatDossierContextActionsLabel({
    showInlineEmptyFollowUpAction,
    showInlineEmptyNotesAction,
  });
  const groupedDossierContextActionsLabel = hasPrimaryDossierAction
    ? optionalDossierContextActionsFallbackLabel
    : groupedDossierContextActionsExpandedLabel;
  const groupedDossierContextActionsAccessibleLabel = groupedDossierContextActionsLabel === optionalDossierContextActionsFallbackLabel
    ? groupedDossierContextActionsExpandedLabel
    : groupedDossierContextActionsLabel;
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
  const emptyReceiptActionLabel = showEvidenceOnlyEmptyReceiptCopy
    ? 'Agregar evidencia'
    : 'Agregar comprobante';
  const emptyReceiptActionAccessibleLabel = showEvidenceOnlyEmptyReceiptCopy
    ? 'Agregar evidencia'
    : 'Agregar primer comprobante';
  const showReceiptsSection = !(
    activeRegistrationKnownStatus === 'cancelled'
    && !hasReceipts
    && !showReceiptComposer
    && !isMarkPaidIntent
  );
  const showCompactMarkPaidNotesState = selectedDossier?.intent === 'markPaid'
    && !showNotesComposer
    && !hasSavedNotes;
  const showEmptyNotesState = !showNotesComposer && !hasSavedNotes;
  const showEditNotesAction = !hasOpenDossierComposer && hasSavedNotes;
  const showNotesSaveAction = hasNotesDraftChanges;
  const notesComposerIdleHelperText = hasSavedNotes
    ? 'Edita el contenido para mostrar Guardar notas.'
    : 'Escribe una nota para mostrar Guardar notas.';
  const emptyNotesSectionHelperText = showCompactMarkPaidNotesState
    ? markPaidEmptyNotesHelperText
    : emptyNotesHelperText;
  const showReceiptCountChip = receipts.length > 1;
  const canSubmitReceipt = Boolean(trimToNull(receiptForm.fileUrl));
  const showReceiptSaveAction = canSubmitReceipt || receiptForm.editingId != null;
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
  const showAddReceiptAction = !hasOpenDossierComposer && hasReceipts && selectedDossier?.intent !== 'markPaid';
  const canHideReceiptUrlField = showReceiptUrlField
    && receiptForm.editingId == null
    && !canSubmitReceipt
    && !hasReceiptMetadataDraft;
  const showReceiptExistingLinkAction = !showReceiptUrlField && !canSubmitReceipt;
  const showReceiptReviewPane = hasReceipts || !showReceiptComposer;
  const showReceiptUploadWidget = !showReceiptUrlField || receiptForm.editingId != null;
  const showReceiptMetadataFields = (
    receiptForm.editingId != null
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
  const showFollowUpUploadWidget = !showFollowUpUrlField || followUpForm.editingId != null;
  const showFollowUpCountChip = followUps.length > 1;
  const showFollowUpHistoryPane = followUps.length > 0 || !showFollowUpComposer;
  const showAddFollowUpAction = !hasOpenDossierComposer && followUps.length > 0;
  const isCreatingFirstFollowUp = showFollowUpComposer && followUpForm.editingId == null && followUps.length === 0;
  const canSubmitFollowUp = Boolean(trimToNull(followUpForm.notes));
  const showFollowUpSaveAction = canSubmitFollowUp || followUpForm.editingId != null;
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
  const statusMenuActionTargetLabel = statusMenuReg ? getActionTargetLabelForRegistration(statusMenuReg) : '';
  const receiptMenuReceipt = receiptMenuTarget?.receipt ?? null;
  const followUpMenuEntry = followUpMenuTarget?.entry ?? null;
  const receiptMenuActionLabel = receiptMenuReceipt
    ? receiptDisplayLabelWithContext(
      receiptMenuReceipt,
      receiptIdsRequiringFileDisambiguator.has(receiptMenuReceipt.crrId),
    )
    : '';
  const followUpMenuActionLabel = followUpMenuEntry
    ? followUpActionTargetLabelWithContext(
      followUpMenuEntry,
      followUpIdsRequiringActionDisambiguator.has(followUpMenuEntry.crfId),
    )
    : '';
  const activeRegistrationCourseSlug = activeRegistration?.crCourseSlug.trim() ?? '';
  const activeRegistrationCourseLabel = activeRegistrationCourseSlug
    ? (
      cohortSummaryLabelsBySlug.get(activeRegistrationCourseSlug)
      ?? cohortLabelsBySlug.get(activeRegistrationCourseSlug)
      ?? readableCohortFallbackLabel(activeRegistrationCourseSlug)
    )
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
    && !normalizeRegistrationNameValue(activeRegistration?.crFullName)
    && registrationNeedsContact(activeRegistration),
  );
  const activeRegistrationSecondaryLine = showInternalRegistrationReference && activeRegistration?.crPartyId
    ? `Sin datos de contacto. Referencia interna: Party #${activeRegistration.crPartyId}.`
    : activeRegistrationIdentity.secondary;
  const isRefreshingDossier = dossierQuery.isFetching || (showSystemEmailHistoryAction && showEmailHistory && emailEventsQuery.isFetching);
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
    ? isMarkPaidFirstReceiptFlow
      ? hasSavedNotes || showNotesComposer
      : (!isConfirmMarkPaidFlow || hasSavedNotes || showNotesComposer)
    : (hasSavedNotes || showNotesComposer);
  const showFollowUpSection = isMarkPaidIntent
    ? isMarkPaidFirstReceiptFlow
      ? followUps.length > 0 || showFollowUpComposer
      : (!isConfirmMarkPaidFlow || followUps.length > 0 || showFollowUpComposer)
    : (followUps.length > 0 || showFollowUpComposer);
  const prioritizePaymentSection = isMarkPaidIntent;
  const hasVisibleDossierComposer = hasOpenDossierComposer && !dossierQuery.isLoading && !dossierQuery.isError;
  const showDossierFooterCloseAction = !isMarkPaidFirstReceiptFlow && !hasVisibleDossierComposer;
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
      title={copyCsvButtonTitle}
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
            {showEditNotesAction ? (
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
                  {showNotesSaveAction && (
                    <Button
                      variant="contained"
                      size="small"
                      startIcon={<SaveIcon />}
                      onClick={handleSaveNotes}
                      disabled={updateNotesMutation.isPending}
                    >
                      Guardar notas
                    </Button>
                  )}
                  <Button variant="text" size="small" onClick={handleHideNotesComposer}>
                    Cancelar notas
                  </Button>
                </Stack>
                {!hasNotesDraftChanges && (
                  <Typography variant="caption" color="text.secondary">
                    {notesComposerIdleHelperText}
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
              {sharedReceiptSummary ? (
                <Typography
                  variant="body2"
                  color="text.secondary"
                  data-testid="course-registration-shared-receipt-summary"
                >
                  {sharedReceiptSummary}
                </Typography>
              ) : (
                <>
                  {sharedReceiptCreatedLabel && (
                    <Typography variant="body2" color="text.secondary">
                      Todos subidos: {sharedReceiptCreatedLabel}
                    </Typography>
                  )}
                  {sharedReceiptNotes && (
                    <Typography
                      variant="body2"
                      color="text.secondary"
                      data-testid="course-registration-shared-receipt-notes"
                    >
                      Nota de comprobantes: {sharedReceiptNotes}
                    </Typography>
                  )}
                </>
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
                  {showReceiptUploadWidget && (
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
                  )}
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
                      {showReceiptUrlField
                        ? receiptUrlFallbackHelpText
                        : 'Primero elige el archivo o pega un enlace; luego podrás ajustar el nombre visible y las notas.'}
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
                    {showReceiptSaveAction && (
                      <Button
                        variant="contained"
                        onClick={handleSubmitReceipt}
                        disabled={createReceiptMutation.isPending || updateReceiptMutation.isPending || !canSubmitReceipt}
                      >
                        {receiptForm.editingId == null ? 'Guardar comprobante' : 'Actualizar comprobante'}
                      </Button>
                    )}
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
                        <Button
                          color="inherit"
                          size="small"
                          onClick={() => setShowReceiptComposer(true)}
                          aria-label={emptyReceiptActionAccessibleLabel}
                          title={emptyReceiptActionAccessibleLabel}
                        >
                          {emptyReceiptActionLabel}
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
                    const receiptCreatedLabel = sharedReceiptCreatedIsInSummary || sharedReceiptCreatedLabel
                      ? ''
                      : formatOptionalDate(receipt.crrCreatedAt);
                    const isReceiptBeingEdited = receiptForm.editingId === receipt.crrId;
                    const receiptNotes = receipt.crrNotes?.trim() ?? '';
                    const showReceiptNotes = Boolean(receiptNotes)
                      && !(sharedReceiptNotesIsInSummary || receiptNotes === sharedReceiptNotes);

                    return (
                      <Paper key={receipt.crrId} variant="outlined" sx={{ p: 1.5 }}>
                        <Stack spacing={1}>
                          {looksLikeImageResource(receipt.crrFileUrl, receipt.crrFileName) && (
                            <Box
                              component="img"
                              src={receipt.crrFileUrl}
                              alt={receiptLabel}
                              style={{ objectFit: 'contain' }}
                              sx={{
                                width: '100%',
                                maxHeight: 220,
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
                                rel="noopener noreferrer"
                                aria-label={`Abrir comprobante ${receiptLabel} en una pestaña nueva`}
                                title={`Abrir comprobante ${receiptLabel} en una pestaña nueva`}
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
                            {isReceiptBeingEdited ? (
                              <Typography variant="body2" color="text.secondary">
                                En edición
                              </Typography>
                            ) : !showReceiptComposer ? (
                              <IconButton
                                size="small"
                                title="Opciones del comprobante"
                                aria-label={`Abrir acciones para comprobante ${receiptLabel}`}
                                aria-haspopup="menu"
                                onClick={(event) => handleOpenReceiptMenu(event.currentTarget, receipt)}
                              >
                                <MoreVertIcon fontSize="small" />
                              </IconButton>
                            ) : null}
                          </Stack>
                          {showReceiptNotes && (
                            <Typography variant="body2" color="text.secondary">
                              {receiptNotes}
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
              title={headerRefreshTitle}
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
              aria-describedby={initialEmptyStateAction.target ? initialEmptyStateNewTabDescriptionId : undefined}
              title={initialEmptyStateAction.title}
              target={initialEmptyStateAction.target}
              rel={initialEmptyStateAction.rel}
              endIcon={
                initialEmptyStateAction.target ? (
                  <span data-testid="course-registration-initial-empty-state-new-tab-icon" aria-hidden="true">
                    <OpenInNewIcon fontSize="small" />
                  </span>
                ) : undefined
              }
            >
              {initialEmptyStateAction.label}
            </Button>
          )}
        >
          {initialEmptyStateMessage}
          {initialEmptyStateAction.target && (
            <Box
              id={initialEmptyStateNewTabDescriptionId}
              component="span"
              sx={{
                border: 0,
                clip: 'rect(0 0 0 0)',
                height: 1,
                margin: -1,
                overflow: 'hidden',
                padding: 0,
                position: 'absolute',
                whiteSpace: 'nowrap',
                width: 1,
              }}
            >
              {initialEmptyStateNewTabDescription}
            </Box>
          )}
        </Alert>
      )}

      {showRegistrationFilterPanel && !showInitialFilterGuidance && !hideBusyListPassiveCurrentViewPanel && (
        <Paper sx={{ p: 3, borderRadius: 3 }} data-testid="course-registration-filter-panel">
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
                    {combinedSingleChoiceCustomStatusGuidance && (
                      <Typography variant="caption" color="text.secondary">
                        {combinedSingleChoiceCustomStatusGuidance}
                      </Typography>
                    )}
                    {combinedSingleChoiceContextSummary && (
                      <Typography
                        variant="caption"
                        color="text.secondary"
                        data-testid="course-registration-single-choice-context"
                      >
                        {combinedSingleChoiceContextSummary}
                      </Typography>
                    )}
                    {showFirstRunFilterHelper && !combinedSingleChoiceContextSummary && (
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
                        aria-label={limitToggleAccessibleLabel}
                        aria-expanded={showAdvancedFilters}
                        title={limitToggleTitle}
                      >
                        {limitToggleLabel}
                      </Button>
                    )}
                  </Stack>
                </Grid>
              ) : (
                <>
                  {showCohortFilterColumn && (
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
                          {unavailableCohortFilterLabel}
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
                            title={unavailableCohortFilterRetryTitle}
                          >
                            {unavailableCohortFilterRetryLabel}
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
                          Formularios cargando
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {cohortFilterLoadingMessage}
                        </Typography>
                      </Stack>
                    ) : showEmptyCohortFilterSummaryBlock ? (
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
                          Formularios no configurados
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {emptyCohortFilterMessage}
                        </Typography>
                      </Stack>
                    ) : showPassiveSingleCohortSummary ? (
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
                          Formulario público
                        </Typography>
                        <Typography variant="body2" fontWeight={600}>
                          {singleAvailableCohortLabel}
                          {standaloneSingleChoiceInlineSourceSummary ? ` · ${standaloneSingleChoiceInlineSourceSummary}` : ''}
                        </Typography>
                        {showInlineSingleChoiceLimitToggle && (
                          <Button
                            size="small"
                            variant="text"
                            sx={{ alignSelf: 'flex-start', mt: 0.5 }}
                            onClick={handleToggleAdvancedFilters}
                            aria-label={limitToggleAccessibleLabel}
                            aria-expanded={showAdvancedFilters}
                            title={limitToggleTitle}
                          >
                            {limitToggleLabel}
                          </Button>
                        )}
                      </Stack>
                    ) : (
                      <TextField
                        select
                        label={COHORT_FILTER_LABEL}
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
                            ? 'No se pudieron cargar formularios.'
                            : cohortsQuery.isLoading
                              ? 'Cargando formularios…'
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
                  )}
                  {showStatusFilterColumn && (
                    <Grid item xs={12} md={statusFilterGridColumns}>
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
                            {standaloneSingleChoiceInlineSourceSummary ? ` · ${standaloneSingleChoiceInlineSourceSummary}` : ''}
                          </Typography>
                          {showFirstRunFilterHelper && singleVisibleStatusHelperText && (
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
                              aria-label={limitToggleAccessibleLabel}
                              aria-expanded={showAdvancedFilters}
                              title={limitToggleTitle}
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
                            La vista filtrada ya muestra solo este estado.
                          </Typography>
                          {showInlineActiveStatusResetAction && (
                            <Button
                              size="small"
                              variant="text"
                              sx={{ alignSelf: 'flex-start', mt: 0.5 }}
                              onClick={handleResetFilters}
                            >
                              {resetViewLabel}
                            </Button>
                          )}
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
                            Estados no estándar
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
                            {displayedActionableStatusFilters.map((value) => (
                              <Chip
                                key={value}
                                clickable
                                component="button"
                                type="button"
                                color={registrationStatusChipColor(value)}
                                label={statusFilterChipLabel(value, statusCounts, showStatusFilterCounts)}
                                variant={status === value ? 'filled' : 'outlined'}
                                aria-label={statusFilterChipAccessibleLabel(value, status === value, hasLocalSearch)}
                                aria-pressed={status === value}
                                title={statusFilterChipTitle(value, status === value, hasLocalSearch)}
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
            {showInlineEmptyCohortFilterGuidance && (
              <Typography
                data-testid="course-registration-empty-cohort-filter-inline"
                variant="caption"
                color="text.secondary"
                sx={{ display: 'block', mt: 1.5 }}
              >
                {emptyCohortFilterMessage}
              </Typography>
            )}
            {showLimitAdjustmentAction
              && !showInlineSingleChoiceLimitToggle
              && !showEmptyLocalSearchLimitGuidance && (
              <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
                <Button
                  size="small"
                  variant="text"
                  onClick={handleToggleAdvancedFilters}
                  aria-label={limitToggleAccessibleLabel}
                  aria-expanded={showAdvancedFilters}
                  title={limitToggleTitle}
                >
                  {limitToggleLabel}
                </Button>
              </Stack>
            )}
            {showAdvancedFilters && showAdvancedLimitControl && (
              <Collapse in unmountOnExit>
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
            )}
            {showFirstRunFilterHelper
              && filtersHelpText
              && !showFilteredEmptyState
              && !showEmptyCohortFilterSummary && (
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
                {showSharedVisibleMissingContactSummary && (
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
                {showSharedNotesSummaryInList && (
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
              action={showRegistrationErrorInlineRetry || hasCustomFilters ? (
                <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                  {showRegistrationErrorInlineRetry && (
                    <Button color="inherit" size="small" onClick={handleRefresh}>
                      {registrationErrorRetryLabel}
                    </Button>
                  )}
                  {hasCustomFilters && (
                    <Button color="inherit" size="small" onClick={handleResetFilters}>
                      {resetViewLabel}
                    </Button>
                  )}
                </Stack>
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
                    const nextLocalSearch = normalizeVisibleLocalSearchInput(e.target.value);
                    if (nextLocalSearch) setHasUsedFilterControl(true);
                    setLocalSearch(nextLocalSearch);
                  }}
                  placeholder={localSearchPlaceholder}
                  helperText={localSearchHelperText}
                  size="small"
                  fullWidth
                  autoComplete="off"
                  inputProps={{
                    spellCheck: false,
                    title: localSearchInputTitle,
                  }}
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
                            onClick={handleClearLocalSearch}
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
              data-testid="course-registration-empty-local-search"
              aria-label={emptyLocalSearchResultsAccessibleLabel}
              title={emptyLocalSearchResultsAccessibleLabel}
              action={showEmptyLocalSearchLimitRecoveryAction || showEmptyLocalSearchAlertClearAction ? (
                <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                  {showEmptyLocalSearchLimitRecoveryAction && (
                    <Button
                      color="inherit"
                      size="small"
                      onClick={handleToggleAdvancedFilters}
                      aria-expanded={showAdvancedFilters}
                      aria-label={emptyLocalSearchLimitRecoveryAccessibleLabel}
                      title={emptyLocalSearchLimitRecoveryTitle}
                    >
                      {emptyLocalSearchLimitRecoveryLabel}
                    </Button>
                  )}
                  {showEmptyLocalSearchAlertClearAction && (
                    <Button color="inherit" size="small" onClick={handleClearLocalSearch}>
                      Limpiar búsqueda
                    </Button>
                  )}
                </Stack>
              ) : undefined}
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
                  const rowUsesGeneratedIdentity = !normalizeRegistrationNameValue(reg.crFullName)
                    && registrationNeedsContact(reg);
                  const showRowRegistrationDisambiguator =
                    registrationIdsRequiringActionRecordDisambiguator.has(reg.crId)
                    || (
                      registrationIdsRequiringActionDisambiguator.has(reg.crId)
                      && !rowSecondaryIdentity
                    );
                  const rowActionTarget = getActionTargetLabelForRegistration(reg);
                  const useDirectPendingRecoveryAction =
                    shouldUseDirectPendingRecoveryAction(
                      reg.crStatus,
                      showActiveStatusFilterSummary
                        || statusAlreadyVisibleInBusySearchOnboarding
                        || localSearchSharedPaidStatusSummary
                        || (
                          localSearchSingleResultUsesDirectPaidRecovery
                          && localSearchSingleResult?.crId === reg.crId
                        )
                        || singleStatusSummarySupportsPaidRecovery,
                    )
                    || (
                      showBusyListSearchOnboarding
                      && hasOnlyPendingRecoveryStatusAction(reg.crStatus)
                    );
                  const useStatusIconAction = showBusyStatusIconActions && !useDirectPendingRecoveryAction;
                  const statusIconActionIsPaymentWorkflow = canOpenPaymentWorkflowFromStatus(reg.crStatus);
                  const statusIconActionTitle = statusMenuButtonTitle(reg.crStatus, rowActionTarget);
                  const usePaymentStatusMenuLabel =
                    showInlinePaymentWorkflowRowLabel && statusIconActionIsPaymentWorkflow;
                  const rowCohortSlug = reg.crCourseSlug.trim();
                  const rowCohortLabel = cohortSummaryLabelsBySlug.get(rowCohortSlug)
                    ?? cohortLabelsBySlug.get(rowCohortSlug)
                    ?? readableCohortFallbackLabel(rowCohortSlug);
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
                  const useDirectPendingRecoveryIconAction =
                    useDirectPendingRecoveryAction
                    && (
                      showBusyDirectRecoveryIconActions
                      || showRepeatedDirectRecoveryIconActions
                      || showBusyStatusIconActions
                    );
                  const directPendingRecoveryActionLabel = `${pendingStatusMenuLabel(reg.crStatus)} para ${rowActionTarget}`;
                  const directPendingRecoveryActionTitle =
                    `${pendingStatusMenuLabel(reg.crStatus)}; actual: ${registrationStatusLabel(reg.crStatus)}`;
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
                            {registrationRecordLabel(reg.crId)}
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
                      {useStatusIconAction ? (
                        <Tooltip title={statusIconActionTitle}>
                          <span>
                            <IconButton
                              size="small"
                              color={registrationStatusButtonColor(reg.crStatus)}
                              title={statusIconActionTitle}
                              aria-label={statusMenuIconButtonAriaLabel(reg.crStatus, rowActionTarget)}
                              aria-haspopup="menu"
                              data-action-icon={statusIconActionIsPaymentWorkflow ? 'payment-receipt' : 'status-menu'}
                              disabled={isUpdating}
                              onClick={(event) => {
                                handleOpenStatusMenu(event.currentTarget, reg);
                              }}
                            >
                              {statusIconActionIsPaymentWorkflow ? (
                                <ReceiptLongIcon fontSize="small" />
                              ) : (
                                <MoreVertIcon fontSize="small" />
                              )}
                            </IconButton>
                          </span>
                        </Tooltip>
                      ) : useDirectPendingRecoveryIconAction ? (
                        <Tooltip title={directPendingRecoveryActionTitle}>
                          <span>
                            <IconButton
                              size="small"
                              color={registrationStatusButtonColor('pending_payment')}
                              title={directPendingRecoveryActionTitle}
                              aria-label={directPendingRecoveryActionLabel}
                              data-action-icon="pending-recovery"
                              disabled={isUpdating}
                              onClick={() => {
                                handleCloseStatusMenu();
                                handleQuickStatus(reg, 'pending_payment');
                              }}
                              sx={{
                                border: '1px solid',
                                borderColor: 'currentColor',
                              }}
                            >
                              <UndoIcon fontSize="small" />
                            </IconButton>
                          </span>
                        </Tooltip>
                      ) : (
                        <Button
                          size="small"
                          variant="text"
                          color={
                            useDirectPendingRecoveryAction
                              ? registrationStatusButtonColor('pending_payment')
                              : registrationStatusButtonColor(reg.crStatus)
                          }
                          endIcon={useDirectPendingRecoveryAction ? undefined : <ArrowDropDownIcon />}
                          title={
                            useDirectPendingRecoveryAction
                              ? directPendingRecoveryActionTitle
                              : statusMenuButtonTitle(reg.crStatus, rowActionTarget)
                          }
                          aria-label={
                            useDirectPendingRecoveryAction
                              ? directPendingRecoveryActionLabel
                              : usePaymentStatusMenuLabel
                              ? paymentStatusMenuButtonAriaLabel(rowActionTarget)
                              : `Cambiar estado para ${rowActionTarget}`
                          }
                          aria-haspopup={useDirectPendingRecoveryAction ? undefined : 'menu'}
                          disabled={isUpdating}
                          onClick={(event) => {
                            if (useDirectPendingRecoveryAction) {
                              handleCloseStatusMenu();
                              handleQuickStatus(reg, 'pending_payment');
                              return;
                            }

                            handleOpenStatusMenu(event.currentTarget, reg);
                          }}
                        >
                          {useDirectPendingRecoveryAction
                            ? pendingStatusButtonLabel(reg.crStatus, useCompactStatusActionLabel)
                            : usePaymentStatusMenuLabel
                            ? paymentStatusMenuButtonLabel
                            : registrationStatusButtonLabel(reg.crStatus, useCompactStatusActionLabel)}
                        </Button>
                      )}
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
            aria-label={`${openPaymentWorkflowLabel} para ${statusMenuActionTargetLabel}`}
            title={`${openPaymentWorkflowLabel} para ${statusMenuActionTargetLabel}`}
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
            aria-label={`${pendingStatusMenuLabel(statusMenuReg.crStatus)} para ${statusMenuActionTargetLabel}`}
            title={`Usa esta acción para ${pendingStatusMenuTargetLabel(statusMenuReg.crStatus)}.`}
            onClick={() => {
              handleCloseStatusMenu();
              handleQuickStatus(statusMenuReg, 'pending_payment');
            }}
          >
            {pendingStatusMenuLabel(statusMenuReg.crStatus)}
          </MenuItem>
        )}
        {statusMenuReg && canCancelRegistrationFromStatus(statusMenuReg.crStatus) && (
          <MenuItem
            aria-label={`${cancelStatusMenuLabel(statusMenuReg.crStatus)} para ${statusMenuActionTargetLabel}`}
            title={`Usa esta acción para ${cancelStatusMenuTargetLabel(statusMenuReg.crStatus)}.`}
            onClick={() => {
              handleCloseStatusMenu();
              handleQuickStatus(statusMenuReg, 'cancelled');
            }}
          >
            {cancelStatusMenuLabel(statusMenuReg.crStatus)}
          </MenuItem>
        )}
      </Menu>

      <Menu
        open={Boolean(receiptMenuTarget)}
        anchorEl={receiptMenuTarget?.anchorEl ?? null}
        onClose={handleCloseReceiptMenu}
      >
        {receiptMenuReceipt && (
          <MenuItem
            aria-label={`Editar comprobante ${receiptMenuActionLabel}`}
            title={`Editar comprobante ${receiptMenuActionLabel}`}
            onClick={() => handleEditReceipt(receiptMenuReceipt)}
          >
            Editar comprobante
          </MenuItem>
        )}
        {receiptMenuReceipt && (
          <MenuItem
            aria-label={`Eliminar comprobante ${receiptMenuActionLabel}`}
            title={`Eliminar comprobante ${receiptMenuActionLabel}`}
            onClick={() => handleDeleteReceipt(receiptMenuReceipt)}
          >
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
            aria-label={optionalDossierNotesActionLabel}
            title={optionalDossierNotesActionLabel}
            onClick={() => {
              handleCloseDossierContextMenu();
              handleOpenNotesComposer();
            }}
          >
            {optionalDossierNotesActionLabel}
          </MenuItem>
        )}
        {showInlineEmptyFollowUpAction && (
          <MenuItem
            aria-label={optionalDossierFollowUpActionLabel}
            title={optionalDossierFollowUpActionLabel}
            onClick={() => {
              handleCloseDossierContextMenu();
              setShowFollowUpComposer(true);
            }}
          >
            {optionalDossierFollowUpActionLabel}
          </MenuItem>
        )}
      </Menu>

      <Menu
        open={Boolean(followUpMenuTarget)}
        anchorEl={followUpMenuTarget?.anchorEl ?? null}
        onClose={handleCloseFollowUpMenu}
      >
        {followUpMenuEntry && (
          <MenuItem
            aria-label={`Editar seguimiento ${followUpMenuActionLabel}`}
            title={`Editar seguimiento ${followUpMenuActionLabel}`}
            onClick={() => handleEditFollowUp(followUpMenuEntry)}
          >
            Editar seguimiento
          </MenuItem>
        )}
        {followUpMenuEntry && (
          <MenuItem
            aria-label={`Eliminar seguimiento ${followUpMenuActionLabel}`}
            title={`Eliminar seguimiento ${followUpMenuActionLabel}`}
            onClick={() => handleDeleteFollowUp(followUpMenuEntry)}
          >
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
                    {showActiveRegistrationStatusChip && statusChip(activeRegistrationStatus)}
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
                          onClick={handleSystemEmailHistoryAction}
                          aria-expanded={showEmailHistory}
                          disabled={showCollapsedSystemEmailRetryAction && emailEventsQuery.isFetching}
                        >
                          {systemEmailHistoryActionLabel}
                        </Button>
                      )}
                      {showGroupedDossierContextActions && (
                        <Button
                          variant="outlined"
                          endIcon={<ArrowDropDownIcon />}
                          aria-label={groupedDossierContextActionsAccessibleLabel}
                          aria-haspopup="menu"
                          aria-expanded={Boolean(dossierContextMenuAnchor)}
                          title={groupedDossierContextActionsAccessibleLabel}
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
                          {optionalDossierNotesActionLabel}
                        </Button>
                      )}
                      {showDirectInlineEmptyFollowUpAction && (
                        <Button
                          variant="outlined"
                          onClick={() => setShowFollowUpComposer(true)}
                        >
                          {optionalDossierFollowUpActionLabel}
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
                      {sharedEmailEventSummary ? (
                        <Typography variant="body2" color="text.secondary">
                          {sharedEmailEventSummary}
                        </Typography>
                      ) : (
                        <>
                          {sharedEmailEventCreatedLabel && (
                            <Typography variant="body2" color="text.secondary">
                              Correos registrados: {sharedEmailEventCreatedLabel}
                            </Typography>
                          )}
                          {sharedEmailEventTypeLabel && (
                            <Typography variant="body2" color="text.secondary">
                              Tipo de correo: {sharedEmailEventTypeLabel}
                            </Typography>
                          )}
                        </>
                      )}
                      {sharedEmailEventStatusLabel && !sharedEmailEventStatusIsInSummary && (
                        <Typography variant="body2" color="text.secondary">
                          Estado de correos: {sharedEmailEventStatusLabel}
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

                      {!emailEventsQuery.isLoading && !emailEventsQuery.isError && emailEvents.length === 0 && (
                        <Alert severity="info">{emptySystemEmailHistoryMessage}</Alert>
                      )}

                      {!emailEventsQuery.isLoading && !emailEventsQuery.isError && emailEvents.length > 0 && (
                        <Stack spacing={1}>
                          {emailEvents.map((entry) => {
                            const emailEventCreatedLabel = sharedEmailEventCreatedLabel ? '' : formatDate(entry.ceCreatedAt);
                            const showEmailEventMetadata = !sharedEmailEventStatusLabel
                              || !sharedEmailEventTypeLabel
                              || Boolean(emailEventCreatedLabel);
                            const emailEventMessage = entry.ceMessage ?? '';
                            const hasEmailEventMessage = emailEventMessage.trim() !== '';
                            if (!showEmailEventMetadata && !hasEmailEventMessage) return null;

                            return (
                              <Paper
                                key={entry.ceId}
                                variant="outlined"
                                sx={{ p: 1.5 }}
                                data-testid="course-registration-email-event-card"
                              >
                                {showEmailEventMetadata && (
                                  <Stack direction="row" spacing={1} alignItems="center" sx={{ mb: 0.75 }} flexWrap="wrap" useFlexGap>
                                    {!sharedEmailEventStatusLabel && (
                                      <Chip size="small" label={eventStatusLabel(entry.ceStatus)} color={eventStatusColor(entry.ceStatus)} />
                                    )}
                                    {!sharedEmailEventTypeLabel && (
                                      <Chip size="small" label={eventTypeLabel(entry.ceEventType)} variant="outlined" />
                                    )}
                                    {emailEventCreatedLabel && (
                                      <Typography variant="caption" color="text.secondary">
                                        {emailEventCreatedLabel}
                                      </Typography>
                                    )}
                                  </Stack>
                                )}
                                {hasEmailEventMessage && (
                                  <Typography
                                    variant="body2"
                                    sx={{ fontFamily: 'monospace', whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}
                                  >
                                    {emailEventMessage}
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
                  {showReceiptsSection && receiptsSection}
                  {showNotesSection && notesSection}
                </>
              ) : (
                <>
                  {showNotesSection && notesSection}
                  {showReceiptsSection && receiptsSection}
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
                      {showAddFollowUpAction && (
                        <Button
                          size="small"
                          variant="contained"
                          onClick={() => setShowFollowUpComposer(true)}
                        >
                          Agregar seguimiento
                        </Button>
                      )}
                    </Stack>
                    {sharedFollowUpSummary ? (
                      <Typography variant="body2" color="text.secondary">
                        {sharedFollowUpSummary}
                      </Typography>
                    ) : (
                      <>
                        {sharedFollowUpCreatedLabel && (
                          <Typography variant="body2" color="text.secondary">
                            Todos registrados: {sharedFollowUpCreatedLabel}
                          </Typography>
                        )}
                        {sharedFollowUpTypeLabel && (
                          <Typography variant="body2" color="text.secondary">
                            Tipo de seguimiento: {sharedFollowUpTypeLabel}
                          </Typography>
                        )}
                        {sharedFollowUpNextLabel && (
                          <Typography variant="body2" color="text.secondary">
                            Próximo seguimiento: {sharedFollowUpNextLabel}
                          </Typography>
                        )}
                      </>
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
                                  {showFollowUpUploadWidget && (
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
                                  )}
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
                                {showFollowUpSaveAction && (
                                  <Button
                                    variant="contained"
                                    onClick={handleSubmitFollowUp}
                                    disabled={createFollowUpMutation.isPending || updateFollowUpMutation.isPending || !canSubmitFollowUp}
                                  >
                                    {followUpForm.editingId == null ? 'Guardar seguimiento' : 'Actualizar seguimiento'}
                                  </Button>
                                )}
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
                                <Button
                                  size="small"
                                  variant="text"
                                  onClick={() => setShowFollowUpComposer(true)}
                                  aria-label={markPaidOptionalFollowUpAccessibleLabel}
                                  title={markPaidOptionalFollowUpAccessibleLabel}
                                >
                                  {markPaidOptionalFollowUpActionLabel}
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
                              const followUpCreatedLabel = sharedFollowUpCreatedIsInSummary
                                || sharedFollowUpCreatedLabel
                                ? ''
                                : formatOptionalDate(entry.crfCreatedAt);
                              const showFollowUpTypeChip = !sharedFollowUpTypeIsInSummary && !sharedFollowUpTypeLabel;
                              const showFollowUpNextChip = Boolean(
                                entry.crfNextFollowUpAt
                                && !sharedFollowUpNextIsInSummary
                                && !sharedFollowUpNextLabel,
                              );
                              const showFollowUpMetadata = showFollowUpTypeChip
                                || Boolean(followUpCreatedLabel)
                                || showFollowUpNextChip;
                              const trimmedFollowUpAttachmentName = entry.crfAttachmentName?.trim() ?? '';
                              const followUpAttachmentLabel = trimmedFollowUpAttachmentName
                                ? trimmedFollowUpAttachmentName
                                : `Adjunto de ${followUpActionLabel}`;
                              const isFollowUpBeingEdited = followUpForm.editingId === entry.crfId;

                              return (
                                <Paper key={entry.crfId} variant="outlined" sx={{ p: 1.5 }}>
                                  <Stack spacing={1}>
                                    <Stack
                                      direction="row"
                                      justifyContent={showFollowUpMetadata ? 'space-between' : 'flex-end'}
                                      alignItems="flex-start"
                                      flexWrap="wrap"
                                      useFlexGap
                                    >
                                      {showFollowUpMetadata && (
                                        <Stack direction="row" spacing={0.75} alignItems="center" flexWrap="wrap" useFlexGap>
                                          {showFollowUpTypeChip && (
                                            <Chip size="small" label={followUpTypeLabel(entry.crfEntryType)} variant="outlined" />
                                          )}
                                          {followUpCreatedLabel && (
                                            <Typography variant="caption" color="text.secondary">
                                              {followUpCreatedLabel}
                                            </Typography>
                                          )}
                                          {showFollowUpNextChip && (
                                            <Chip
                                              size="small"
                                              color="warning"
                                              label={`Próximo: ${formatDate(entry.crfNextFollowUpAt)}`}
                                            />
                                          )}
                                        </Stack>
                                      )}
                                      {isFollowUpBeingEdited ? (
                                        <Typography variant="body2" color="text.secondary">
                                          En edición
                                        </Typography>
                                      ) : !showFollowUpComposer ? (
                                        <IconButton
                                          size="small"
                                          title="Opciones del seguimiento"
                                          aria-label={`Abrir acciones para seguimiento ${followUpActionLabel}`}
                                          aria-haspopup="menu"
                                          onClick={(event) => handleOpenFollowUpMenu(event.currentTarget, entry)}
                                        >
                                          <MoreVertIcon fontSize="small" />
                                        </IconButton>
                                      ) : null}
                                    </Stack>
                                    {followUpSubject && (
                                      <Typography variant="subtitle2">{followUpSubject}</Typography>
                                    )}
                                    <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap' }}>
                                      {entry.crfNotes}
                                    </Typography>
                                    {entry.crfAttachmentUrl && (
                                      <Link
                                        href={entry.crfAttachmentUrl}
                                        target="_blank"
                                        rel="noopener noreferrer"
                                        aria-label={`Abrir adjunto ${followUpAttachmentLabel} en una pestaña nueva`}
                                        title={`Abrir adjunto ${followUpAttachmentLabel} en una pestaña nueva`}
                                        underline="hover"
                                      >
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
