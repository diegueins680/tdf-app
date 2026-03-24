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
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useSearchParams } from 'react-router-dom';
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
const markPaidReceiptHint = 'Sube un comprobante o pega una URL existente para habilitar Marcar pagado.';
const markPaidReceiptSectionHelpText = 'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y luego podrás marcar la inscripción como pagada.';
const emptyReceiptAlertMessage = 'Agrega el primer comprobante para documentar el pago y habilitar Marcar pagado. Cuando lo guardes aparecerá aquí con enlace y acciones para revisarlo después.';
const firstReceiptComposerHelpText = 'Este formulario ya está abierto para registrar el primer comprobante. Guárdalo y aparecerá aquí con enlace y acciones para revisarlo después.';
const initialEmptyStateMessage = 'Todavía no hay inscripciones. Cuando exista la primera, aquí aparecerán cohorte, estado y tamaño del lote para filtrar la vista.';
const dossierScopeHint = 'Expediente reúne notas, comprobantes, seguimiento y correos. Usa Estado solo para cambios rápidos.';

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

const formatRowCountLabel = (count: number) => `${count} fila${count === 1 ? '' : 's'}`;
const formatRegistrationCountLabel = (count: number) => `${count} inscripci${count === 1 ? 'ón' : 'ones'}`;

const formatDate = (iso: string | null | undefined) => formatTimestampForDisplay(iso, '-');

const isRegistrationStatus = (
  status: string,
): status is Exclude<StatusFilter, 'all'> => (
  status === 'pending_payment' || status === 'paid' || status === 'cancelled'
);

const registrationStatusLabel = (status: string) =>
  isRegistrationStatus(status) ? statusFilterLabels[status] : status.trim() || 'Estado desconocido';

const registrationStatusButtonLabel = (
  status: string,
  statusAlreadySummarized: boolean,
) => (statusAlreadySummarized ? 'Cambiar estado' : `Estado: ${registrationStatusLabel(status)}`);

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

const eventTypeLabel = (eventType: string) =>
  eventType
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

const registrationContactSummary = (email: string | null | undefined, phone: string | null | undefined) => {
  const trimmedEmail = email?.trim() ?? '';
  const trimmedPhone = phone?.trim() ?? '';
  const parts = [trimmedEmail || 'Sin correo'];
  if (trimmedPhone) parts.push(trimmedPhone);
  return parts.join(' · ');
};

const registrationListContextSummary = ({
  cohortLabel,
  createdAt,
  showCohort,
  showSource,
  source,
}: {
  cohortLabel: string;
  createdAt: string | null | undefined;
  showCohort: boolean;
  showSource: boolean;
  source: string | null | undefined;
}) => {
  const parts: string[] = [];
  if (showCohort) parts.push(`Cohorte: ${cohortLabel}`);
  if (showSource) parts.push(`Fuente: ${registrationSourceLabel(source)}`);
  parts.push(`Creado: ${formatDate(createdAt)}`);
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
  const parts = [`Curso: ${courseLabel}`];
  const trimmedSource = source?.trim() ?? '';
  if (trimmedSource) parts.push(`Fuente: ${trimmedSource}`);
  parts.push(`Creado: ${formatDate(createdAt)}`);
  return parts.join(' · ');
};

const trimToNull = (value: string): string | null => {
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
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
  const [copyMessage, setCopyMessage] = useState<string | null>(null);
  const [pageFlash, setPageFlash] = useState<FlashState | null>(null);
  const [dossierFlash, setDossierFlash] = useState<FlashState | null>(null);
  const [selectedDossier, setSelectedDossier] = useState<DossierTarget | null>(null);
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
  const selectedSlug = slug.trim();

  const listQueryKey = useMemo(
    () => ['admin', 'course-registrations', { slug, status, limit }],
    [slug, status, limit],
  );

  const selectedRegistrationId = showEmailHistory ? (selectedDossier?.reg.crId ?? null) : null;
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
  const singleAvailableCohortLabel = useMemo(() => {
    if (!cohortsQuery.data || cohortsQuery.isError || cohortOptions.length !== 1) return '';
    return cohortOptions[0]?.label ?? '';
  }, [cohortOptions, cohortsQuery.data, cohortsQuery.isError]);

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

  const singleVisibleCohortLabel = useMemo(() => {
    if (selectedSlug || !regsQuery.data || regsQuery.data.length < 2) return '';
    const uniqueCohortSlugs = Array.from(
      new Set(
        regsQuery.data
          .map((reg) => reg.crCourseSlug.trim())
          .filter((value) => value !== ''),
      ),
    );
    if (uniqueCohortSlugs.length !== 1) return '';
    const cohortSlug = uniqueCohortSlugs[0];
    if (!cohortSlug) return '';
    return cohortLabelsBySlug.get(cohortSlug) ?? cohortSlug;
  }, [cohortLabelsBySlug, regsQuery.data, selectedSlug]);
  const singleVisibleSourceLabel = useMemo(() => {
    if (!regsQuery.data || regsQuery.data.length < 2) return '';
    const uniqueSources = Array.from(
      new Set(
        regsQuery.data
          .map((reg) => registrationSourceLabel(reg.crSource)),
      ),
    );
    if (uniqueSources.length !== 1) return '';
    return uniqueSources[0] ?? '';
  }, [regsQuery.data]);
  const sharedVisibleSourceSummary = singleVisibleSourceLabel
    ? singleVisibleSourceLabel === 'Sin fuente'
      ? 'Todas las inscripciones visibles están sin fuente registrada.'
      : `Mostrando una sola fuente: ${singleVisibleSourceLabel}.`
    : '';

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

  const emailEventsQuery = useQuery<CourseEmailEventDTO[]>({
    queryKey: ['admin', 'course-registration-email-events', selectedRegistrationId],
    enabled: selectedRegistrationId != null,
    queryFn: () => {
      if (selectedRegistrationId == null) return Promise.resolve([]);
      return Courses.listRegistrationEmails(selectedRegistrationId, 200);
    },
    staleTime: 30_000,
  });

  const statusCounts = useMemo(() => {
    const base = { total: 0, pending_payment: 0, paid: 0, cancelled: 0 };
    if (!regsQuery.data) return base;
    return regsQuery.data.reduce(
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
  }, [regsQuery.data]);
  const hasVisibleRegistrations = (regsQuery.data?.length ?? 0) > 0;
  const visibleStatusFilters = useMemo<readonly StatusFilter[]>(() => {
    if (!hasVisibleRegistrations) return statusFilters;
    return statusFilters.filter((value) => value === 'all' || status === value || statusCounts[value] > 0);
  }, [hasVisibleRegistrations, status, statusCounts]);
  const hasHiddenStatusFilters = visibleStatusFilters.length < statusFilters.length;
  const singleVisibleStatus = useMemo<Exclude<StatusFilter, 'all'> | null>(() => {
    if (!hasVisibleRegistrations) return null;
    const realStatuses = visibleStatusFilters.filter((value): value is Exclude<StatusFilter, 'all'> => value !== 'all');
    return realStatuses.length === 1 ? (realStatuses[0] ?? null) : null;
  }, [hasVisibleRegistrations, visibleStatusFilters]);

  const showSingleStatusSummary = Boolean(
    singleVisibleStatus && (status === 'all' || status === singleVisibleStatus),
  );
  const hasCustomFilters = slug.trim() !== '' || status !== 'all' || limit !== DEFAULT_LIMIT;
  const activeFilterSummary = useMemo(
    () => summarizeActiveFilters({ cohortLabel: activeCohortLabel, status, limit }),
    [activeCohortLabel, status, limit],
  );
  const combinedSingleChoiceSummary = singleAvailableCohortLabel && showSingleStatusSummary && singleVisibleStatus
    ? `${singleAvailableCohortLabel} · ${statusFilterLabels[singleVisibleStatus]}`
    : '';
  const combinedSingleChoiceLimitSummary = combinedSingleChoiceSummary && limit !== DEFAULT_LIMIT
    ? `Límite actual: hasta ${limit} inscripci${limit === 1 ? 'ón' : 'ones'}.`
    : '';
  const summarizedVisibleSourceLabel = singleVisibleSourceLabel
    ? singleVisibleSourceLabel === 'Sin fuente'
      ? 'Fuente visible: sin fuente registrada.'
      : `Fuente visible: ${singleVisibleSourceLabel}.`
    : '';
  const combinedSingleChoiceSourceSummary = combinedSingleChoiceSummary
    ? summarizedVisibleSourceLabel
    : '';
  const standaloneSingleChoiceSourceSummary = !combinedSingleChoiceSummary && (singleAvailableCohortLabel || showSingleStatusSummary)
    ? summarizedVisibleSourceLabel
    : '';
  const filteredEmptyStateMessage = activeFilterSummary
    ? `No hay inscripciones con los filtros actuales: ${activeFilterSummary}. Restablece filtros o usa refrescar si esperabas resultados.`
    : 'No hay inscripciones con los filtros actuales. Restablece filtros o usa refrescar si esperabas resultados.';
  const canCopyCsv = (regsQuery.data?.length ?? 0) > 1;
  const showListUtilitySummary = canCopyCsv || Boolean(copyMessage);
  const shouldShowSharedCohortSummary = !hasCustomFilters && Boolean(singleVisibleCohortLabel) && !singleAvailableCohortLabel;
  const hasSharedVisibleSource = Boolean(singleVisibleSourceLabel);
  const shouldShowSharedSourceSummary = hasSharedVisibleSource
    && !combinedSingleChoiceSourceSummary
    && !standaloneSingleChoiceSourceSummary;
  const loadedRegistrationCount = regsQuery.data?.length ?? 0;
  const visibleRegistrationsSummary = hasCustomFilters
    ? `Mostrando ${formatRegistrationCountLabel(loadedRegistrationCount)} con los filtros actuales.`
    : `Mostrando ${formatRegistrationCountLabel(loadedRegistrationCount)} en esta vista.`;
  const copyCsvButtonLabel = hasCustomFilters ? 'Copiar CSV filtrado' : 'Copiar CSV';
  const viewHitsCurrentLimit = hasVisibleRegistrations && loadedRegistrationCount >= limit;
  const showAdvancedLimitControl = viewHitsCurrentLimit || limit !== DEFAULT_LIMIT;
  const visibleActiveFilterSummary = useMemo(() => {
    const parts: string[] = [];
    const cohortAlreadyExplained = Boolean(combinedSingleChoiceSummary || singleAvailableCohortLabel);
    const statusAlreadyExplained = Boolean(combinedSingleChoiceSummary || showSingleStatusSummary);
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
    status,
  ]);
  const showInitialFilterGuidance = !regsQuery.isLoading
    && !regsQuery.isError
    && !cohortsQuery.isError
    && !hasCustomFilters
    && !hasVisibleRegistrations;
  const limitToggleLabel = showAdvancedFilters
    ? 'Ocultar límite'
    : limit !== DEFAULT_LIMIT
      ? `Ajustar límite (${limit})`
      : 'Ajustar límite';
  const filtersHelpText = combinedSingleChoiceSummary
    ? showAdvancedLimitControl
      ? 'Esta vista ya está acotada a una cohorte y un estado. Usa Ajustar límite solo cuando necesites revisar un lote distinto.'
      : ''
    : showAdvancedLimitControl
      ? hasVisibleRegistrations
        ? 'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; usa Ajustar límite solo cuando necesites revisar un lote distinto.'
        : 'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; usa Ajustar límite solo cuando necesites revisar un lote distinto. Ajusta la vista o usa refrescar si esperabas resultados.'
      : hasVisibleRegistrations
      ? 'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado.'
      : 'Los filtros se aplican automáticamente al cambiar. Empieza por cohorte y estado; Ajustar límite aparecerá cuando esta vista llene el lote actual o si ya estás usando un límite personalizado. Ajusta la vista o usa refrescar si esperabas resultados.';
  const showFilteredEmptyState = !regsQuery.isLoading
    && !regsQuery.isError
    && hasCustomFilters
    && !hasVisibleRegistrations;

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
    setDossierFlash(
      selectedDossier.intent === 'markPaid'
        ? {
            severity: 'info',
            message: markPaidReceiptHint,
          }
        : null,
    );
  }, [selectedDossier]);

  useEffect(() => {
    setNotesDraft(dossierQuery.data?.crdRegistration.crAdminNotes ?? '');
  }, [dossierQuery.data?.crdRegistration.crId, dossierQuery.data?.crdRegistration.crAdminNotes]);

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'course-registrations'] });
  };

  const handleRefreshDossier = () => {
    if (!selectedDossier) return;
    const requests: Array<Promise<unknown>> = [dossierQuery.refetch()];
    if (showEmailHistory && selectedRegistrationId != null) {
      requests.push(emailEventsQuery.refetch());
    }
    void Promise.all(requests);
  };

  const handleCopyCsv = async () => {
    const registrations = regsQuery.data;
    if (!registrations || registrations.length < 2) return;
    const header = ['id', 'slug', 'nombre', 'email', 'estado', 'creado'];
    const rows = registrations.map((reg) => [
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
    setSlug('');
    setStatus('all');
    setLimit(DEFAULT_LIMIT);
    setShowAdvancedFilters(false);
  };

  const handleOpenStatusMenu = (anchorEl: HTMLElement, reg: CourseRegistrationDTO) => {
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
          message: `Estado actualizado para ${reg.crFullName ?? `registro #${reg.crId}`}.`,
        });
      })
      .catch((err: Error) => {
        setPageFlash({ severity: 'error', message: err.message });
      });
  };

  const handleOpenDossier = (reg: CourseRegistrationDTO, intent: DossierIntent) => {
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
        setDossierFlash({ severity: 'success', message: 'Inscripción marcada como pagada.' });
      })
      .catch((err: Error) => {
        setDossierFlash({ severity: 'error', message: err.message });
      });
  };

  const dossierData = dossierQuery.data;
  const activeRegistration = dossierData?.crdRegistration ?? selectedDossier?.reg ?? null;
  const receipts = dossierData?.crdReceipts ?? [];
  const followUps = dossierData?.crdFollowUps ?? [];
  const persistedNotes = trimToNull(getPersistedNotesValue());
  const hasSavedNotes = Boolean(persistedNotes);
  const hasNotesDraftChanges = trimToNull(notesDraft) !== persistedNotes;
  const notesActionLabel = hasSavedNotes ? 'Editar notas' : 'Agregar nota';
  const canMarkPaid = dossierData?.crdCanMarkPaid ?? false;
  const hasReceipts = receipts.length > 0;
  const showReceiptCountChip = receipts.length > 1;
  const canSubmitReceipt = Boolean(trimToNull(receiptForm.fileUrl));
  const receiptSectionHelpText = (
    selectedDossier?.intent === 'markPaid'
    && showReceiptComposer
    && !hasReceipts
  )
    ? markPaidReceiptSectionHelpText
    : showReceiptComposer && !hasReceipts
      ? firstReceiptComposerHelpText
      : hasReceipts
        ? 'Abre el formulario solo cuando necesites guardar un comprobante o pegar un enlace existente.'
        : '';
  const showReceiptMetadataFields = (
    selectedDossier?.intent === 'markPaid'
    || receiptForm.editingId != null
    || showReceiptUrlField
    || Boolean(trimToNull(receiptForm.fileName))
    || canSubmitReceipt
  );
  const hasFollowUpOptionalDetails = (
    followUpForm.editingId != null
    || showFollowUpUrlField
    || Boolean(trimToNull(followUpForm.subject))
    || Boolean(trimToNull(followUpForm.attachmentName))
    || Boolean(trimToNull(followUpForm.attachmentUrl))
    || followUpForm.nextFollowUpAt.trim() !== ''
  );
  const showFollowUpOptionalFields = showFollowUpDetails || hasFollowUpOptionalDetails;
  const showFollowUpCountChip = followUps.length > 1;
  const currentMutationRegistrationId = updateStatusMutation.variables?.id ?? null;
  const statusMenuReg = statusMenuTarget?.reg ?? null;
  const receiptMenuReceipt = receiptMenuTarget?.receipt ?? null;
  const followUpMenuEntry = followUpMenuTarget?.entry ?? null;
  const activeRegistrationCourseSlug = activeRegistration?.crCourseSlug.trim() ?? '';
  const activeRegistrationCourseLabel = activeRegistrationCourseSlug
    ? (cohortLabelsBySlug.get(activeRegistrationCourseSlug) ?? activeRegistrationCourseSlug)
    : 'Sin cohorte';
  const activeRegistrationSummary = activeRegistration
    ? registrationDossierContextSummary({
      courseLabel: activeRegistrationCourseLabel,
      createdAt: activeRegistration.crCreatedAt,
      source: activeRegistration.crSource,
    })
    : '';
  const showPartyIdFallback = Boolean(
    activeRegistration?.crPartyId
    && !activeRegistration?.crFullName?.trim()
    && !activeRegistration?.crEmail?.trim()
    && !activeRegistration?.crPhoneE164?.trim(),
  );
  const isRefreshingDossier = dossierQuery.isFetching || (showEmailHistory && emailEventsQuery.isFetching);
  const dossierRefreshLabel = showEmailHistory ? 'Refrescar expediente y correos' : 'Refrescar expediente';

  return (
    <Stack spacing={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" flexWrap="wrap" useFlexGap>
        <Typography variant="h4" fontWeight={700}>
          Inscripciones de cursos
        </Typography>
        <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
          <Tooltip title="Refrescar">
            <span>
              <IconButton aria-label="Refrescar inscripciones" onClick={handleRefresh} disabled={regsQuery.isFetching}>
                <RefreshIcon />
              </IconButton>
            </span>
          </Tooltip>
        </Stack>
      </Stack>

      {pageFlash && <Alert severity={pageFlash.severity}>{pageFlash.message}</Alert>}

      <Paper sx={{ p: 3, borderRadius: 3 }}>
        {showInitialFilterGuidance ? (
          <Alert severity="info" variant="outlined">
            {initialEmptyStateMessage}
          </Alert>
        ) : (
          <>
            <Grid container spacing={2}>
              {combinedSingleChoiceSummary ? (
                <Grid item xs={12}>
                  <Stack
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
                    {combinedSingleChoiceSourceSummary && (
                      <Typography variant="caption" color="text.secondary">
                        {combinedSingleChoiceSourceSummary}
                      </Typography>
                    )}
                    {combinedSingleChoiceLimitSummary && (
                      <Typography variant="caption" color="text.secondary">
                        {combinedSingleChoiceLimitSummary}
                      </Typography>
                    )}
                    <Typography variant="caption" color="text.secondary">
                      No hace falta filtrar cohorte ni estado: esta vista solo tiene una cohorte y un estado por ahora.
                    </Typography>
                  </Stack>
                </Grid>
              ) : (
                <>
                  <Grid item xs={12} md={6}>
                    {singleAvailableCohortLabel ? (
                      <Stack
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
                        <Typography variant="caption" color="text.secondary">
                          No hace falta filtrarla: es la unica cohorte disponible ahora mismo.
                        </Typography>
                      </Stack>
                    ) : (
                      <TextField
                        select
                        label="Curso / cohorte"
                        value={slug}
                        onChange={(e) => setSlug(e.target.value)}
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
                        <Typography variant="caption" color="text.secondary">
                          No hace falta filtrarlo: es el unico estado presente en esta vista.
                        </Typography>
                      </Stack>
                    ) : (
                      <Stack spacing={1}>
                        <Typography variant="caption" color="text.secondary">
                          Estado
                        </Typography>
                        <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                          {visibleStatusFilters.map((value) => (
                            <Chip
                              key={value}
                              clickable
                              color={registrationStatusChipColor(value)}
                              label={statusFilterChipLabel(value, statusCounts, hasVisibleRegistrations)}
                              variant={status === value ? 'filled' : 'outlined'}
                              aria-label={`Filtrar inscripciones por estado ${statusFilterLabels[value]}`}
                              aria-pressed={status === value}
                              onClick={() => setStatus(value)}
                            />
                          ))}
                        </Stack>
                        {hasHiddenStatusFilters && (
                          <Typography variant="caption" color="text.secondary">
                            Solo aparecen estados con inscripciones en esta vista.
                          </Typography>
                        )}
                      </Stack>
                    )}
                  </Grid>
                </>
              )}
            </Grid>
            {showAdvancedLimitControl && (
              <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
                <Button
                  size="small"
                  variant="text"
                  onClick={() => setShowAdvancedFilters((current) => !current)}
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
                  onChange={(e) => setLimit(parsePositiveLimit(e.target.value, DEFAULT_LIMIT))}
                  helperText="Máximo de filas a cargar en esta vista. Déjalo en 200 salvo que necesites revisar un lote distinto."
                  fullWidth
                  size="small"
                />
              </Box>
            </Collapse>
            {filtersHelpText && !showFilteredEmptyState && (
              <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mt: 2 }}>
                {filtersHelpText}
              </Typography>
            )}
            {hasCustomFilters && hasVisibleRegistrations && (
              <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 1.5 }} flexWrap="wrap" useFlexGap>
                {visibleActiveFilterSummary && (
                  <Typography variant="body2" color="text.secondary">
                    Vista filtrada: {visibleActiveFilterSummary}.
                  </Typography>
                )}
                <Button size="small" onClick={handleResetFilters}>
                  Restablecer filtros
                </Button>
              </Stack>
            )}
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
            {hasVisibleRegistrations && showListUtilitySummary && (
              <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
                <Typography variant="body2" color="text.secondary">
                  {visibleRegistrationsSummary}
                </Typography>
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
        )}
      </Paper>

      {!showInitialFilterGuidance && (
        <Paper sx={{ p: 3, borderRadius: 3 }}>
          {regsQuery.isError && (
            <Typography color="error">
              No se pudieron cargar las inscripciones: {regsQuery.error instanceof Error ? regsQuery.error.message : 'Error'}
            </Typography>
          )}
          {regsQuery.isLoading && <Typography>Cargando inscripciones…</Typography>}
          {!regsQuery.isLoading && regsQuery.data?.length === 0 && (
            hasCustomFilters ? (
              <Alert
                severity="info"
                action={(
                  <Button color="inherit" size="small" onClick={handleResetFilters}>
                    Restablecer filtros
                  </Button>
                )}
              >
                {filteredEmptyStateMessage}
              </Alert>
            ) : (
              <Typography color="text.secondary">Todavía no hay inscripciones para mostrar en esta vista.</Typography>
            )
          )}
          {regsQuery.data?.length ? (
            <Stack spacing={1.5}>
              <Typography variant="body2" color="text.secondary">
                {dossierScopeHint}
              </Typography>
              <Stack divider={<Divider flexItem />} spacing={2}>
                {regsQuery.data.map((reg) => {
                  const isUpdating = updateStatusMutation.isPending && currentMutationRegistrationId === reg.crId;
                  const rowCohortSlug = reg.crCourseSlug.trim();
                  const rowCohortLabel = cohortLabelsBySlug.get(rowCohortSlug) ?? rowCohortSlug;
                  const showRowCohort = selectedSlug
                    ? rowCohortSlug !== selectedSlug
                    : !(singleVisibleCohortLabel || singleAvailableCohortLabel);
                  const showRowSource = !hasSharedVisibleSource;
                  const rowContextSummary = registrationListContextSummary({
                    cohortLabel: rowCohortLabel,
                    createdAt: reg.crCreatedAt,
                    showCohort: showRowCohort,
                    showSource: showRowSource,
                    source: reg.crSource,
                  });
                  return (
                    <Box key={reg.crId} sx={{ display: 'flex', gap: 2, alignItems: 'center', flexWrap: 'wrap' }}>
                      <Box sx={{ minWidth: 240 }}>
                        <Typography variant="subtitle1" fontWeight={700}>
                          {reg.crFullName ?? 'Sin nombre'}
                        </Typography>
                        <Typography variant="body2" color="text.secondary">
                          {registrationContactSummary(reg.crEmail, reg.crPhoneE164)}
                        </Typography>
                        {reg.crAdminNotes && <Chip size="small" label="Con notas" variant="outlined" sx={{ mt: 1 }} />}
                      </Box>
                      <Box sx={{ minWidth: 180 }}>
                        <Typography variant="body2" color="text.secondary">
                          {rowContextSummary}
                        </Typography>
                      </Box>
                      <Button
                        size="small"
                        variant="outlined"
                        aria-label={`Abrir expediente de ${reg.crFullName ?? reg.crEmail ?? `registro #${reg.crId}`}`}
                        onClick={() => handleOpenDossier(reg, 'review')}
                      >
                        Expediente
                      </Button>
                      <Button
                        size="small"
                        variant="outlined"
                        color={registrationStatusButtonColor(reg.crStatus)}
                        endIcon={<ArrowDropDownIcon />}
                        aria-label={`Cambiar estado para ${reg.crFullName ?? reg.crEmail ?? 'esta inscripción'}`}
                        aria-haspopup="menu"
                        disabled={isUpdating}
                        onClick={(event) => handleOpenStatusMenu(event.currentTarget, reg)}
                      >
                        {registrationStatusButtonLabel(reg.crStatus, showSingleStatusSummary)}
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
        {statusMenuReg && canTransitionToStatus(statusMenuReg.crStatus, 'paid') && (
          <MenuItem
            onClick={() => {
              handleCloseStatusMenu();
              handleOpenDossier(statusMenuReg, 'markPaid');
            }}
          >
            Subir comprobante para marcar pagado
          </MenuItem>
        )}
        {statusMenuReg && canTransitionToStatus(statusMenuReg.crStatus, 'pending_payment') && (
          <MenuItem
            onClick={() => {
              handleCloseStatusMenu();
              handleQuickStatus(statusMenuReg, 'pending_payment');
            }}
          >
            Marcar pendiente
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
            <span>Expediente de inscripción</span>
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

          {activeRegistration && !dossierQuery.isError && (
            <Stack spacing={2.5}>
              {dossierFlash && <Alert severity={dossierFlash.severity}>{dossierFlash.message}</Alert>}

              <Paper variant="outlined" sx={{ p: 2 }}>
                <Stack spacing={1.5}>
                  <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                    <Typography variant="h6">{activeRegistration.crFullName ?? 'Sin nombre'}</Typography>
                    {statusChip(activeRegistration.crStatus)}
                    {showPartyIdFallback && (
                      <Chip size="small" label={`Party #${activeRegistration.crPartyId}`} variant="outlined" />
                    )}
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    {registrationContactSummary(activeRegistration.crEmail, activeRegistration.crPhoneE164)}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    {activeRegistrationSummary}
                  </Typography>
                  <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                    {canMarkPaid && (
                      <Button
                        variant="contained"
                        color="success"
                        onClick={handleMarkPaidFromDossier}
                        disabled={updateStatusMutation.isPending}
                      >
                        Marcar pagado
                      </Button>
                    )}
                    <Button
                      variant="outlined"
                      onClick={() => setShowEmailHistory((current) => !current)}
                      aria-expanded={showEmailHistory}
                    >
                      {showEmailHistory ? 'Ocultar correos' : 'Ver correos'}
                    </Button>
                  </Stack>
                </Stack>
              </Paper>

              <Collapse in={showEmailHistory} unmountOnExit>
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
                          <Typography variant="h6">Historial de correos</Typography>
                          <Typography variant="body2" color="text.secondary">
                            Historial persistente por inscripción. Usa el refresco del expediente para volver a
                            consultarlo.
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
                        <Alert severity="info">No hay correos registrados para esta inscripción.</Alert>
                      )}

                      {!emailEventsQuery.isLoading && !emailEventsQuery.isError && (emailEventsQuery.data?.length ?? 0) > 0 && (
                        <Stack spacing={1}>
                          {(emailEventsQuery.data ?? []).map((entry) => (
                            <Paper key={entry.ceId} variant="outlined" sx={{ p: 1.5 }}>
                              <Stack direction="row" spacing={1} alignItems="center" sx={{ mb: 0.75 }} flexWrap="wrap" useFlexGap>
                                <Chip size="small" label={entry.ceStatus} color={eventStatusColor(entry.ceStatus)} />
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

              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={1.5}>
                    <Stack direction="row" alignItems="center" justifyContent="space-between" flexWrap="wrap" useFlexGap>
                      <Typography variant="h6">Notas internas</Typography>
                      {!showNotesComposer ? (
                        <Button
                          variant={hasSavedNotes ? 'contained' : 'outlined'}
                          size="small"
                          onClick={handleOpenNotesComposer}
                        >
                          {notesActionLabel}
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
                      <Typography variant="body2" color="text.secondary">
                        Aún no hay notas internas. Úsalas solo cuando necesites dejar contexto, acuerdos o próximos pasos.
                      </Typography>
                    )}
                  </Stack>
                </CardContent>
              </Card>

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
                      {!showReceiptComposer && hasReceipts && (
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
                        <Grid item xs={12} md={6}>
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
                                onClick={() => setShowReceiptUrlField(true)}
                              >
                                Usar enlace existente en lugar de subir archivo
                              </Button>
                            )}
                            <Collapse in={showReceiptUrlField} unmountOnExit>
                              <TextField
                                label="URL del comprobante"
                                value={receiptForm.fileUrl}
                                onChange={(e) => setReceiptForm((prev) => ({ ...prev, fileUrl: e.target.value }))}
                                placeholder="Pega un enlace existente si el archivo ya está cargado"
                                fullWidth
                              />
                            </Collapse>
                            {!showReceiptMetadataFields && (
                              <Typography variant="caption" color="text.secondary">
                                Primero elige el archivo o pega un enlace; luego podras ajustar el nombre visible y
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
                              <Button variant="text" onClick={() => resetReceiptComposer()}>
                                {receiptForm.editingId == null ? 'Cancelar comprobante' : 'Cancelar edición de comprobante'}
                              </Button>
                            </Stack>
                          </Stack>
                        </Grid>
                      )}
                      <Grid item xs={12} md={showReceiptComposer ? 6 : 12}>
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
                              {emptyReceiptAlertMessage}
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
                                    <Typography variant="subtitle2">
                                      {receipt.crrFileName ?? `Comprobante #${receipt.crrId}`}
                                    </Typography>
                                    <Typography variant="caption" color="text.secondary">
                                      Subido: {formatDate(receipt.crrCreatedAt)}
                                    </Typography>
                                  </Box>
                                  <Button
                                    size="small"
                                    variant="text"
                                    endIcon={<ArrowDropDownIcon />}
                                    aria-label={`Abrir acciones para comprobante ${receipt.crrFileName ?? `comprobante ${receipt.crrId}`}`}
                                    aria-haspopup="menu"
                                    onClick={(event) => handleOpenReceiptMenu(event.currentTarget, receipt)}
                                  >
                                    Acciones
                                  </Button>
                                </Stack>
                                {receipt.crrNotes && (
                                  <Typography variant="body2" color="text.secondary">
                                    {receipt.crrNotes}
                                  </Typography>
                                )}
                                <Link href={receipt.crrFileUrl} target="_blank" rel="noreferrer" underline="hover">
                                  <Stack direction="row" spacing={0.75} alignItems="center">
                                    <OpenInNewIcon sx={{ fontSize: 16 }} />
                                    <span>Abrir comprobante</span>
                                  </Stack>
                                </Link>
                              </Stack>
                            </Paper>
                          ))}
                        </Stack>
                      </Grid>
                    </Grid>
                  </Stack>
                </CardContent>
              </Card>

              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={2}>
                    <Stack direction="row" alignItems="center" justifyContent="space-between" flexWrap="wrap" useFlexGap>
                      <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                        <Typography variant="h6">Historial de seguimiento</Typography>
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
                        <Grid item xs={12} md={6}>
                          <Stack spacing={1.5}>
                            <Box sx={{ minWidth: 240, flexGrow: 1 }}>
                              <Typography variant="subtitle2">
                                {followUpForm.editingId == null ? 'Registrar seguimiento' : 'Editar seguimiento'}
                              </Typography>
                              <Typography variant="body2" color="text.secondary">
                                Abre el formulario solo cuando necesites documentar una llamada, correo o
                                próximo paso.
                              </Typography>
                            </Box>
                            <Stack spacing={1.5} sx={{ pt: 0.5 }}>
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
                                    Agrega asunto, recordatorio o evidencia solo si hacen falta.
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
                                      onClick={() => setShowFollowUpUrlField(true)}
                                    >
                                      Usar enlace existente en lugar de subir adjunto
                                    </Button>
                                  )}
                                  <Collapse in={showFollowUpUrlField} unmountOnExit>
                                    <TextField
                                      label="URL del adjunto"
                                      value={followUpForm.attachmentUrl}
                                      onChange={(e) => setFollowUpForm((prev) => ({ ...prev, attachmentUrl: e.target.value }))}
                                      fullWidth
                                    />
                                  </Collapse>
                                </Stack>
                              </Collapse>
                              <Stack direction="row" spacing={1}>
                                <Button
                                  variant="contained"
                                  onClick={handleSubmitFollowUp}
                                  disabled={createFollowUpMutation.isPending || updateFollowUpMutation.isPending}
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
                      <Grid item xs={12} md={showFollowUpComposer ? 6 : 12}>
                        <Stack spacing={1.5}>
                          {followUps.length === 0 && !showFollowUpComposer && (
                            <Alert
                              severity="info"
                              action={(
                                <Button color="inherit" size="small" onClick={() => setShowFollowUpComposer(true)}>
                                  Registrar primer seguimiento
                                </Button>
                              )}
                            >
                              Aún no hay seguimiento manual. Documenta llamadas, correos o próximos pasos desde
                              aquí. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.
                            </Alert>
                          )}
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
                                  <Button
                                    size="small"
                                    variant="text"
                                    endIcon={<ArrowDropDownIcon />}
                                    aria-label={`Abrir acciones para seguimiento ${followUpActionTargetLabel(entry)}`}
                                    aria-haspopup="menu"
                                    onClick={(event) => handleOpenFollowUpMenu(event.currentTarget, entry)}
                                  >
                                    Acciones
                                  </Button>
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
                    </Grid>
                  </Stack>
                </CardContent>
              </Card>
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setSelectedDossier(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>

    </Stack>
  );
}
