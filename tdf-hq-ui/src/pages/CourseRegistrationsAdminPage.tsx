import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  Grid,
  IconButton,
  Link,
  MenuItem,
  Paper,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import DoneIcon from '@mui/icons-material/Done';
import CancelIcon from '@mui/icons-material/Cancel';
import PendingIcon from '@mui/icons-material/HourglassBottom';
import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import EditIcon from '@mui/icons-material/Edit';
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

const parsePositiveLimit = (value: string | null, fallback = 200): number => {
  const trimmed = value?.trim() ?? '';
  if (!/^\d+$/.test(trimmed)) return fallback;
  const parsed = Number(trimmed);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : fallback;
};

const formatDate = (iso: string | null | undefined) => formatTimestampForDisplay(iso, '-');

const statusChip = (status: string) => {
  const normalized = status.toLowerCase();
  if (normalized === 'paid') return <Chip label="Pagado" color="success" size="small" />;
  if (normalized === 'cancelled') return <Chip label="Cancelado" color="error" size="small" />;
  return <Chip label="Pendiente de pago" color="warning" size="small" />;
};

const isRegistrationStatus = (
  status: string,
): status is Exclude<StatusFilter, 'all'> => (
  status === 'pending_payment' || status === 'paid' || status === 'cancelled'
);

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

const cohortOptionLabel = (cohort: CourseCohortOptionDTO) => {
  const slug = cohort.ccSlug.trim();
  const title = cohort.ccTitle?.trim();
  if (!title) return slug;
  return `${title} (${slug})`;
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
  const [copyMessage, setCopyMessage] = useState<string | null>(null);
  const [pageFlash, setPageFlash] = useState<FlashState | null>(null);
  const [dossierFlash, setDossierFlash] = useState<FlashState | null>(null);
  const [selectedRegForEmails, setSelectedRegForEmails] =
    useState<CourseRegistrationDTO | null>(null);
  const [selectedDossier, setSelectedDossier] = useState<DossierTarget | null>(null);
  const [notesDraft, setNotesDraft] = useState('');
  const [receiptForm, setReceiptForm] = useState<ReceiptFormState>(emptyReceiptForm);
  const [followUpForm, setFollowUpForm] = useState<FollowUpFormState>(emptyFollowUpForm);

  const listQueryKey = useMemo(
    () => ['admin', 'course-registrations', { slug, status, limit }],
    [slug, status, limit],
  );

  const selectedRegistrationId = selectedRegForEmails?.crId;
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

  const cohortOptions = useMemo(() => {
    const bySlug = new Map<string, string>();
    for (const cohort of cohortsQuery.data ?? []) {
      const cohortSlug = cohort.ccSlug.trim();
      if (!cohortSlug || bySlug.has(cohortSlug)) continue;
      bySlug.set(cohortSlug, cohortOptionLabel(cohort));
    }
    const selectedSlug = slug.trim();
    if (selectedSlug && !bySlug.has(selectedSlug)) {
      bySlug.set(selectedSlug, selectedSlug);
    }
    return Array.from(bySlug.entries()).map(([value, label]) => ({ value, label }));
  }, [cohortsQuery.data, slug]);

  const regsQuery = useQuery({
    queryKey: listQueryKey,
    queryFn: () =>
      Courses.listRegistrations({
        slug: slug.trim() || undefined,
        status: status === 'all' ? undefined : status,
        limit,
      }),
  });

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
    if (limit && limit !== 200) params.set('limit', String(limit));
    setSearchParams(params, { replace: true });
  }, [slug, status, limit, setSearchParams]);

  useEffect(() => {
    if (!selectedDossier) {
      setDossierFlash(null);
      setNotesDraft('');
      setReceiptForm(emptyReceiptForm());
      setFollowUpForm(emptyFollowUpForm());
      return;
    }
    setReceiptForm(emptyReceiptForm());
    setFollowUpForm(emptyFollowUpForm());
    setDossierFlash(
      selectedDossier.intent === 'markPaid'
        ? {
            severity: 'info',
            message: 'Para marcar esta inscripción como pagada, primero sube o pega un comprobante.',
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

  const handleCopyCsv = async () => {
    if (!regsQuery.data?.length) return;
    const header = ['id', 'slug', 'nombre', 'email', 'estado', 'creado'];
    const rows = regsQuery.data.map((reg) => [
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
      setCopyMessage(`Copiado CSV (${rows.length} filas)`);
      setTimeout(() => setCopyMessage(null), 2000);
    } catch {
      setCopyMessage('No se pudo copiar el CSV');
      setTimeout(() => setCopyMessage(null), 2000);
    }
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
        setReceiptForm(emptyReceiptForm());
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
    setReceiptForm({
      editingId: receipt.crrId,
      fileUrl: receipt.crrFileUrl,
      fileName: receipt.crrFileName ?? '',
      notes: receipt.crrNotes ?? '',
    });
  };

  const handleDeleteReceipt = (receipt: CourseRegistrationReceiptDTO) => {
    if (!selectedDossier || selectedDossierId == null) return;
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
          setReceiptForm(emptyReceiptForm());
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
        setFollowUpForm(emptyFollowUpForm());
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
    setFollowUpForm({
      editingId: entry.crfId,
      entryType: entry.crfEntryType,
      subject: entry.crfSubject ?? '',
      notes: entry.crfNotes,
      attachmentUrl: entry.crfAttachmentUrl ?? '',
      attachmentName: entry.crfAttachmentName ?? '',
      nextFollowUpAt: toLocalDateTimeInputValue(entry.crfNextFollowUpAt),
    });
  };

  const handleDeleteFollowUp = (entry: CourseRegistrationFollowUpDTO) => {
    if (!selectedDossier || selectedDossierId == null) return;
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
          setFollowUpForm(emptyFollowUpForm());
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
  const canMarkPaid = dossierData?.crdCanMarkPaid ?? false;
  const currentMutationRegistrationId = updateStatusMutation.variables?.id ?? null;

  return (
    <Stack spacing={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" flexWrap="wrap" useFlexGap>
        <Typography variant="h4" fontWeight={700}>
          Inscripciones de cursos
        </Typography>
        <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
          <Chip label={`Total: ${statusCounts.total}`} size="small" />
          <Chip label={`Pagadas: ${statusCounts.paid}`} size="small" color="success" variant="outlined" />
          <Chip label={`Pendientes: ${statusCounts.pending_payment}`} size="small" color="warning" variant="outlined" />
          <Chip label={`Canceladas: ${statusCounts.cancelled}`} size="small" color="error" variant="outlined" />
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
        <Grid container spacing={2}>
          <Grid item xs={12} md={5}>
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
          </Grid>
          <Grid item xs={12} md={4}>
            <TextField
              select
              label="Estado"
              value={status}
              onChange={(e) => setStatus(parseStatusFilter(e.target.value))}
              fullWidth
              size="small"
            >
              <MenuItem value="all">Todos</MenuItem>
              <MenuItem value="pending_payment">Pendiente de pago</MenuItem>
              <MenuItem value="paid">Pagado</MenuItem>
              <MenuItem value="cancelled">Cancelado</MenuItem>
            </TextField>
          </Grid>
          <Grid item xs={12} md={3}>
            <TextField
              label="Límite"
              type="number"
              inputProps={{ min: 1 }}
              value={limit}
              onChange={(e) => setLimit(parsePositiveLimit(e.target.value, 100))}
              fullWidth
              size="small"
            />
          </Grid>
        </Grid>
        <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mt: 2 }}>
          Los filtros se aplican automáticamente al cambiar. Abre el expediente para gestionar notas,
          comprobantes, seguimiento y correos. Usa refrescar si necesitas volver a consultar.
        </Typography>
        <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mt: 0.75 }}>
          Cada fila solo muestra cambios de estado disponibles; el estado actual ya queda marcado en su chip.
        </Typography>
        <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
          <Typography variant="caption" color="text.secondary">
            Leyenda de estados:
          </Typography>
          <Chip label="Pagado" size="small" color="success" />
          <Chip label="Pendiente" size="small" color="warning" />
          <Chip label="Cancelado" size="small" color="error" />
          <Button
            size="small"
            startIcon={<ContentCopyIcon fontSize="small" />}
            onClick={() => void handleCopyCsv()}
            disabled={!regsQuery.data?.length}
          >
            Copiar CSV filtrado
          </Button>
          {copyMessage && (
            <Typography variant="caption" color="text.secondary">
              {copyMessage}
            </Typography>
          )}
        </Stack>
      </Paper>

      <Paper sx={{ p: 3, borderRadius: 3 }}>
        {regsQuery.isError && (
          <Typography color="error">
            No se pudieron cargar las inscripciones: {regsQuery.error instanceof Error ? regsQuery.error.message : 'Error'}
          </Typography>
        )}
        {regsQuery.isLoading && <Typography>Cargando inscripciones…</Typography>}
        {!regsQuery.isLoading && regsQuery.data?.length === 0 && (
          <Typography color="text.secondary">No hay inscripciones para estos filtros.</Typography>
        )}
        {regsQuery.data?.length ? (
          <Stack divider={<Divider flexItem />} spacing={2}>
            {regsQuery.data.map((reg) => {
              const isUpdating = updateStatusMutation.isPending && currentMutationRegistrationId === reg.crId;
              return (
                <Box key={reg.crId} sx={{ display: 'flex', gap: 2, alignItems: 'center', flexWrap: 'wrap' }}>
                  <Box sx={{ minWidth: 240 }}>
                    <Typography variant="subtitle1" fontWeight={700}>
                      {reg.crFullName ?? 'Sin nombre'}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {reg.crEmail ?? 'Sin correo'}
                    </Typography>
                    {reg.crPhoneE164 && (
                      <Typography variant="body2" color="text.secondary">
                        {reg.crPhoneE164}
                      </Typography>
                    )}
                    {reg.crAdminNotes && <Chip size="small" label="Con notas" variant="outlined" sx={{ mt: 1 }} />}
                  </Box>
                  <Box sx={{ minWidth: 180 }}>
                    <Typography variant="body2">Slug: {reg.crCourseSlug}</Typography>
                    <Typography variant="body2" color="text.secondary">
                      Fuente: {reg.crSource}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      Creado: {formatDate(reg.crCreatedAt)}
                    </Typography>
                  </Box>
                  <Box>{statusChip(reg.crStatus)}</Box>
                  <Button size="small" variant="outlined" onClick={() => handleOpenDossier(reg, 'review')}>
                    Abrir expediente
                  </Button>
                  <Box sx={{ flexGrow: 1 }} />
                  <Stack direction="row" spacing={1}>
                    {canTransitionToStatus(reg.crStatus, 'paid') && (
                      <Tooltip title="Subir comprobante y marcar pagado">
                        <span>
                          <IconButton
                            size="small"
                            color="success"
                            aria-label={`Subir comprobante y marcar pagado para ${reg.crFullName ?? reg.crEmail ?? 'esta inscripción'}`}
                            disabled={isUpdating}
                            onClick={() => handleOpenDossier(reg, 'markPaid')}
                          >
                            <DoneIcon fontSize="small" />
                          </IconButton>
                        </span>
                      </Tooltip>
                    )}
                    {canTransitionToStatus(reg.crStatus, 'pending_payment') && (
                      <Tooltip title="Marcar pendiente">
                        <span>
                          <IconButton
                            size="small"
                            color="warning"
                            aria-label={`Marcar pendiente para ${reg.crFullName ?? reg.crEmail ?? 'esta inscripción'}`}
                            disabled={isUpdating}
                            onClick={() => handleQuickStatus(reg, 'pending_payment')}
                          >
                            <PendingIcon fontSize="small" />
                          </IconButton>
                        </span>
                      </Tooltip>
                    )}
                    {canTransitionToStatus(reg.crStatus, 'cancelled') && (
                      <Tooltip title="Cancelar">
                        <span>
                          <IconButton
                            size="small"
                            color="error"
                            aria-label={`Cancelar inscripción para ${reg.crFullName ?? reg.crEmail ?? 'esta inscripción'}`}
                            disabled={isUpdating}
                            onClick={() => handleQuickStatus(reg, 'cancelled')}
                          >
                            <CancelIcon fontSize="small" />
                          </IconButton>
                        </span>
                      </Tooltip>
                    )}
                  </Stack>
                </Box>
              );
            })}
          </Stack>
        ) : null}
      </Paper>

      <Dialog
        open={Boolean(selectedDossier)}
        onClose={() => setSelectedDossier(null)}
        fullWidth
        maxWidth="lg"
      >
        <DialogTitle>Expediente de inscripción</DialogTitle>
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
                    {activeRegistration.crPartyId && (
                      <Chip size="small" label={`Party #${activeRegistration.crPartyId}`} variant="outlined" />
                    )}
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    {activeRegistration.crEmail ?? 'Sin correo'} {activeRegistration.crPhoneE164 ? `· ${activeRegistration.crPhoneE164}` : ''}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    Slug: {activeRegistration.crCourseSlug} · Fuente: {activeRegistration.crSource} · Creado: {formatDate(activeRegistration.crCreatedAt)}
                  </Typography>
                  <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                    <Button
                      variant="contained"
                      color="success"
                      onClick={handleMarkPaidFromDossier}
                      disabled={updateStatusMutation.isPending || !canMarkPaid}
                    >
                      Marcar pagado
                    </Button>
                    <Button
                      variant="outlined"
                      onClick={() => setSelectedRegForEmails(activeRegistration)}
                    >
                      Ver correos
                    </Button>
                    {!canMarkPaid && (
                      <Chip size="small" color="warning" label="Falta comprobante para marcar pagado" />
                    )}
                  </Stack>
                </Stack>
              </Paper>

              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={1.5}>
                    <Stack direction="row" alignItems="center" justifyContent="space-between" flexWrap="wrap" useFlexGap>
                      <Typography variant="h6">Notas internas</Typography>
                      <Button
                        variant="contained"
                        size="small"
                        startIcon={<SaveIcon />}
                        onClick={handleSaveNotes}
                        disabled={updateNotesMutation.isPending}
                      >
                        Guardar notas
                      </Button>
                    </Stack>
                    <TextField
                      label="Notas internas"
                      multiline
                      minRows={4}
                      value={notesDraft}
                      onChange={(e) => setNotesDraft(e.target.value)}
                      placeholder="Contexto interno, acuerdos, bloqueos o próximos pasos."
                      fullWidth
                    />
                  </Stack>
                </CardContent>
              </Card>

              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={2}>
                    <Stack direction="row" alignItems="center" justifyContent="space-between" flexWrap="wrap" useFlexGap>
                      <Typography variant="h6">Comprobantes de pago</Typography>
                      <Chip size="small" label={`${receipts.length} guardado${receipts.length === 1 ? '' : 's'}`} />
                    </Stack>

                    <Grid container spacing={2}>
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
                          <TextField
                            label="URL del comprobante"
                            value={receiptForm.fileUrl}
                            onChange={(e) => setReceiptForm((prev) => ({ ...prev, fileUrl: e.target.value }))}
                            placeholder="Pega un enlace existente si el archivo ya está cargado"
                            fullWidth
                          />
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
                          <Stack direction="row" spacing={1}>
                            <Button
                              variant="contained"
                              onClick={handleSubmitReceipt}
                              disabled={createReceiptMutation.isPending || updateReceiptMutation.isPending}
                            >
                              {receiptForm.editingId == null ? 'Guardar comprobante' : 'Actualizar comprobante'}
                            </Button>
                            {receiptForm.editingId != null && (
                              <Button variant="text" onClick={() => setReceiptForm(emptyReceiptForm())}>
                                Cancelar edición
                              </Button>
                            )}
                          </Stack>
                        </Stack>
                      </Grid>
                      <Grid item xs={12} md={6}>
                        <Stack spacing={1.5}>
                          {receipts.length === 0 && (
                            <Alert severity="info">
                              Aún no hay comprobantes. Sin al menos uno, la inscripción no puede pasar a pagada.
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
                                  <Stack direction="row" spacing={0.5}>
                                    <IconButton
                                      size="small"
                                      aria-label={`Editar ${receipt.crrFileName ?? `comprobante ${receipt.crrId}`}`}
                                      onClick={() => handleEditReceipt(receipt)}
                                    >
                                      <EditIcon fontSize="small" />
                                    </IconButton>
                                    <IconButton
                                      size="small"
                                      color="error"
                                      aria-label={`Eliminar ${receipt.crrFileName ?? `comprobante ${receipt.crrId}`}`}
                                      onClick={() => handleDeleteReceipt(receipt)}
                                    >
                                      <DeleteOutlineIcon fontSize="small" />
                                    </IconButton>
                                  </Stack>
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
                      <Typography variant="h6">Historial de seguimiento</Typography>
                      <Chip size="small" label={`${followUps.length} entrad${followUps.length === 1 ? 'a' : 'as'}`} />
                    </Stack>

                    <Grid container spacing={2}>
                      <Grid item xs={12} md={6}>
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
                            label="Nota de seguimiento"
                            value={followUpForm.notes}
                            onChange={(e) => setFollowUpForm((prev) => ({ ...prev, notes: e.target.value }))}
                            multiline
                            minRows={3}
                            placeholder="Qué pasó, qué se acordó y cuál es el siguiente paso."
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
                          <TextField
                            label="URL del adjunto"
                            value={followUpForm.attachmentUrl}
                            onChange={(e) => setFollowUpForm((prev) => ({ ...prev, attachmentUrl: e.target.value }))}
                            fullWidth
                          />
                          <Stack direction="row" spacing={1}>
                            <Button
                              variant="contained"
                              onClick={handleSubmitFollowUp}
                              disabled={createFollowUpMutation.isPending || updateFollowUpMutation.isPending}
                            >
                              {followUpForm.editingId == null ? 'Guardar seguimiento' : 'Actualizar seguimiento'}
                            </Button>
                            {followUpForm.editingId != null && (
                              <Button variant="text" onClick={() => setFollowUpForm(emptyFollowUpForm())}>
                                Cancelar edición
                              </Button>
                            )}
                          </Stack>
                        </Stack>
                      </Grid>
                      <Grid item xs={12} md={6}>
                        <Stack spacing={1.5}>
                          {followUps.length === 0 && (
                            <Alert severity="info">
                              Aún no hay seguimiento manual. Los cambios de estado y los comprobantes nuevos también quedarán registrados aquí.
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
                                  <Stack direction="row" spacing={0.5}>
                                    <IconButton
                                      size="small"
                                      aria-label={`Editar seguimiento ${entry.crfSubject ?? `${eventTypeLabel(entry.crfEntryType)} del ${formatDate(entry.crfCreatedAt)}`}`}
                                      onClick={() => handleEditFollowUp(entry)}
                                    >
                                      <EditIcon fontSize="small" />
                                    </IconButton>
                                    <IconButton
                                      size="small"
                                      color="error"
                                      aria-label={`Eliminar seguimiento ${entry.crfSubject ?? `${eventTypeLabel(entry.crfEntryType)} del ${formatDate(entry.crfCreatedAt)}`}`}
                                      onClick={() => handleDeleteFollowUp(entry)}
                                    >
                                      <DeleteOutlineIcon fontSize="small" />
                                    </IconButton>
                                  </Stack>
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
          <Button
            onClick={() => {
              if (selectedDossier) {
                void qc.invalidateQueries({
                  queryKey: ['admin', 'course-registration-dossier', selectedDossier.reg.crCourseSlug, selectedDossier.reg.crId],
                });
              }
            }}
            disabled={dossierQuery.isFetching}
          >
            Actualizar
          </Button>
          <Button onClick={() => setSelectedDossier(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={Boolean(selectedRegForEmails)}
        onClose={() => setSelectedRegForEmails(null)}
        fullWidth
        maxWidth="md"
      >
        <DialogTitle>Historial de correos</DialogTitle>
        <DialogContent dividers>
          {selectedRegForEmails && (
            <Stack spacing={1.5}>
              <Typography variant="subtitle2">
                {selectedRegForEmails.crFullName ?? 'Sin nombre'} · {selectedRegForEmails.crEmail ?? 'Sin correo'}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                Historial persistente por inscripción.
              </Typography>

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
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => void emailEventsQuery.refetch()} disabled={emailEventsQuery.isFetching}>
            Actualizar
          </Button>
          <Button onClick={() => setSelectedRegForEmails(null)}>Cerrar</Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
