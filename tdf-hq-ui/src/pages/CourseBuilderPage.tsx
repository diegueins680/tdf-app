import { useEffect, useMemo, useState, useCallback, useRef } from 'react';
import { DateTime } from 'luxon';
import { useMutation } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Collapse,
  Grid,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { DatePicker } from '@mui/x-date-pickers/DatePicker';
import { LocalizationProvider } from '@mui/x-date-pickers/LocalizationProvider';
import { AdapterLuxon } from '@mui/x-date-pickers/AdapterLuxon';
import { Courses, type CourseUpsert, type CourseMetadata } from '../api/courses';
import { COURSE_DEFAULTS, COURSE_PATH_BASE } from '../config/appConfig';
import { resolveRuntimeCurrency } from '../utils/formatters';
import { useAutoSave } from '../hooks/useAutoSave';
import { formatPriceCentsInput, parseOptionalPriceCents } from '../utils/priceInput';
import { formatCourseMetadataPriceInput } from '../utils/coursePriceInput';

// Draft-only editor state must never be sent as a course API payload.
type CourseBuilderDraft = Omit<CourseUpsert, 'priceCents'> & {
  priceCents?: number;
  priceInput?: string;
  priceInputError?: string;
};
const COURSE_DRAFT_KEY = 'tdf-course-builder-draft';
const readDraftSnapshot = (): string | null | undefined => {
  try { return window.localStorage.getItem(COURSE_DRAFT_KEY); }
  catch { return undefined; }
};

interface SessionInput { label: string; date: string }
interface SyllabusInput { title: string; topics: string }

const splitLines = (input: string) =>
  input
    .split('\n')
    .map((line) => line.trim())
    .filter(Boolean);

const splitTopics = (input: string) =>
  input
    .split(/;|,/)
    .map((t) => t.trim())
    .filter(Boolean);

const DEFAULT_SESSIONS: SessionInput[] = [
  { label: 'Sábado 1 · Introducción', date: '2026-06-06' },
  { label: 'Sábado 2 · Grabación', date: '2026-06-13' },
  { label: 'Sábado 3 · Mezcla', date: '2026-06-20' },
  { label: 'Sábado 4 · Masterización', date: '2026-06-27' },
];

const DEFAULT_SYLLABUS: SyllabusInput[] = [
  { title: 'Introducción a la producción musical', topics: 'Conceptos básicos; Herramientas esenciales' },
  { title: 'Grabación y captura de audio', topics: 'Técnicas de grabación; Configuración de micrófonos' },
  { title: 'Mezcla y edición', topics: 'Ecualización y compresión; Balance y panoramización' },
  { title: 'Masterización y publicación', topics: 'Mastering; Distribución digital' },
];

const MONTH_SLUGS = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];

const slugifyValue = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/-{2,}/g, '-')
    .replace(/(^-|-$)/g, '');

const stripCoursePrefix = (title: string) => title.replace(/^curso\s+(de\s+)?/i, '').trim();

const findEarliestSessionDate = (sessions: SessionInput[]) => {
  const validDates = sessions
    .map((s) => s.date.trim())
    .filter((d) => /^\d{4}-\d{2}-\d{2}$/.test(d));

  if (!validDates.length) return null;
  return validDates.sort()[0];
};

const parseDateValue = (value: string) => {
  if (!value) return null;
  const parsed = DateTime.fromISO(value);
  return parsed.isValid ? parsed : null;
};

const formatDateValue = (value: DateTime | null) => (value ? value.toISODate() ?? '' : '');

const generateSlug = (title: string, startDate: string | null) => {
  const cleanedTitle = stripCoursePrefix(title) || title;
  const titleSlug = slugifyValue(cleanedTitle) || 'curso';

  if (!startDate) return titleSlug;
  const match = /^(\d{4})-(\d{2})/.exec(startDate);
  if (!match) return titleSlug;

  const [, year, month] = match;
  const monthIdx = Number(month) - 1;
  const monthSlug = MONTH_SLUGS[monthIdx] ?? month;

  return [titleSlug, monthSlug, year].filter(Boolean).join('-');
};

const DEFAULT_TITLE = 'Curso de Producción Musical';
const DEFAULT_SLUG = COURSE_DEFAULTS.slug || generateSlug(DEFAULT_TITLE, DEFAULT_SESSIONS[0]?.date ?? null);

export default function CourseBuilderPage() {
  const [title, setTitle] = useState(DEFAULT_TITLE);
  const [subtitle, setSubtitle] = useState('Presencial · Cuatro sábados · 16 horas en total · Próximo inicio: sábado 6 de junio');
  const [format, setFormat] = useState('Presencial');
  const [duration, setDuration] = useState('Cuatro sábados (16 horas en total)');
  const [price, setPrice] = useState('150');
  const [priceLoadError, setPriceLoadError] = useState<string | null>(null);
  const priceValue = useMemo(() => {
    if (priceLoadError) return { cents: null, error: priceLoadError };
    try {
      const cents = parseOptionalPriceCents(price);
      return { cents, error: cents === null ? 'Ingresa un precio; usa 0 si el curso es gratuito.' : null };
    } catch {
      return { cents: null, error: 'Usa un precio no negativo con hasta dos decimales, dentro del rango admitido.' };
    }
  }, [price, priceLoadError]);
  const [currency, setCurrency] = useState(resolveRuntimeCurrency);
  const [capacity, setCapacity] = useState('16');
  const [sessionStartHour, setSessionStartHour] = useState('15');
  const [sessionDurationHours, setSessionDurationHours] = useState('4');
  const [locationLabel, setLocationLabel] = useState('TDF Records');
  const [locationMapUrl, setLocationMapUrl] = useState(COURSE_DEFAULTS.mapUrl);
  const [whatsappCtaUrl, setWhatsappCtaUrl] = useState(COURSE_DEFAULTS.whatsappUrl);
  const landingFor = useCallback((s: string) => `${COURSE_PATH_BASE}/${s}`, []);
  const [landingUrl, setLandingUrl] = useState(landingFor(DEFAULT_SLUG));
  const [landingUrlTouched, setLandingUrlTouched] = useState(false);
  const [includes, setIncludes] = useState('Acceso a grabaciones\nCertificado de participación\nMentorías\nGrupo de WhatsApp\nAcceso a la plataforma de TDF Records');
  const [daws, setDaws] = useState('Logic\nLuna');
  const [instructorName, setInstructorName] = useState('Esteban Muñoz');
  const [instructorBio, setInstructorBio] = useState('Productor en TDF Records. 10+ años grabando bandas, rap y electrónica.');
  const [instructorAvatarUrl, setInstructorAvatarUrl] = useState(COURSE_DEFAULTS.instructorAvatarUrl);
  const [sessions, setSessions] = useState<SessionInput[]>(DEFAULT_SESSIONS);
  const [syllabus, setSyllabus] = useState<SyllabusInput[]>(DEFAULT_SYLLABUS);
  const [loadSlug, setLoadSlug] = useState(COURSE_DEFAULTS.slug);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [hasDraft, setHasDraft] = useState(false);
  const [draftReady, setDraftReady] = useState(false);
  const [showPayloadPreview, setShowPayloadPreview] = useState(false);
  const [showExistingCourseLoader, setShowExistingCourseLoader] = useState(false);
  const draftLoadedRef = useRef(false);
  const autoLoadedRef = useRef(false);
  const hasLocalEditsRef = useRef(false);
  const editRevision = useRef(0);
  const markDirty = useCallback(() => {
    hasLocalEditsRef.current = true;
    editRevision.current += 1;
  }, []);
  const sections = [
    { key: 'detalles', label: 'Detalles' },
    { key: 'sesiones', label: 'Sesiones' },
    { key: 'temario', label: 'Temario' },
    { key: 'publicacion', label: 'Publicación' },
  ] as const;

  const startDate = useMemo<string | null>(() => findEarliestSessionDate(sessions) ?? null, [sessions]);
  const slug = useMemo(() => generateSlug(title, startDate), [title, startDate]);
  const sessionDateErrors = useMemo(() => {
    const errors: string[] = [];
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    let lastDate: Date | null = null;
    sessions.forEach((s, idx) => {
      const raw = s.date.trim();
      if (!raw) {
        errors[idx] = 'Requerido';
        return;
      }
      const parsed = new Date(`${raw}T00:00:00`);
      if (Number.isNaN(parsed.getTime())) {
        errors[idx] = 'Fecha inválida';
        return;
      }
      if (parsed < today) {
        errors[idx] = 'No puede ser en el pasado';
      } else if (lastDate && parsed < lastDate) {
        errors[idx] = 'Mantén el orden cronológico';
      }
      lastDate = parsed;
    });
    return errors;
  }, [sessions]);
  const syllabusErrors = useMemo(() => {
    const errors: string[] = [];
    syllabus.forEach((s, idx) => {
      if (!s.title.trim() && !s.topics.trim()) {
        errors[idx] = 'Completa título o temas';
      }
    });
    return errors;
  }, [syllabus]);

  useEffect(() => {
    if (!landingUrlTouched) {
      setLandingUrl(landingFor(slug));
    }
  }, [slug, landingUrlTouched, landingFor]);

  const handleLandingUrlChange = (value: string) => {
    setLandingUrl(value);
    setLandingUrlTouched(true);
    markDirty();
  };
  const handleResetLanding = () => {
    setLandingUrl(landingFor(slug));
    setLandingUrlTouched(false);
    markDirty();
  };

  const scrollToSection = (id: string) => {
    const el = document.getElementById(id);
    if (el) {
      el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  };

  const buildDraft = useCallback((): CourseBuilderDraft => {
    const toOptionalNumber = (value: string) => {
      const trimmed = value.trim();
      if (!trimmed) return null;
      const num = Number(trimmed);
      return Number.isFinite(num) ? num : null;
    };
    const cleanCapacity = Number.isFinite(Number(capacity)) ? Number(capacity) : 0;
    const landing = landingUrl.trim();
    const sessionStart = toOptionalNumber(sessionStartHour);
    const sessionDuration = toOptionalNumber(sessionDurationHours);
    return {
      slug: slug.trim(),
      title: title.trim(),
      subtitle: subtitle.trim() || null,
      format: format.trim() || null,
      duration: duration.trim() || null,
      priceCents: priceValue.cents ?? undefined,
      priceInput: price,
      priceInputError: priceLoadError ?? undefined,
      currency: currency.trim() || resolveRuntimeCurrency(),
      capacity: Math.max(0, Math.round(cleanCapacity)),
      sessionStartHour: sessionStart ?? null,
      sessionDurationHours: sessionDuration ?? null,
      locationLabel: locationLabel.trim() || null,
      locationMapUrl: locationMapUrl.trim() || null,
      daws: splitLines(daws),
      includes: splitLines(includes),
      instructorName: instructorName.trim() || null,
      instructorBio: instructorBio.trim() || null,
      instructorAvatarUrl: instructorAvatarUrl.trim() || null,
      sessions: sessions
        .filter((s) => s.label.trim() && s.date.trim())
        .map((s) => ({ label: s.label.trim(), date: s.date.trim() })),
      syllabus: syllabus
        .filter((s) => s.title.trim())
        .map((s) => ({
          title: s.title.trim(),
          topics: splitTopics(s.topics),
        })),
      whatsappCtaUrl: whatsappCtaUrl.trim() || null,
      landingUrl: landing || null,
    };
  }, [
    capacity,
    currency,
    daws,
    duration,
    format,
    includes,
    instructorAvatarUrl,
    instructorBio,
    instructorName,
    landingUrl,
    locationLabel,
    locationMapUrl,
    price,
    priceValue.cents,
    priceLoadError,
    sessionDurationHours,
    sessionStartHour,
    sessions,
    slug,
    subtitle,
    syllabus,
    title,
    whatsappCtaUrl,
  ]);

  const buildPayload = useCallback((): CourseUpsert => {
    if (priceValue.error || priceValue.cents === null) {
      throw new Error(priceValue.error ?? 'Ingresa un precio válido.');
    }
    const draft = buildDraft();
    delete draft.priceInput;
    delete draft.priceInputError;
    return { ...draft, priceCents: priceValue.cents };
  }, [buildDraft, priceValue]);
  const payload = buildDraft();
  const { loadDraft, saveDraft, clearDraft } = useAutoSave('course-builder', payload, {
    enabled: hasLocalEditsRef.current && draftReady,
    debounceMs: 2000,
    storageKey: COURSE_DRAFT_KEY,
  });

  const createMutation = useMutation({
    mutationFn: async () => {
      const p = buildPayload();
      const revision = editRevision.current;
      const draftSnapshot = readDraftSnapshot();
      await Courses.upsert(p);
      return { revision, draftSnapshot };
    },
    onSuccess: ({ revision, draftSnapshot }) => {
      // A delayed save must not discard edits made while it was in flight.
      if (revision === editRevision.current) {
        hasLocalEditsRef.current = false;
        // Another editor can replace this shared browser draft while saving.
        if (draftSnapshot !== undefined && readDraftSnapshot() === draftSnapshot) clearDraft();
        setHasDraft(false);
      }
    },
  });
  const courseSaved = createMutation.isSuccess && createMutation.data?.revision === editRevision.current;

  const handleSessionChange = (idx: number, field: keyof SessionInput, value: string) => {
    markDirty();
    setSessions((prev) => prev.map((s, i) => (i === idx ? { ...s, [field]: value } : s)));
  };

  const handleSyllabusChange = (idx: number, field: keyof SyllabusInput, value: string) => {
    markDirty();
    setSyllabus((prev) => prev.map((s, i) => (i === idx ? { ...s, [field]: value } : s)));
  };

  const handleDuplicateSession = (idx: number) => {
    markDirty();
    setSessions((prev) => {
      const copy = prev[idx];
      if (!copy) return prev;
      return [...prev.slice(0, idx + 1), { ...copy }, ...prev.slice(idx + 1)];
    });
  };

  const handleDuplicateLastSession = () => {
    handleDuplicateSession(sessions.length - 1);
  };

  const handleAddSession = () => {
    markDirty();
    setSessions((prev) => [...prev, { label: '', date: '' }]);
  };

  const handleRemoveSession = (idx: number) => {
    markDirty();
    setSessions((prev) => prev.filter((_, i) => i !== idx));
  };

  const handleAddSyllabus = () => {
    markDirty();
    setSyllabus((prev) => [...prev, { title: '', topics: '' }]);
  };

  const payloadPreview = priceValue.error
    ? 'Corrige el precio para ver el payload del curso.'
    : JSON.stringify(buildPayload(), null, 2);
  const canResetLanding = landingUrlTouched && landingUrl !== landingFor(slug);
  const landingUrlHelperText = canResetLanding
    ? `Editaste la URL. Slug sugerido: ${slug}.`
    : `Slug sugerido: ${slug}.`;
  const existingCourseLoaderOpen = showExistingCourseLoader || Boolean(loadError);

  useEffect(() => {
    if (draftLoadedRef.current) return;
    setDraftReady(true);
    const draft = loadDraft() as Partial<CourseBuilderDraft> | null;
    if (!draft || (!draft.title && !draft.slug)) return;
    setTitle((prev) => draft.title ?? prev);
    setSubtitle((prev) => draft.subtitle === undefined ? prev : draft.subtitle ?? '');
    setFormat((prev) => draft.format === undefined ? prev : draft.format ?? '');
    setDuration((prev) => draft.duration === undefined ? prev : draft.duration ?? '');
    if (typeof draft.priceInput === 'string') {
      setPrice(draft.priceInput);
      setPriceLoadError(typeof draft.priceInputError === 'string' ? draft.priceInputError : null);
    } else {
      try {
        setPrice(formatPriceCentsInput(draft.priceCents));
        setPriceLoadError(null);
      } catch {
        setPrice('');
        setPriceLoadError('El precio del borrador no conserva centavos exactos. Vuelve a ingresarlo.');
      }
    }
    setCurrency((prev) => draft.currency ?? prev);
    setCapacity((prev) => (draft.capacity != null ? String(draft.capacity) : prev));
    setSessionStartHour((prev) => draft.sessionStartHour === undefined ? prev : draft.sessionStartHour === null ? '' : String(draft.sessionStartHour));
    setSessionDurationHours((prev) =>
      draft.sessionDurationHours === undefined ? prev : draft.sessionDurationHours === null ? '' : String(draft.sessionDurationHours),
    );
    setLocationLabel((prev) => draft.locationLabel === undefined ? prev : draft.locationLabel ?? '');
    setLocationMapUrl((prev) => draft.locationMapUrl === undefined ? prev : draft.locationMapUrl ?? '');
    setWhatsappCtaUrl((prev) => draft.whatsappCtaUrl === undefined ? prev : draft.whatsappCtaUrl ?? '');
    setLandingUrl((prev) => draft.landingUrl === undefined ? prev : draft.landingUrl ?? '');
    setDaws((prev) => Array.isArray(draft.daws) ? draft.daws.join('\n') : prev);
    setIncludes((prev) => Array.isArray(draft.includes) ? draft.includes.join('\n') : prev);
    setInstructorName((prev) => draft.instructorName === undefined ? prev : draft.instructorName ?? '');
    setInstructorBio((prev) => draft.instructorBio === undefined ? prev : draft.instructorBio ?? '');
    setInstructorAvatarUrl((prev) => draft.instructorAvatarUrl === undefined ? prev : draft.instructorAvatarUrl ?? '');
    setSessions((prev) =>
      draft.sessions
        ? draft.sessions.map((s) => ({ label: s.label ?? '', date: s.date ?? '' }))
        : prev,
    );
    setSyllabus((prev) =>
      draft.syllabus
        ? draft.syllabus.map((s) => ({ title: s.title ?? '', topics: (s.topics ?? []).join('; ') }))
        : prev,
    );
    draftLoadedRef.current = true;
    hasLocalEditsRef.current = true;
    setHasDraft(true);
  }, [loadDraft]);

  useEffect(() => {
    if (!draftReady || !hasLocalEditsRef.current) return;
    saveDraft(payload);
  }, [draftReady, payload, saveDraft]);

  const applyMetadata = (meta: CourseMetadata) => {
    const sanitizedSessions =
      (meta.sessions ?? []).map((s) => ({
        label: s.label ?? '',
        date: s.date ?? '',
      })) || [];
    const sanitizedSyllabus =
      (meta.syllabus ?? []).map((s) => ({
        title: s.title ?? '',
        topics: (s.topics ?? []).join('; '),
      })) || [];
    const safeTitle = meta.title ?? DEFAULT_TITLE;
    const targetSlug =
      (meta.slug ?? '').trim() ||
      generateSlug(safeTitle, findEarliestSessionDate(sanitizedSessions) ?? null);

    setLoadSlug(targetSlug);
    setTitle(safeTitle);
    setSubtitle(meta.subtitle ?? '');
    setFormat(meta.format ?? '');
    setDuration(meta.duration ?? '');
    try {
      setPrice(formatCourseMetadataPriceInput(meta.price));
      setPriceLoadError(null);
    } catch {
      setPrice('');
      setPriceLoadError('El precio recibido no conserva centavos exactos. Vuelve a ingresarlo.');
    }
    setCurrency(meta.currency ?? resolveRuntimeCurrency());
    setCapacity(meta.capacity ? String(meta.capacity) : '0');
    setSessionStartHour(meta.sessionStartHour != null ? String(meta.sessionStartHour) : '');
    setSessionDurationHours(meta.sessionDurationHours != null ? String(meta.sessionDurationHours) : '');
    setLocationLabel(meta.locationLabel ?? '');
    setLocationMapUrl(meta.locationMapUrl ?? '');
    setWhatsappCtaUrl(meta.whatsappCtaUrl ?? '');
    setLandingUrl(meta.landingUrl ?? landingFor(targetSlug));
    setLandingUrlTouched(false);
    setDaws((meta.daws ?? []).join('\n'));
    setIncludes((meta.includes ?? []).join('\n'));
    setInstructorName(meta.instructorName ?? '');
    setInstructorBio(meta.instructorBio ?? '');
    setInstructorAvatarUrl(meta.instructorAvatarUrl ?? '');
    setSessions(sanitizedSessions.length ? sanitizedSessions : DEFAULT_SESSIONS);
    setSyllabus(sanitizedSyllabus.length ? sanitizedSyllabus : DEFAULT_SYLLABUS);
  };

  const loadCourseMutation = useMutation({
    mutationFn: async ({ slug: targetSlug }: { slug: string; source: 'auto' | 'manual' }) => {
      const target = targetSlug.trim();
      if (!target) throw new Error('Ingresa un slug.');
      return Courses.getMetadata(target);
    },
    onSuccess: (meta, variables) => {
      if (variables.source === 'auto' && hasLocalEditsRef.current) return;
      applyMetadata(meta);
      setLoadError(null);
      hasLocalEditsRef.current = false;
      if (variables.source === 'manual') {
        draftLoadedRef.current = false;
        setHasDraft(false);
      }
    },
    onError: (error) => {
      setLoadError(error instanceof Error ? error.message : 'No pudimos cargar ese curso.');
      setShowExistingCourseLoader(true);
    },
  });

  useEffect(() => {
    if (autoLoadedRef.current || draftLoadedRef.current || !COURSE_DEFAULTS.slug) return;
    autoLoadedRef.current = true;
    loadCourseMutation.mutate({ slug: COURSE_DEFAULTS.slug, source: 'auto' });
  }, [loadCourseMutation]);

  const handleReloadFromServer = () => {
    const target = (loadSlug.trim() || slug.trim() || COURSE_DEFAULTS.slug || '').trim();
    if (!target) {
      setLoadError('Ingresa un slug.');
      return;
    }
    hasLocalEditsRef.current = false;
    editRevision.current += 1;
    draftLoadedRef.current = false;
    setHasDraft(false);
    loadCourseMutation.mutate({ slug: target, source: 'manual' });
  };

  const handleClearDraft = () => {
    clearDraft();
    hasLocalEditsRef.current = false;
    draftLoadedRef.current = false;
    setHasDraft(false);
  };

  return (
    <LocalizationProvider dateAdapter={AdapterLuxon}>
      <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems="flex-start" spacing={2}>
        <Box>
          <Typography variant="overline" color="text.secondary">Cursos</Typography>
          <Typography variant="h4" fontWeight={800}>Crear curso</Typography>
          <Typography color="text.secondary">
            Publica un nuevo curso. Requiere permisos de administrador/webmaster (guarda en tablas de Escuela).
          </Typography>
        </Box>
        {courseSaved && (
          <Stack direction="row" spacing={1}>
            <Chip label="Estado: Publicado" color="success" />
          </Stack>
        )}
      </Stack>
      <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
        {sections.map((section) => (
          <Chip
            key={section.key}
            label={section.label}
            clickable
            variant="outlined"
            onClick={() => scrollToSection(section.key)}
          />
        ))}
        <Button
          size="small"
          variant="contained"
          href={landingUrl || `${COURSE_PATH_BASE}/${slug}`}
          target="_blank"
          rel="noreferrer"
        >
          Vista previa landing
        </Button>
        <Button
          size="small"
          variant={existingCourseLoaderOpen ? 'outlined' : 'text'}
          aria-expanded={existingCourseLoaderOpen}
          aria-controls="course-builder-existing-loader"
          onClick={() => {
            if (existingCourseLoaderOpen) {
              setLoadError(null);
              setShowExistingCourseLoader(false);
              return;
            }
            setShowExistingCourseLoader(true);
          }}
        >
          {existingCourseLoaderOpen ? 'Ocultar carga' : 'Cargar existente'}
        </Button>
      </Stack>
      <Collapse in={existingCourseLoaderOpen} unmountOnExit>
        <Card variant="outlined" id="course-builder-existing-loader">
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="body2" color="text.secondary">
                Usa esto solo para editar un curso ya publicado. Para crear uno nuevo, sigue con Detalles.
              </Typography>
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems={{ xs: 'stretch', md: 'center' }}>
                <TextField
                  label="Cargar curso existente (slug)"
                  value={loadSlug}
                  onChange={(e) => {
                    setLoadSlug(e.target.value);
                    markDirty();
                  }}
                  size="small"
                  sx={{ minWidth: { md: 260 } }}
                />
                <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                  <Button
                    variant="outlined"
                    size="small"
                    onClick={handleReloadFromServer}
                    disabled={loadCourseMutation.isPending}
                  >
                    {loadCourseMutation.isPending ? 'Cargando…' : 'Cargar curso'}
                  </Button>
                  {loadError && (
                    <Typography variant="caption" color="error">
                      {loadError}
                    </Typography>
                  )}
                </Stack>
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      </Collapse>

      {hasDraft && (
        <Alert
          severity="info"
          action={
            <Stack direction="row" spacing={1}>
              <Button size="small" onClick={handleReloadFromServer} disabled={loadCourseMutation.isPending}>
                Recargar servidor
              </Button>
              <Button size="small" onClick={handleClearDraft}>
                Descartar borrador
              </Button>
            </Stack>
          }
        >
          Hay un borrador local guardado. Puedes recargar los datos del servidor o descartarlo.
        </Alert>
      )}

      {courseSaved && (
        <Alert severity="success">
          Curso publicado. Revisa la landing en <strong>/curso/{slug}</strong>.
        </Alert>
      )}
      {createMutation.isError && (
        <Alert severity="error">No pudimos guardar el curso. Revisa los campos obligatorios.</Alert>
      )}

      <Card variant="outlined" id="detalles">
        <CardContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={6}>
              <TextField
                label="Landing URL"
                fullWidth
                value={landingUrl}
                onChange={(e) => handleLandingUrlChange(e.target.value)}
                helperText={landingUrlHelperText}
              />
              {canResetLanding && (
                <Box sx={{ mt: 0.5 }}>
                  <Button size="small" onClick={handleResetLanding}>
                    Restablecer URL sugerida
                  </Button>
                </Box>
              )}
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Título"
                fullWidth
                value={title}
                onChange={(e) => {
                  setTitle(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Subtítulo"
                fullWidth
                value={subtitle}
                onChange={(e) => {
                  setSubtitle(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Formato"
                fullWidth
                value={format}
                onChange={(e) => {
                  setFormat(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Duración"
                fullWidth
                value={duration}
                onChange={(e) => {
                  setDuration(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label={`Precio (${currency.trim() || resolveRuntimeCurrency()})`}
                fullWidth
                value={price}
                onChange={(e) => {
                  setPrice(e.target.value);
                  setPriceLoadError(null);
                  markDirty();
                }}
                inputProps={{ inputMode: 'decimal', maxLength: 32 }}
                error={Boolean(priceValue.error)}
                helperText={priceValue.error ?? 'Hasta dos decimales, con punto o coma. Usa 0 para un curso gratuito.'}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Moneda"
                fullWidth
                value={currency}
                onChange={(e) => {
                  setCurrency(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Cupos"
                fullWidth
                value={capacity}
                onChange={(e) => {
                  setCapacity(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Hora de inicio (24h)"
                fullWidth
                value={sessionStartHour}
                onChange={(e) => {
                  setSessionStartHour(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Duración por sesión (h)"
                fullWidth
                value={sessionDurationHours}
                onChange={(e) => {
                  setSessionDurationHours(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Herramientas / enfoque"
                fullWidth
                multiline
                minRows={3}
                value={daws}
                onChange={(e) => {
                  setDaws(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Incluye"
                fullWidth
                multiline
                minRows={3}
                value={includes}
                onChange={(e) => {
                  setIncludes(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Instructor principal"
                fullWidth
                value={instructorName}
                onChange={(e) => {
                  setInstructorName(e.target.value);
                  markDirty();
                }}
                helperText="Nombre visible en la landing."
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Foto/Avatar del instructor (URL)"
                fullWidth
                value={instructorAvatarUrl}
                onChange={(e) => {
                  setInstructorAvatarUrl(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                label="Bio del instructor"
                fullWidth
                multiline
                minRows={3}
                value={instructorBio}
                onChange={(e) => {
                  setInstructorBio(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="WhatsApp CTA"
                fullWidth
                value={whatsappCtaUrl}
                onChange={(e) => {
                  setWhatsappCtaUrl(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Lugar"
                fullWidth
                value={locationLabel}
                onChange={(e) => {
                  setLocationLabel(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Mapa"
                fullWidth
                value={locationMapUrl}
                onChange={(e) => {
                  setLocationMapUrl(e.target.value);
                  markDirty();
                }}
              />
            </Grid>
          </Grid>
        </CardContent>
      </Card>

      <Card variant="outlined" id="sesiones">
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={800}>Sesiones</Typography>
            {sessions.map((s, idx) => {
              const minDate = idx > 0 ? parseDateValue(sessions[idx - 1]?.date ?? '') : null;
              return (
                <Grid container spacing={2} key={idx} alignItems="center">
                  <Grid item xs={12} md={6}>
                    <TextField
                      label="Título"
                      fullWidth
                      value={s.label}
                      onChange={(e) => handleSessionChange(idx, 'label', e.target.value)}
                    />
                  </Grid>
                  <Grid item xs={12} md={4}>
                    <DatePicker
                      label="Fecha"
                      format="yyyy-LL-dd"
                      value={parseDateValue(s.date)}
                      onChange={(value: DateTime | null) =>
                        handleSessionChange(idx, 'date', formatDateValue(value))
                      }
                      disablePast
                      minDate={minDate ?? undefined}
                      slotProps={{
                        textField: {
                          fullWidth: true,
                          error: Boolean(sessionDateErrors[idx]),
                          helperText: sessionDateErrors[idx] ?? undefined,
                        },
                      }}
                    />
                  </Grid>
                  <Grid item xs={12} md={2}>
                    <Stack direction="row" spacing={1}>
                      {sessions.length > 1 && (
                        <Button
                          variant="text"
                          size="small"
                          color="error"
                          onClick={() => handleRemoveSession(idx)}
                        >
                          Borrar
                        </Button>
                      )}
                    </Stack>
                  </Grid>
                </Grid>
              );
            })}
            <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
              <Button variant="outlined" onClick={handleAddSession}>
                Añadir sesión
              </Button>
              {sessions.length > 1 && (
                <Button variant="text" onClick={handleDuplicateLastSession}>
                  Duplicar última sesión
                </Button>
              )}
            </Stack>
          </Stack>
        </CardContent>
      </Card>

      <Card variant="outlined" id="temario">
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={800}>Temario</Typography>
            {syllabus.map((s, idx) => (
              <Grid container spacing={2} key={idx}>
                <Grid item xs={12} md={6}>
                  <TextField
                    label="Título de la sección"
                    fullWidth
                    value={s.title}
                    onChange={(e) => handleSyllabusChange(idx, 'title', e.target.value)}
                    error={Boolean(syllabusErrors[idx])}
                    helperText={syllabusErrors[idx] ?? undefined}
                  />
                </Grid>
                <Grid item xs={12} md={6}>
                  <TextField
                    label="Temas (separa con ; o ,)"
                    fullWidth
                    value={s.topics}
                    onChange={(e) => handleSyllabusChange(idx, 'topics', e.target.value)}
                    error={Boolean(syllabusErrors[idx])}
                  />
                </Grid>
              </Grid>
            ))}
            <Button variant="outlined" onClick={handleAddSyllabus}>
              Añadir sección
            </Button>
          </Stack>
        </CardContent>
      </Card>

      <Card variant="outlined" id="publicacion">
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={800}>Revisar y publicar</Typography>
            <Box
              sx={{
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 1,
                p: 1.5,
                backgroundColor: 'rgba(148,163,184,0.06)',
              }}
            >
              <Typography variant="subtitle2" gutterBottom>Resumen de publicación</Typography>
              <Stack spacing={0.5}>
                <Typography variant="body2" color="text.secondary">
                  Landing: {landingUrl || '—'}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  WhatsApp: {whatsappCtaUrl || '—'}
                </Typography>
              </Stack>
            </Box>
            <Stack spacing={1} alignItems="flex-start">
              <Typography variant="body2" color="text.secondary">
                Publica desde este resumen. Abre el payload técnico solo si necesitas revisar el JSON que se enviará al servidor.
              </Typography>
              <Button
                size="small"
                variant="text"
                aria-expanded={showPayloadPreview}
                aria-controls="course-builder-payload-preview"
                onClick={() => setShowPayloadPreview((value) => !value)}
              >
                {showPayloadPreview ? 'Ocultar payload técnico' : 'Ver payload técnico'}
              </Button>
            </Stack>
            {showPayloadPreview && (
              <TextField
                id="course-builder-payload-preview"
                label="Payload técnico"
                multiline
                minRows={8}
                value={payloadPreview}
                InputProps={{ readOnly: true, sx: { fontFamily: 'monospace' } }}
              />
            )}
            <Button
              variant="contained"
              onClick={() => createMutation.mutate()}
              disabled={createMutation.isPending || Boolean(priceValue.error)}
            >
              {createMutation.isPending ? 'Guardando...' : 'Publicar curso'}
            </Button>
          </Stack>
        </CardContent>
      </Card>
      </Stack>
    </LocalizationProvider>
  );
}
