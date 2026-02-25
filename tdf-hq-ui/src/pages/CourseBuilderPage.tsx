import { useEffect, useLayoutEffect, useMemo, useState, useCallback, useRef } from 'react';
import { DateTime } from 'luxon';
import { useMutation } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
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

const COURSE_DRAFT_STORAGE_KEY = 'tdf-course-builder-draft';

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
  { label: 'Sábado 1 · Introducción', date: '2026-02-28' },
  { label: 'Sábado 2 · Grabación', date: '2026-03-07' },
  { label: 'Sábado 3 · Mezcla', date: '2026-03-14' },
  { label: 'Sábado 4 · Masterización', date: '2026-03-21' },
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
  const [subtitle, setSubtitle] = useState('Presencial · 4 sábados · 16 horas');
  const [format, setFormat] = useState('Presencial');
  const [duration, setDuration] = useState('4 sábados · 16 horas');
  const [price, setPrice] = useState('150');
  const [currency, setCurrency] = useState('USD');
  const [capacity, setCapacity] = useState('16');
  const [sessionStartHour, setSessionStartHour] = useState('15');
  const [sessionDurationHours, setSessionDurationHours] = useState('4');
  const [locationLabel, setLocationLabel] = useState('TDF Records – Quito');
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
  const draftLoadedRef = useRef(false);
  const autoLoadedRef = useRef(false);
  const hasLocalEditsRef = useRef(false);
  const markDirty = useCallback(() => {
    hasLocalEditsRef.current = true;
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

  const buildPayload = useCallback((): CourseUpsert => {
    const toOptionalNumber = (value: string) => {
      const trimmed = value.trim();
      if (!trimmed) return null;
      const num = Number(trimmed);
      return Number.isFinite(num) ? num : null;
    };
    const cleanCapacity = Number.isFinite(Number(capacity)) ? Number(capacity) : 0;
    const numericPrice = Number(price);
    const cleanPrice = Number.isFinite(numericPrice) ? Math.max(0, Math.round(numericPrice * 100)) : 0;
    const landing = landingUrl.trim();
    const sessionStart = toOptionalNumber(sessionStartHour);
    const sessionDuration = toOptionalNumber(sessionDurationHours);
    return {
      slug: slug.trim(),
      title: title.trim(),
      subtitle: subtitle.trim() || null,
      format: format.trim() || null,
      duration: duration.trim() || null,
      priceCents: cleanPrice,
      currency: currency.trim() || 'USD',
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
    sessionDurationHours,
    sessionStartHour,
    sessions,
    slug,
    subtitle,
    syllabus,
    title,
    whatsappCtaUrl,
  ]);

  const createMutation = useMutation({
    mutationFn: async () => {
      const payload = buildPayload();
      await Courses.upsert(payload);
    },
  });

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

  const payloadPreview = JSON.stringify(buildPayload(), null, 2);
  const canResetLanding = landingUrlTouched && landingUrl !== landingFor(slug);

  useLayoutEffect(() => {
    if (draftLoadedRef.current) return;
    try {
      const raw = window.localStorage.getItem(COURSE_DRAFT_STORAGE_KEY);
      if (!raw) return;
      const draft = JSON.parse(raw) as Partial<CourseUpsert>;
      if (!draft.title && !draft.slug) return;
      setTitle((prev) => draft.title ?? prev);
      setSubtitle((prev) => draft.subtitle ?? prev);
      setFormat((prev) => draft.format ?? prev);
      setDuration((prev) => draft.duration ?? prev);
      setPrice((prev) =>
        draft.priceCents != null ? String(Math.round(draft.priceCents / 100)) : prev,
      );
      setCurrency((prev) => draft.currency ?? prev);
      setCapacity((prev) => (draft.capacity != null ? String(draft.capacity) : prev));
      setSessionStartHour((prev) => (draft.sessionStartHour != null ? String(draft.sessionStartHour) : prev));
      setSessionDurationHours((prev) =>
        draft.sessionDurationHours != null ? String(draft.sessionDurationHours) : prev,
      );
      setLocationLabel((prev) => draft.locationLabel ?? prev);
      setLocationMapUrl((prev) => draft.locationMapUrl ?? prev);
      setWhatsappCtaUrl((prev) => draft.whatsappCtaUrl ?? prev);
      setLandingUrl((prev) => draft.landingUrl ?? prev);
      setDaws((prev) => (draft.daws?.length ? draft.daws.join('\n') : prev));
      setIncludes((prev) => (draft.includes?.length ? draft.includes.join('\n') : prev));
      setInstructorName((prev) => draft.instructorName ?? prev);
      setInstructorBio((prev) => draft.instructorBio ?? prev);
      setInstructorAvatarUrl((prev) => draft.instructorAvatarUrl ?? prev);
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
    } catch {
      // ignore parse errors
    }
  }, []);

  useEffect(() => {
    const payload = buildPayload();
    try {
      window.localStorage.setItem(COURSE_DRAFT_STORAGE_KEY, JSON.stringify(payload));
    } catch {
      // ignore
    }
  }, [buildPayload]);

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
    setPrice(meta.price ? String(Math.round(meta.price)) : '0');
    setCurrency(meta.currency ?? 'USD');
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
    draftLoadedRef.current = false;
    setHasDraft(false);
    loadCourseMutation.mutate({ slug: target, source: 'manual' });
  };

  const handleClearDraft = () => {
    try {
      window.localStorage.removeItem(COURSE_DRAFT_STORAGE_KEY);
    } catch {
      // ignore
    }
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
        <Stack direction="row" spacing={1}>
          <Chip label={`Slug: ${slug}`} color="primary" />
          <Chip label={`Estado: ${createMutation.isSuccess ? 'Publicado' : 'Borrador'}`} />
        </Stack>
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
      </Stack>
      <Card variant="outlined" id="detalles">
        <CardContent>
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
            <Stack direction="row" spacing={1}>
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
        </CardContent>
      </Card>

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

      {createMutation.isSuccess && (
        <Alert severity="success">
          Curso publicado. Revisa la landing en <strong>/curso/{slug}</strong>.
        </Alert>
      )}
      {createMutation.isError && (
        <Alert severity="error">No pudimos guardar el curso. Revisa los campos obligatorios.</Alert>
      )}

      <Card variant="outlined" id="sesiones">
        <CardContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={6}>
              <TextField
                label="Slug (auto)"
                fullWidth
                value={slug}
                InputProps={{ readOnly: true }}
                helperText="Se genera con el título y la fecha de inicio."
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Landing URL"
                fullWidth
                value={landingUrl}
                onChange={(e) => handleLandingUrlChange(e.target.value)}
                helperText={canResetLanding ? 'Editaste la URL; puedes restablecer la sugerida.' : undefined}
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
                label="Precio"
                fullWidth
                value={price}
                onChange={(e) => {
                  setPrice(e.target.value);
                  markDirty();
                }}
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
                label="DAWs"
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

      <Card variant="outlined" id="temario">
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
                      <Button variant="text" size="small" onClick={() => handleDuplicateSession(idx)}>
                        Duplicar
                      </Button>
                      <Button
                        variant="text"
                        size="small"
                        color="error"
                        onClick={() => handleRemoveSession(idx)}
                        disabled={sessions.length <= 1}
                      >
                        Borrar
                      </Button>
                    </Stack>
                  </Grid>
                </Grid>
              );
            })}
            <Button variant="outlined" onClick={handleAddSession}>
              Añadir sesión
            </Button>
          </Stack>
        </CardContent>
      </Card>

      <Card variant="outlined" id="publicacion">
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

      <Card variant="outlined">
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={800}>Revisar y publicar</Typography>
            <Card variant="outlined" sx={{ backgroundColor: 'rgba(148,163,184,0.06)' }}>
              <CardContent>
                <Typography variant="subtitle2" gutterBottom>CTA preview</Typography>
                <Stack spacing={0.5}>
                  <Typography variant="body2" color="text.secondary">
                    Landing: {landingUrl || '—'}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    WhatsApp CTA: {whatsappCtaUrl || '—'}
                  </Typography>
                </Stack>
              </CardContent>
            </Card>
            <TextField
              label="Payload"
              multiline
              minRows={8}
              value={payloadPreview}
              InputProps={{ readOnly: true, sx: { fontFamily: 'monospace' } }}
            />
            <Button
              variant="contained"
              onClick={() => createMutation.mutate()}
              disabled={createMutation.isPending}
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
