import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  AlertTitle,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Divider,
  Grid,
  LinearProgress,
  MenuItem,
  Paper,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useSearchParams } from 'react-router-dom';
import { Cms, type CmsContentDTO, type CmsContentIn } from '../api/cms';
import ApiErrorNotice from '../components/ApiErrorNotice';
import { SessionGate } from '../components/SessionGate';
import { COURSE_DEFAULTS, PUBLIC_BASE } from '../config/appConfig';
import { CUSTOM_CMS_SLUG_OPTION, DEFAULT_CMS_SLUGS, getCmsSlugFieldState } from './cmsAdminSlugSelection';
import { getCmsVersionListUiState } from './cmsAdminVersionListState';
import { getCmsVersionRowActions } from './cmsAdminVersionActions';
import { getCmsLiveEditorActionState } from './cmsAdminLiveEditorActions';

const localeOptions = [
  { value: 'es', label: 'Español (es)' },
  { value: 'en', label: 'Inglés (en)' },
] as const;
const STORAGE_KEY = 'tdf-cms-admin:last-selection';
const DRAFT_PREFIX = 'tdf-cms-admin:draft';
const CONTENT_STATUS_OPTIONS = ['draft', 'published'] as const;
type ContentStatus = (typeof CONTENT_STATUS_OPTIONS)[number];
const STATUS_FILTER_OPTIONS = ['all', 'published', 'draft', 'archived'] as const;
type StatusFilter = (typeof STATUS_FILTER_OPTIONS)[number];
type SamplePayload = {
  heroTitle?: string;
  heroSubtitle?: string;
  locale?: string;
} & Record<string, unknown>;
const schemaHints: Record<string, string[]> = {
  'records-public': ['heroTitle', 'heroSubtitle', 'ctaText', 'ctaUrl', 'cards[]'],
  'records-recordings': ['channelUrl', 'videos[]', 'videos[].title', 'videos[].url/youtubeId', 'videos[].duration', 'videos[].sortOrder'],
  'records-releases': ['playlistUrl', 'tracks[]', 'tracks[].title', 'tracks[].artist', 'tracks[].url/spotifyUrl', 'tracks[].links[]', 'tracks[].sortOrder'],
  'records-sessions': ['playlistUrl', 'videos[]', 'videos[].title', 'videos[].url/youtubeId', 'videos[].sortOrder'],
  'fan-hub': ['heroTitle', 'heroSubtitle', 'ctaWhatsapp', 'sections[]'],
  'course-production': ['heroTitle', 'heroSubtitle', 'bullets[]', 'ctaPrimary', 'sessions[]'],
};
const draftAutosaveHelperText =
  'El borrador se guarda automáticamente en este navegador por slug e idioma mientras editas.';
const samplePayloads: Record<string, SamplePayload> = {
  'records-public': {
    heroTitle: 'Lanzamientos destacados',
    heroSubtitle: 'Explora los releases recientes del sello.',
    locale: 'es',
  },
  'records-releases': {
    playlistName: 'RELEASES by TDF',
    playlistUrl: 'https://open.spotify.com/playlist/4FSMAk7z9GFk4pUH9Uffbt',
    tracks: [
      {
        title: 'Canción',
        artist: 'Artista',
        spotifyUrl: 'https://open.spotify.com/track/TRACK_ID',
        duration: '03:30',
        links: [
          { platform: 'Spotify', url: 'https://open.spotify.com/track/TRACK_ID', accent: '#1db954' },
          { platform: 'YouTube', url: 'https://www.youtube.com/watch?v=VIDEO_ID', accent: '#ff0033' },
        ],
        sortOrder: 1,
      },
    ],
    locale: 'es',
  },
  'records-recordings': {
    channelUrl: 'https://www.youtube.com/@tdf.records',
    videos: [
      {
        title: 'DJ Set @ TDF Sessions',
        artist: 'TDF Records',
        url: 'https://www.youtube.com/watch?v=VIDEO_ID',
        duration: '44:14',
        vibe: 'TDF Sessions',
        description: 'Video reciente del canal TDF Records.',
        sortOrder: 1,
      },
    ],
    locale: 'es',
  },
  'records-sessions': {
    playlistUrl: 'https://www.youtube.com/playlist?list=...',
    videos: [
      {
        title: 'Artista - TDF Live Sessions E01',
        guests: 'Artista',
        url: 'https://www.youtube.com/watch?v=VIDEO_ID&list=PLAYLIST_ID',
        duration: '12:34',
        description: 'Sesión en vivo para TDF Live Sessions.',
        sortOrder: 1,
      },
    ],
    locale: 'es',
  },
  'fan-hub': {
    heroTitle: 'Descubre artistas emergentes',
    heroSubtitle: 'Sigue y guarda lanzamientos para escuchar luego.',
    locale: 'es',
  },
  'course-production': {
    heroTitle: 'Producción musical en vivo',
    heroSubtitle: 'Reserva tu cupo con clases hands-on.',
    locale: 'es',
  },
};

const getSchemaHints = (slug: string): string[] | undefined => {
  if (slug.startsWith('records-session-')) {
    return ['title', 'url/youtubeId', 'guests', 'duration', 'description', 'sortOrder'];
  }
  if (slug.startsWith('records-release-')) {
    return ['title', 'artist', 'releasedOn', 'description/blurb', 'cover', 'links[]'];
  }
  if (slug.startsWith('records-recording-')) {
    return ['title', 'image', 'description', 'artist', 'recordedAt', 'vibe'];
  }
  return schemaHints[slug];
};

const getSamplePayload = (slug: string): SamplePayload | undefined => {
  if (samplePayloads[slug]) return samplePayloads[slug];
  if (slug.startsWith('records-session-')) {
    return {
      title: 'Artista - TDF Live Sessions E01',
      guests: 'Artista',
      url: 'https://www.youtube.com/watch?v=VIDEO_ID&list=PLAYLIST_ID',
      duration: '12:34',
      description: 'Sesión en vivo para TDF Live Sessions.',
      sortOrder: 1,
      locale: 'es',
    };
  }
  return undefined;
};

const livePathForSlug = (slug: string) => {
  switch (slug) {
    case 'records-public':
      return '/records';
    case 'records-recordings':
      return '/records';
    case 'records-releases':
      return '/records';
    case 'records-sessions':
      return '/records';
    case 'fan-hub':
      return '/fans';
    case 'course-production':
      return `/curso/${COURSE_DEFAULTS.slug}`;
    default:
      return `/${slug}`;
  }
};

interface DiffLine {
  type: 'same' | 'added' | 'removed';
  text: string;
}
const buildLineDiff = (left: string, right: string): DiffLine[] => {
  const leftLines = left.split('\n');
  const rightLines = right.split('\n');
  const result: DiffLine[] = [];
  const max = Math.max(leftLines.length, rightLines.length);
  for (let i = 0; i < max; i += 1) {
    const l = leftLines[i];
    const r = rightLines[i];
    if (l === r) {
      if (l !== undefined) result.push({ type: 'same', text: l });
    } else {
      if (l !== undefined) result.push({ type: 'removed', text: l });
      if (r !== undefined) result.push({ type: 'added', text: r });
    }
  }
  return result;
};

const isStatusFilter = (value: string): value is StatusFilter =>
  STATUS_FILTER_OPTIONS.some((status) => status === value);

const isContentStatus = (value: string): value is ContentStatus =>
  CONTENT_STATUS_OPTIONS.some((status) => status === value);

const parseMinVersionFilter = (raw: string): number | null => {
  const trimmed = raw.trim();
  if (!trimmed) return null;
  if (!/^\d+$/.test(trimmed)) return null;
  const parsed = Number(trimmed);
  return Number.isSafeInteger(parsed) ? parsed : null;
};

const formatCmsAdminTimestamp = (value: string) => {
  try {
    return new Intl.DateTimeFormat(undefined, {
      dateStyle: 'medium',
      timeStyle: 'short',
    }).format(new Date(value));
  } catch {
    return value;
  }
};

const CMS_STATUS_LABELS: Record<string, string> = {
  archived: 'Archivado',
  draft: 'Borrador',
  published: 'Publicado',
};

const normalizeCmsStatus = (value: string) => value.trim().toLowerCase();

const formatCmsStatusLabel = (value: string) => {
  const normalized = normalizeCmsStatus(value);
  return CMS_STATUS_LABELS[normalized] ?? (value.trim() || 'Sin estado');
};

const formatLocaleLabel = (value: string) =>
  localeOptions.find((option) => option.value === value)?.label ?? value;

export default function CmsAdminPage() {
  const qc = useQueryClient();
  const [searchParams] = useSearchParams();
  const querySlug = searchParams.get('slug')?.trim() ?? '';
  const queryLocale = searchParams.get('locale')?.trim() ?? '';
  const [slugFilter, setSlugFilter] = useState<string>(() => {
    if (querySlug) return querySlug;
    if (typeof window === 'undefined') return 'records-public';
    try {
      const raw = window.localStorage.getItem(STORAGE_KEY);
      const parsed = raw ? (JSON.parse(raw) as { slug?: string }) : null;
      return parsed?.slug ?? 'records-public';
    } catch {
      return 'records-public';
    }
  });
  const [localeFilter, setLocaleFilter] = useState<string>(() => {
    if (queryLocale) return queryLocale;
    if (typeof window === 'undefined') return 'es';
    try {
      const raw = window.localStorage.getItem(STORAGE_KEY);
      const parsed = raw ? (JSON.parse(raw) as { locale?: string }) : null;
      return parsed?.locale ?? 'es';
    } catch {
      return 'es';
    }
  });
  const [title, setTitle] = useState('');
  const [payload, setPayload] = useState('{}');
  const [status, setStatus] = useState<ContentStatus>('draft');
  const [editingFromId, setEditingFromId] = useState<number | null>(null);
  const [payloadError, setPayloadError] = useState<string | null>(null);
  const [formattedPayload, setFormattedPayload] = useState<string>('{}');
  const [statusFilter, setStatusFilter] = useState<StatusFilter>('all');
  const [minVersionFilter, setMinVersionFilter] = useState<number | null>(null);
  const [loadingLiveOnDemand, setLoadingLiveOnDemand] = useState(false);
  const [liveFetchError, setLiveFetchError] = useState<string | null>(null);
  const [pendingVersion, setPendingVersion] = useState<CmsContentDTO | null>(null);
  const [showDraftDiff, setShowDraftDiff] = useState(false);
  const draftKey = useMemo(() => `${DRAFT_PREFIX}:${slugFilter}:${localeFilter}`, [slugFilter, localeFilter]);
  const normalizedSlugFilter = slugFilter.trim();
  const hasSlugSelection = normalizedSlugFilter.length > 0;
  const slugFieldState = useMemo(() => getCmsSlugFieldState(slugFilter), [slugFilter]);
  const customSlugHelperText = hasSlugSelection
    ? 'Usa el mismo slug que consume la ruta pública.'
    : 'Completa este slug para habilitar el guardado y Abrir página en vivo.';
  const saveActionLabel = status === 'published' ? 'Guardar y publicar' : 'Guardar borrador';
  const baseStatusHelperText =
    status === 'published'
      ? 'Publicará esta versión al guardar y actualizará la página en vivo.'
      : 'Guardará esta versión como borrador sin cambiar la página en vivo.';

  useEffect(() => {
    if (querySlug && querySlug !== slugFilter) {
      setSlugFilter(querySlug);
    }
    if (queryLocale && queryLocale !== localeFilter) {
      setLocaleFilter(queryLocale);
    }
  }, [localeFilter, queryLocale, querySlug, slugFilter]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      window.localStorage.setItem(STORAGE_KEY, JSON.stringify({ slug: slugFilter, locale: localeFilter }));
    } catch {
      // ignore storage issues
    }
  }, [slugFilter, localeFilter]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const raw = window.localStorage.getItem(draftKey);
      if (raw) {
        const parsed = JSON.parse(raw) as { title?: string; payload?: string; status?: string };
        setTitle(parsed.title ?? '');
        setPayload(parsed.payload ?? '{}');
        setStatus(parsed.status && isContentStatus(parsed.status) ? parsed.status : 'draft');
        setEditingFromId(null);
        return;
      }
    } catch {
      // ignore broken drafts
    }

    if (hasSlugSelection && slugFieldState.showCustomInput) return;
    setTitle('');
    setPayload('{}');
    setStatus('draft');
    setEditingFromId(null);
  }, [draftKey, hasSlugSelection, slugFieldState.showCustomInput]);

  useEffect(() => {
    try {
      const parsed = JSON.parse(payload);
      setPayloadError(null);
      setFormattedPayload(JSON.stringify(parsed, null, 2));
    } catch (err) {
      const msg = err instanceof Error ? err.message : 'JSON inválido';
      setPayloadError(msg);
    }
  }, [payload]);

  const listQuery = useQuery({
    queryKey: ['cms-content', slugFilter, localeFilter],
    queryFn: () => Cms.list({ slug: slugFilter, locale: localeFilter }),
    enabled: hasSlugSelection,
  });

  const liveQuery = useQuery({
    queryKey: ['cms-public', slugFilter, localeFilter],
    queryFn: () => Cms.getPublic(normalizedSlugFilter, localeFilter),
    retry: 1,
    enabled: Boolean(normalizedSlugFilter && localeFilter),
  });

  const createMutation = useMutation({
    mutationFn: (input: CmsContentIn) => Cms.create(input),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['cms-content'] });
      void qc.invalidateQueries({ queryKey: ['cms-public'] });
    },
  });

  const publishMutation = useMutation({
    mutationFn: (id: number) => Cms.publish(id),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['cms-content'] });
      void qc.invalidateQueries({ queryKey: ['cms-public'] });
    },
  });

  const deleteMutation = useMutation({
    mutationFn: (id: number) => Cms.remove(id),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['cms-content'] });
      void qc.invalidateQueries({ queryKey: ['cms-public'] });
    },
  });

  const versions: CmsContentDTO[] = useMemo(
    () => (Array.isArray(listQuery.data) ? listQuery.data : []),
    [listQuery.data],
  );
  const listDataInvalid = listQuery.data !== undefined && !Array.isArray(listQuery.data);
  const filteredVersions = useMemo(() => {
    return versions.filter((v) => {
      const statusOk = statusFilter === 'all' || normalizeCmsStatus(v.ccdStatus) === statusFilter;
      const versionOk = minVersionFilter == null || v.ccdVersion >= minVersionFilter;
      return statusOk && versionOk;
    });
  }, [minVersionFilter, statusFilter, versions]);
  const editingVersion = useMemo(
    () => versions.find((v) => v.ccdId === editingFromId)?.ccdVersion ?? null,
    [editingFromId, versions],
  );
  const liveContent =
    liveQuery.data && normalizeCmsStatus(liveQuery.data.ccdStatus) !== 'missing'
      ? liveQuery.data
      : null;

  const liveVersion = liveContent?.ccdVersion ?? null;
  const draftBehindLive =
    editingVersion !== null && liveVersion !== null ? editingVersion < liveVersion : false;
  const pendingPayloadPreview = useMemo(
    () => (pendingVersion ? JSON.stringify(pendingVersion.ccdPayload ?? {}, null, 2) : ''),
    [pendingVersion],
  );
  const livePayloadPreview = useMemo(
    () => JSON.stringify(liveContent?.ccdPayload ?? {}, null, 2),
    [liveContent],
  );
  const pendingHasLiveComparison = Boolean(pendingVersion && liveContent);
  const pendingEqualsLive = pendingHasLiveComparison ? pendingPayloadPreview === livePayloadPreview : false;
  const showPendingLivePayloadComparison = pendingHasLiveComparison && !pendingEqualsLive;
  const pendingPayloadLabel = pendingHasLiveComparison ? 'Payload de la versión seleccionada' : 'Payload a cargar';
  const pendingVersionSummary = pendingHasLiveComparison
    ? pendingEqualsLive
      ? 'El payload coincide con la versión en vivo.'
      : 'Revisa los payloads antes de sobrescribir el editor.'
    : 'Todavía no hay una versión publicada. Revisa el payload que vas a cargar antes de sobrescribir el editor.';

  const handleCreate = () => {
    if (!hasSlugSelection) {
      return;
    }
    if (payloadError) return;
    let parsed: unknown = null;
    try {
      parsed = JSON.parse(payload);
    } catch {
      return;
    }
    const normalizedTitle = title.trim();
    createMutation.mutate({
      cciSlug: normalizedSlugFilter,
      cciLocale: localeFilter,
      cciTitle: normalizedTitle.length > 0 ? normalizedTitle : undefined,
      cciStatus: status,
      cciPayload: parsed,
    });
    setEditingFromId(null);
  };

  const loadVersionIntoForm = (v: CmsContentDTO) => {
    setSlugFilter(v.ccdSlug);
    setLocaleFilter(v.ccdLocale);
    setTitle(v.ccdTitle ?? '');
    setStatus((v.ccdStatus as 'draft' | 'published') ?? 'draft');
    setEditingFromId(v.ccdId);
    try {
      setPayload(JSON.stringify(v.ccdPayload ?? {}, null, 2));
    } catch {
      setPayload('{}');
    }
  };

  const handleLoadVersion = (v: CmsContentDTO) => {
    setPendingVersion(v);
  };

  const resetVersionFilters = () => {
    setStatusFilter('all');
    setMinVersionFilter(null);
  };

  const handleConfirmLoadVersion = () => {
    if (!pendingVersion) return;
    loadVersionIntoForm(pendingVersion);
    setPendingVersion(null);
  };

  useEffect(() => {
    if (typeof window === 'undefined') return;
    const timer = window.setTimeout(() => {
      try {
        window.localStorage.setItem(
          draftKey,
          JSON.stringify({ title, payload, status }),
        );
      } catch {
        // ignore quota errors
      }
    }, 400);
    return () => window.clearTimeout(timer);
  }, [draftKey, payload, status, title]);

  const handleFormatPayload = () => {
    if (payloadError) return;
    setPayload(formattedPayload);
  };

  const handleFetchLiveNow = async () => {
    if (!hasSlugSelection) return;
    setLoadingLiveOnDemand(true);
    setLiveFetchError(null);
    try {
      const fresh = await Cms.getPublic(normalizedSlugFilter, localeFilter);
      setTitle(fresh.ccdTitle ?? '');
      setStatus((fresh.ccdStatus as 'draft' | 'published') ?? 'draft');
      setEditingFromId(fresh.ccdId);
      try {
        setPayload(JSON.stringify(fresh.ccdPayload ?? {}, null, 2));
      } catch {
        setPayload('{}');
      }
    } catch (err) {
      const msg = err instanceof Error ? err.message : 'No pudimos traer el contenido en vivo.';
      setLiveFetchError(msg);
    } finally {
      setLoadingLiveOnDemand(false);
    }
  };

  const liveUrl = hasSlugSelection
    ? `${PUBLIC_BASE}${livePathForSlug(normalizedSlugFilter)}${localeFilter ? `?locale=${encodeURIComponent(localeFilter)}` : ''}`
    : '';
  const livePayloadPretty = useMemo(() => {
    if (!liveContent) return '';
    try {
      return JSON.stringify(liveContent.ccdPayload ?? {}, null, 2);
    } catch {
      if (typeof liveContent.ccdPayload === 'string') return liveContent.ccdPayload;
      return JSON.stringify(liveContent.ccdPayload ?? {});
    }
  }, [liveContent]);
  const payloadChanged = useMemo(() => {
    const livePretty = (livePayloadPretty ?? '').trim();
    const draftPretty = (formattedPayload ?? '').trim();
    return livePretty !== '' && draftPretty !== '' && livePretty !== draftPretty;
  }, [formattedPayload, livePayloadPretty]);
  const editorHasMeaningfulPayloadDraft = payload.trim() !== '{}' || editingFromId !== null;
  const titleChangedFromLive = liveContent
    ? title.trim() !== (liveContent.ccdTitle ?? '').trim()
    : false;
  const statusChangedFromLive = liveContent
    ? status !== liveContent.ccdStatus
    : false;
  const canCompareWithLive =
    Boolean(livePayloadPretty) && !payloadError && payloadChanged && editorHasMeaningfulPayloadDraft;
  const liveEditorActionState = useMemo(
    () => getCmsLiveEditorActionState({
      hasSlugSelection,
      hasLiveContent: Boolean(liveContent),
      hasPayloadError: Boolean(payloadError),
      isLoadingLiveOnDemand: loadingLiveOnDemand,
      payloadChangedFromLive: payloadChanged,
      statusChangedFromLive,
      titleChangedFromLive,
    }),
    [
      hasSlugSelection,
      liveContent,
      loadingLiveOnDemand,
      payloadChanged,
      payloadError,
      statusChangedFromLive,
      titleChangedFromLive,
    ],
  );
  const liveContentSummary = canCompareWithLive
    ? 'El comparador revisa el payload en vivo contra este borrador.'
    : liveEditorActionState.showUseLiveAction
      ? 'Carga la versión en vivo para editar la estructura publicada desde el formulario.'
      : 'El editor ya usa el payload en vivo.';
  const draftVsLiveDiff = useMemo(
    () => buildLineDiff(livePayloadPretty || '', formattedPayload || ''),
    [formattedPayload, livePayloadPretty],
  );
  const safeDraftDiff = Array.isArray(draftVsLiveDiff) ? draftVsLiveDiff : [];
  const sharedVersionSlug = useMemo(() => {
    const slugs = Array.from(new Set(
      filteredVersions
        .map((version) => version.ccdSlug.trim())
        .filter((value) => value !== ''),
    ));
    return slugs.length === 1 ? (slugs[0] ?? null) : null;
  }, [filteredVersions]);
  const sharedVersionLocale = useMemo(() => {
    const localesInView = Array.from(new Set(
      filteredVersions
        .map((version) => version.ccdLocale.trim())
        .filter((value) => value !== ''),
    ));
    return localesInView.length === 1 ? (localesInView[0] ?? null) : null;
  }, [filteredVersions]);
  const sharedVersionTitle = useMemo(() => {
    const titlesInView = Array.from(new Set(
      filteredVersions
        .map((version) => (version.ccdTitle ?? '').trim())
        .filter((value) => value !== ''),
    ));
    return titlesInView.length === 1 ? (titlesInView[0] ?? null) : null;
  }, [filteredVersions]);
  const sharedVersionStatus = useMemo(() => {
    const statusesInView = Array.from(new Set(
      filteredVersions
        .map((version) => normalizeCmsStatus(version.ccdStatus))
        .filter((value) => value !== ''),
    ));
    return statusesInView.length === 1 ? (statusesInView[0] ?? null) : null;
  }, [filteredVersions]);
  const sharedVersionContextSummary = useMemo(() => {
    const parts: string[] = [];
    if (sharedVersionTitle) parts.push(`título ${sharedVersionTitle}`);
    if (sharedVersionSlug) parts.push(`slug ${sharedVersionSlug}`);
    if (sharedVersionLocale) parts.push(`idioma ${formatLocaleLabel(sharedVersionLocale)}`);
    if (sharedVersionStatus) parts.push(`estado ${formatCmsStatusLabel(sharedVersionStatus)}`);
    return parts.join(' · ');
  }, [sharedVersionLocale, sharedVersionSlug, sharedVersionStatus, sharedVersionTitle]);
  const versionListUiState = useMemo(
    () => getCmsVersionListUiState({
      filteredCount: filteredVersions.length,
      minVersionFilter,
      statusFilter,
      totalVersions: versions.length,
    }),
    [filteredVersions.length, minVersionFilter, statusFilter, versions.length],
  );
  const historyStatuses = useMemo(
    () =>
      Array.from(
        new Set(
          versions
            .map((version) => normalizeCmsStatus(version.ccdStatus))
            .filter((value) => value !== ''),
        ),
      ),
    [versions],
  );
  const hasActiveVersionFilters = statusFilter.trim().toLowerCase() !== 'all' || minVersionFilter != null;
  const showSingleLiveVersionSummary =
    !hasActiveVersionFilters &&
    versions.length === 1 &&
    filteredVersions.length === 1 &&
    liveContent?.ccdId === versions[0]?.ccdId;
  const visibleVersionRows = showSingleLiveVersionSummary ? [] : filteredVersions;
  const hasStaleVersionPublishCandidate = visibleVersionRows.some((version) => (
    liveVersion !== null
    && version.ccdVersion < liveVersion
    && normalizeCmsStatus(version.ccdStatus) !== 'published'
    && liveContent?.ccdId !== version.ccdId
    && editingFromId !== version.ccdId
  ));
  const versionToolbarHint = showSingleLiveVersionSummary ? null : versionListUiState.toolbarHint;
  const showHistoryStatusFilter = (versions.length > 2 && historyStatuses.length > 1) || statusFilter !== 'all';
  const showHistoryMinVersionFilter = versions.length > 2 || minVersionFilter != null;
  const showVersionHistoryEmptyState =
    Boolean(versionListUiState.emptyMessage) &&
    !listQuery.isLoading &&
    !listQuery.isError &&
    !listDataInvalid;
  const showVersionEmptyResetAction = showVersionHistoryEmptyState && versionListUiState.showEmptyReset;
  const showVersionToolbarReset = versionListUiState.showToolbarReset && !showVersionEmptyResetAction;
  const showVersionToolbarControls =
    versionListUiState.showToolbarFilters
    && (showHistoryStatusFilter || showHistoryMinVersionFilter || showVersionToolbarReset);
  const versionCountLabel = useMemo(() => {
    const totalVersions = versions.length;
    const visibleVersions = filteredVersions.length;

    if (totalVersions === 0) return null;
    if (visibleVersions === totalVersions) {
      return totalVersions > 1 ? `${totalVersions} versiones` : null;
    }

    return `${visibleVersions} de ${totalVersions}`;
  }, [filteredVersions.length, versions.length]);
  const editingSourceChipLabel = editingFromId
    ? editingVersion != null
      ? `Base: v${editingVersion}`
      : `Base: ID ${editingFromId}`
    : null;
  const samplePayload = getSamplePayload(normalizedSlugFilter);
  const samplePayloadPreview = useMemo(
    () => (samplePayload ? JSON.stringify(samplePayload, null, 2) : ''),
    [samplePayload],
  );
  const editorMatchesSamplePayload =
    Boolean(samplePayload)
    && !payloadError
    && payload.trim() === samplePayloadPreview.trim()
    && (!samplePayload?.heroTitle || title.trim() === samplePayload.heroTitle.trim())
    && (!samplePayload?.locale || localeFilter === samplePayload.locale);
  const liveLookupFailed = liveQuery.isError;
  const liveLookupPending = liveQuery.isLoading || liveQuery.isFetching;
  const liveLookupUnresolved = liveLookupPending || liveLookupFailed;
  const schemaHint = getSchemaHints(normalizedSlugFilter);
  const payloadHelperText = payloadError
    ? `Error: ${payloadError}`
    : schemaHint
      ? `Estructura JSON del bloque (usa objetos/arrays). Claves sugeridas: ${schemaHint.join(', ')}`
      : 'Estructura JSON del bloque (usa objetos/arrays). Para slugs nuevos, parte de tu propio JSON o trae la versión en vivo si ya existe.';
  const hasSamplePayload = Boolean(samplePayload);
  const hasStartedNewContentDraft = title.trim().length > 0 || payload.trim() !== '{}';
  const hasCustomNewContentDraft = !liveContent && hasStartedNewContentDraft;
  const showExampleAction =
    hasSamplePayload
    && !editorMatchesSamplePayload
    && !hasCustomNewContentDraft
    && !liveContent
    && !liveLookupUnresolved;
  const showLiveStartEmptyEditorGuard =
    hasSlugSelection
    && Boolean(liveContent)
    && editingFromId === null
    && !payloadError
    && title.trim().length === 0
    && formattedPayload.trim() === '{}';
  const samplePayloadGuidance = liveLookupPending && hasSamplePayload
    ? 'Confirmando si ya existe una versión en vivo antes de mostrar ejemplos genéricos.'
    : liveLookupFailed && hasSamplePayload
    ? 'No pudimos confirmar si ya existe una versión en vivo. Reintenta la carga en vivo antes de partir de un ejemplo.'
    : liveContent
    ? 'Esta página ya tiene contenido publicado. Parte de la versión en vivo para mantener la estructura real antes de escribir JSON nuevo.'
    : samplePayload
      ? editorMatchesSamplePayload
        ? 'El ejemplo sugerido ya está cargado. Ajusta título y payload antes de guardar.'
        : hasCustomNewContentDraft
        ? 'Ya hay contenido en el editor. Usa "Limpiar" si quieres volver a partir de un ejemplo sugerido.'
        : 'Usa el botón "Cargar ejemplo" para ver la estructura sugerida del payload para este slug (no valida contra un esquema aún).'
      : hasSlugSelection
        ? 'Este slug no tiene un ejemplo sugerido todavía. Empieza con tu propio JSON o trae la versión en vivo si ya existe.'
        : 'Elige un slug sugerido o escribe uno para empezar a editar.';
  const showSamplePayloadGuidance =
    !liveLookupFailed && (!liveContent || (liveEditorActionState.showUseLiveAction && showLiveStartEmptyEditorGuard));
  const showLiveStartGuidance =
    showSamplePayloadGuidance && Boolean(liveContent) && liveEditorActionState.showUseLiveAction;
  const editorHasMetadataDraft = titleChangedFromLive || statusChangedFromLive;
  const compareHint = livePayloadPretty
    ? payloadError
      ? 'Corrige el JSON para volver a comparar este borrador con la versión en vivo.'
      : payloadChanged
        ? editorHasMeaningfulPayloadDraft
          ? 'El payload editable está arriba. Usa Comparar cambios para revisar el borrador contra la versión en vivo sin abrir un segundo visor de payload.'
          : showLiveStartGuidance
            ? 'El payload editable está arriba. Escribe tu propio JSON solo si vas a reemplazar la estructura publicada.'
            : 'Empieza con "Usar versión en vivo" para editar la estructura real, o escribe tu propio JSON si vas a reemplazarla.'
        : editorHasMetadataDraft
          ? 'El payload editable ya coincide con la versión en vivo. Vuelve a cargar la versión en vivo para descartar cambios de título o estado.'
          : 'El payload editable ya coincide con la versión en vivo. El comparador aparecerá cuando vuelvas a modificarlo.'
    : 'El payload editable está arriba. Cuando exista una versión en vivo, la verás en la columna izquierda, aparecerá el botón "Usar versión en vivo" y podrás compararla desde aquí.';
  const editorGuidance = `${draftAutosaveHelperText} ${compareHint}`;
  const showFormatPayloadAction = !payloadError && payload !== formattedPayload;
  const showClearPayloadAction =
    !liveContent
    && (payload.trim() !== '{}' || (editingFromId === null && title.trim().length > 0));
  const handleClearEditorDraft = () => {
    setPayload('{}');
    if (!liveContent && editingFromId === null) {
      setTitle('');
      setStatus('draft');
    }
  };
  const editorHasFirstVersionContentDraft =
    title.trim().length > 0 || (!payloadError && formattedPayload.trim() !== '{}');
  const showFirstVersionLiveLookupGuard =
    hasSlugSelection
    && liveLookupPending
    && !listQuery.isError
    && !listDataInvalid
    && versions.length === 0
    && !liveContent
    && editorHasFirstVersionContentDraft;
  const showFirstVersionEmptyDraftGuard =
    hasSlugSelection
    && !payloadError
    && !listQuery.isError
    && !listDataInvalid
    && versions.length === 0
    && !liveContent
    && !editorHasFirstVersionContentDraft;
  const statusHelperText = !hasSlugSelection
    ? 'Completa el slug para habilitar el estado y el guardado.'
    : showFirstVersionLiveLookupGuard
      ? 'Espera a que termine la búsqueda en vivo antes de guardar la primera versión.'
    : showFirstVersionEmptyDraftGuard
      ? 'Agrega un título o payload antes de guardar la primera versión.'
      : showLiveStartEmptyEditorGuard
        ? 'Parte de la versión en vivo o empieza un borrador propio antes de guardar.'
        : baseStatusHelperText;
  const showStatusControl =
    hasSlugSelection
    && !showFirstVersionLiveLookupGuard
    && !showFirstVersionEmptyDraftGuard
    && !showLiveStartEmptyEditorGuard;
  const canSaveVersion =
    hasSlugSelection
    && !payloadError
    && !showFirstVersionLiveLookupGuard
    && !showFirstVersionEmptyDraftGuard
    && !showLiveStartEmptyEditorGuard;
  const showSaveVersionAction =
    hasSlugSelection
    && !liveEditorActionState.showLiveInSyncChip
    && !showFirstVersionLiveLookupGuard
    && !showFirstVersionEmptyDraftGuard
    && !showLiveStartEmptyEditorGuard;
  const showFirstVersionHistoryGuidance =
    hasSlugSelection &&
    editorHasFirstVersionContentDraft &&
    !listQuery.isLoading &&
    !listQuery.isError &&
    !listDataInvalid &&
    versions.length === 0 &&
    !liveLookupUnresolved &&
    !hasActiveVersionFilters;
  const showVersionHistorySection =
    listQuery.isLoading ||
    listQuery.isError ||
    listDataInvalid ||
    (versions.length > 0 && !showSingleLiveVersionSummary) ||
    hasActiveVersionFilters;

  return (
    <SessionGate message="Inicia sesión para administrar contenido público.">
    <Stack spacing={3}>
      <Dialog
        open={Boolean(pendingVersion)}
        onClose={() => setPendingVersion(null)}
        fullWidth
        maxWidth="md"
      >
        <DialogTitle>Cargar versión en el formulario</DialogTitle>
        <DialogContent dividers>
          {pendingVersion && (
            <Stack spacing={1.5}>
              <Typography fontWeight={700}>{pendingVersion.ccdTitle ?? pendingVersion.ccdSlug}</Typography>
              <Typography variant="body2" color="text.secondary">
                v{pendingVersion.ccdVersion} · {formatCmsStatusLabel(pendingVersion.ccdStatus)} · {formatLocaleLabel(pendingVersion.ccdLocale)} ·{' '}
                {pendingVersion.ccdPublishedAt
                  ? `publicado ${formatCmsAdminTimestamp(pendingVersion.ccdPublishedAt)}`
                  : `creado ${formatCmsAdminTimestamp(pendingVersion.ccdCreatedAt)}`}
              </Typography>
              <Typography variant="body2">
                Live actual:{' '}
                {liveContent
                  ? `v${liveContent.ccdVersion} (${formatCmsStatusLabel(liveContent.ccdStatus)} · ${formatLocaleLabel(liveContent.ccdLocale)})`
                  : 'no hay versión publicada'}
              </Typography>
              <Typography
                variant="body2"
                color={pendingHasLiveComparison && !pendingEqualsLive ? 'warning.main' : 'text.secondary'}
              >
                {pendingVersionSummary}
              </Typography>
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                <Box sx={{ flex: 1 }}>
                  <Typography variant="caption" color="text.secondary">
                    {pendingPayloadLabel}
                  </Typography>
                  <Box
                    component="pre"
                    sx={{
                      mt: 0.5,
                      p: 1,
                      border: '1px solid',
                      borderColor: 'divider',
                      borderRadius: 1,
                      bgcolor: 'rgba(148,163,184,0.04)',
                      fontSize: 12,
                      maxHeight: 260,
                      overflow: 'auto',
                    }}
                  >
                    {pendingPayloadPreview || '{}'}
                  </Box>
                </Box>
                {showPendingLivePayloadComparison && (
                  <Box sx={{ flex: 1 }}>
                    <Typography variant="caption" color="text.secondary">
                      Payload en vivo
                    </Typography>
                    <Box
                      component="pre"
                      sx={{
                        mt: 0.5,
                        p: 1,
                        border: '1px solid',
                        borderColor: 'divider',
                        borderRadius: 1,
                        bgcolor: 'rgba(148,163,184,0.04)',
                        fontSize: 12,
                        maxHeight: 260,
                        overflow: 'auto',
                      }}
                    >
                      {livePayloadPreview || '{}'}
                    </Box>
                  </Box>
                )}
              </Stack>
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setPendingVersion(null)}>Cancelar</Button>
          <Button variant="contained" onClick={handleConfirmLoadVersion} disabled={!pendingVersion}>
            Cargar en formulario
          </Button>
        </DialogActions>
      </Dialog>
      <Dialog open={showDraftDiff} onClose={() => setShowDraftDiff(false)} fullWidth maxWidth="md">
        <DialogTitle>Comparar borrador con versión en vivo</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1}>
            <Typography variant="body2" color="text.secondary">
              Se muestra una diff línea por línea entre el payload que editas y el contenido en vivo.
            </Typography>
            <Box
              component="pre"
              sx={{
                fontSize: 12,
                bgcolor: 'rgba(148,163,184,0.04)',
                p: 1,
                borderRadius: 1,
                border: '1px solid',
                borderColor: 'divider',
                maxHeight: 360,
                overflow: 'auto',
              }}
            >
              {safeDraftDiff.map((line, idx) => (
                <Box
                  key={`${line.type}-${idx}`}
                  component="span"
                  sx={{
                    display: 'block',
                    color:
                      line.type === 'added'
                        ? 'success.main'
                        : line.type === 'removed'
                          ? 'error.main'
                          : 'text.primary',
                  }}
                >
                  {line.type === 'added' ? '+ ' : line.type === 'removed' ? '- ' : '  '}
                  {line.text}
                </Box>
              ))}
            </Box>
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setShowDraftDiff(false)}>Cerrar</Button>
        </DialogActions>
      </Dialog>

      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems="flex-start">
        <Box>
          <Typography variant="overline" color="text.secondary">CMS</Typography>
          <Typography variant="h4" fontWeight={800}>Contenido público</Typography>
          <Typography color="text.secondary">
            Crear, publicar y versionar bloques para páginas públicas (records, fan hub, landing cursos).
          </Typography>
        </Box>
      </Stack>

      <Paper variant="outlined" sx={{ p: 2.5 }}>
        <Stack spacing={2.5}>
          <Grid container spacing={2}>
            <Grid item xs={12} md={4}>
              <TextField
                select
                fullWidth
                label="Slug"
                value={slugFieldState.selectValue}
                onChange={(e) => {
                  const nextValue = e.target.value;
                  setSlugFilter(nextValue === CUSTOM_CMS_SLUG_OPTION ? '' : nextValue);
                }}
                helperText={
                  slugFieldState.showCustomInput
                    ? 'Escribe el slug exacto abajo si todavía no aparece en la lista.'
                    : 'Identificador de la página.'
                }
              >
                {DEFAULT_CMS_SLUGS.map((slug) => (
                  <MenuItem key={slug} value={slug}>{slug}</MenuItem>
                ))}
                <MenuItem value={CUSTOM_CMS_SLUG_OPTION}>Otro slug…</MenuItem>
              </TextField>
              {slugFieldState.showCustomInput && (
                <TextField
                  fullWidth
                  label="Slug personalizado"
                  value={slugFilter}
                  onChange={(e) => setSlugFilter(e.target.value)}
                  sx={{ mt: 1 }}
                  helperText={customSlugHelperText}
                />
              )}
            </Grid>
            <Grid item xs={12} md={2}>
              <TextField
                select
                fullWidth
                label="Idioma"
                value={localeFilter}
                onChange={(e) => setLocaleFilter(e.target.value)}
              >
                {localeOptions.map((option) => (
                  <MenuItem key={option.value} value={option.value}>{option.label}</MenuItem>
                ))}
              </TextField>
            </Grid>
          </Grid>

          <Divider flexItem />

          <Grid container spacing={2}>
            <Grid item xs={12} md={5}>
              <Card variant="outlined" data-testid="cms-admin-live-content-card">
                <CardContent>
                  <Stack spacing={1.5}>
                    <Stack direction="row" justifyContent="space-between" alignItems="center">
                      <Typography variant="subtitle1" fontWeight={700}>Contenido en vivo</Typography>
                    </Stack>
                    {liveQuery.isLoading && <LinearProgress />}
                    {liveQuery.isError && (
                      <ApiErrorNotice
                        error={liveQuery.error}
                        title="No pudimos cargar el contenido publicado"
                        onRetry={() => {
                          void liveQuery.refetch();
                        }}
                        showCorsHint
                        helper={
                          <Typography variant="caption">
                            Reintenta esta carga antes de partir de un ejemplo genérico.
                          </Typography>
                        }
                      />
                    )}
                    {!hasSlugSelection && (
                      <Alert severity="info">
                        Elige un slug para consultar la versión publicada de esa página.
                      </Alert>
                    )}
                    {hasSlugSelection && !liveQuery.isError && !liveQuery.isLoading && !liveContent && (
                      <Alert severity="warning">
                        <AlertTitle>Sin contenido publicado</AlertTitle>
                        Publica una versión para activar la vista previa en vivo y el enlace a la página pública.
                      </Alert>
                    )}
                    {liveContent && (
                      <Stack spacing={1}>
                        <Typography fontWeight={700}>{liveContent.ccdTitle ?? liveContent.ccdSlug}</Typography>
                        <Stack direction="row" spacing={1} flexWrap="wrap">
                          <Chip label={`v${liveContent.ccdVersion}`} size="small" />
                          <Chip
                            label={formatCmsStatusLabel(liveContent.ccdStatus)}
                            size="small"
                            color={normalizeCmsStatus(liveContent.ccdStatus) === 'published' ? 'success' : 'default'}
                          />
                          {liveContent.ccdPublishedAt && (
                            <Chip
                              label={`Publicado: ${formatCmsAdminTimestamp(liveContent.ccdPublishedAt)}`}
                              size="small"
                              variant="outlined"
                            />
                          )}
                        </Stack>
                        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
                          <Typography variant="body2" color="text.secondary" sx={{ flexGrow: 1 }}>
                            {liveContentSummary}
                          </Typography>
                          <Button
                            size="small"
                            variant="outlined"
                            href={liveUrl}
                            target="_blank"
                            rel="noreferrer"
                          >
                            Abrir página en vivo
                          </Button>
                        </Stack>
                      </Stack>
                    )}
                  </Stack>
                </CardContent>
              </Card>
            </Grid>

            <Grid item xs={12} md={7}>
              <Stack spacing={1}>
                <Typography variant="subtitle1" fontWeight={700}>Editar / crear versión</Typography>
                {draftBehindLive && (
                  <Alert severity="info">
                    La versión en vivo (v{liveVersion}) es más reciente que la que estás editando
                    {editingVersion ? ` (v${editingVersion})` : ''}. Carga la última publicada para evitar sobrescribir cambios.
                  </Alert>
                )}
                {showSamplePayloadGuidance && (
                  <Alert severity="info" sx={{ mb: 1 }}>
                    {samplePayloadGuidance}
                  </Alert>
                )}
                <TextField
                  label="Título"
                  fullWidth
                  value={title}
                  onChange={(e) => setTitle(e.target.value)}
                />
                <TextField
                  label="Payload JSON"
                  fullWidth
                  multiline
                  minRows={10}
                  value={payload}
                  onChange={(e) => setPayload(e.target.value)}
                  helperText={payloadHelperText}
                  error={Boolean(payloadError)}
                />
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  {showFormatPayloadAction && (
                    <Button variant="outlined" onClick={handleFormatPayload}>
                      Formatear JSON
                    </Button>
                  )}
                  {showClearPayloadAction && (
                    <Button
                      variant="text"
                      onClick={handleClearEditorDraft}
                      disabled={createMutation.isPending}
                    >
                      Limpiar
                    </Button>
                  )}
                  {showExampleAction && (
                    <Button
                      variant="text"
                      onClick={() => {
                        if (!samplePayload) return;
                        if (samplePayload.locale) setLocaleFilter(samplePayload.locale);
                        setPayload(JSON.stringify(samplePayload, null, 2));
                        if (samplePayload.heroTitle) setTitle(samplePayload.heroTitle);
                      }}
                    >
                      Cargar ejemplo
                    </Button>
                  )}
                  {liveEditorActionState.showUseLiveAction && (
                    <Button
                      variant="outlined"
                      onClick={() => {
                        void handleFetchLiveNow();
                      }}
                      disabled={loadingLiveOnDemand || !hasSlugSelection}
                    >
                      {liveEditorActionState.useLiveActionLabel}
                    </Button>
                  )}
                  {liveEditorActionState.showLiveInSyncChip && (
                    <Chip label="Editor coincide con versión en vivo" color="success" variant="outlined" />
                  )}
                  {canCompareWithLive && (
                    <Button variant="text" onClick={() => setShowDraftDiff(true)}>
                      Comparar cambios
                    </Button>
                  )}
                  {liveFetchError && <Chip label={liveFetchError} color="error" variant="outlined" />}
                </Stack>
                <Typography
                  data-testid="cms-admin-editor-guidance"
                  variant="caption"
                  color="text.secondary"
                >
                  {editorGuidance}
                </Typography>
                {showStatusControl ? (
                  <TextField
                    select
                    label="Estado"
                    value={status}
                    onChange={(e) => {
                      const next = e.target.value.trim();
                      setStatus(isContentStatus(next) ? next : 'draft');
                    }}
                    helperText={statusHelperText}
                    sx={{ width: 240 }}
                  >
                    <MenuItem value="draft">Borrador</MenuItem>
                    <MenuItem value="published">Publicado</MenuItem>
                  </TextField>
                ) : (
                  <Typography
                    data-testid="cms-admin-first-version-save-guidance"
                    variant="caption"
                    color="text.secondary"
                  >
                    {statusHelperText}
                  </Typography>
                )}
                <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                  {showSaveVersionAction && (
                    <Button
                      variant="contained"
                      onClick={handleCreate}
                      disabled={createMutation.isPending || !canSaveVersion}
                    >
                      {saveActionLabel}
                    </Button>
                  )}
                  {editingSourceChipLabel && <Chip label={editingSourceChipLabel} size="small" color="info" />}
                  {createMutation.isError && (
                    <Alert severity="error" sx={{ flexGrow: 1 }}>
                      {createMutation.error instanceof Error ? createMutation.error.message : 'Error al crear.'}
                    </Alert>
                  )}
                  {createMutation.isSuccess && <Alert severity="success">Versión creada.</Alert>}
                </Stack>
                {showFirstVersionHistoryGuidance && (
                  <Typography
                    data-testid="cms-admin-first-version-history-guidance"
                    variant="caption"
                    color="text.secondary"
                  >
                    El historial de versiones aparecerá debajo de este editor cuando guardes la primera versión.
                  </Typography>
                )}
              </Stack>
            </Grid>
          </Grid>
        </Stack>
      </Paper>

      {showVersionHistorySection && (
        <Paper variant="outlined" sx={{ p: 2.5 }} data-testid="cms-admin-version-history">
        <Stack spacing={2}>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }} justifyContent="space-between">
            <Stack spacing={0.75}>
              <Stack direction="row" spacing={1} alignItems="center">
                <Typography variant="h6" fontWeight={800}>Versiones</Typography>
                {versionCountLabel && <Chip label={versionCountLabel} size="small" />}
              </Stack>
              {sharedVersionContextSummary && (
                <Typography variant="body2" color="text.secondary">
                  Contexto compartido: {sharedVersionContextSummary}.
                </Typography>
              )}
              {versionToolbarHint && (
                <Typography variant="body2" color="text.secondary">
                  {versionToolbarHint}
                </Typography>
              )}
              {hasStaleVersionPublishCandidate && (
                <Typography
                  data-testid="cms-admin-stale-version-publish-guidance"
                  variant="body2"
                  color="text.secondary"
                >
                  Las versiones anteriores a la versión en vivo se revisan en el formulario antes de publicarlas.
                </Typography>
              )}
            </Stack>
            {showVersionToolbarControls && (
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
                {showHistoryStatusFilter && (
                  <TextField
                    select
                    size="small"
                    label="Estado del historial"
                    value={statusFilter}
                    onChange={(e) => {
                      const next = e.target.value.trim();
                      setStatusFilter(isStatusFilter(next) ? next : 'all');
                    }}
                    sx={{ minWidth: 140 }}
                  >
                    <MenuItem value="all">Todos</MenuItem>
                    <MenuItem value="published">Publicados</MenuItem>
                    <MenuItem value="draft">Borradores</MenuItem>
                    <MenuItem value="archived">Archivados</MenuItem>
                  </TextField>
                )}
                {showHistoryMinVersionFilter && (
                  <TextField
                    size="small"
                    type="number"
                    label="Versión mínima"
                    value={minVersionFilter ?? ''}
                    onChange={(e) => setMinVersionFilter(parseMinVersionFilter(e.target.value))}
                    sx={{ width: 150 }}
                  />
                )}
                {showVersionToolbarReset && (
                  <Button
                    size="small"
                    onClick={resetVersionFilters}
                  >
                    Limpiar filtros
                  </Button>
                )}
              </Stack>
            )}
          </Stack>
          {listQuery.isLoading && <LinearProgress />}
                  {listQuery.error && (
                    <ApiErrorNotice
                      error={listQuery.error}
                      title="No pudimos cargar la lista de versiones"
                      onRetry={() => {
                        void listQuery.refetch();
                      }}
                      showCorsHint
                    />
                  )}
          {listDataInvalid && (
            <Alert severity="warning">
              Respuesta inesperada del servidor. Revisa las credenciales o intenta de nuevo.
            </Alert>
          )}
          <Stack spacing={1.5}>
            {visibleVersionRows.map((v) => {
              const isCurrentLiveVersion = liveContent?.ccdId === v.ccdId;
              const rowStatus = normalizeCmsStatus(v.ccdStatus);
              const rowHasPublishedTimestamp = Boolean(v.ccdPublishedAt);
              const rowNeedsPublishReview = liveVersion !== null && v.ccdVersion < liveVersion;
              const showRowStatusChip =
                !sharedVersionStatus
                && !isCurrentLiveVersion
                && !(rowStatus === 'published' && rowHasPublishedTimestamp);
              const rowActions = getCmsVersionRowActions(v.ccdStatus, {
                isCurrentLive: isCurrentLiveVersion,
                isLoadedInEditor: editingFromId === v.ccdId,
              });
              const rowTitle = sharedVersionTitle ? `Versión v${v.ccdVersion}` : (v.ccdTitle ?? v.ccdSlug);

              return (
                <Paper key={v.ccdId} variant="outlined" sx={{ p: 1.5, borderRadius: 2 }}>
                  <Stack
                    direction={{ xs: 'column', sm: 'row' }}
                    spacing={1}
                    alignItems={{ xs: 'flex-start', sm: 'center' }}
                  >
                    <Box sx={{ flexGrow: 1 }}>
                      <Typography fontWeight={700}>{rowTitle}</Typography>
                      <Stack direction="row" spacing={1} flexWrap="wrap" sx={{ mt: 0.5 }}>
                        {!sharedVersionSlug && <Chip label={v.ccdSlug} size="small" />}
                        {!sharedVersionLocale && <Chip label={formatLocaleLabel(v.ccdLocale)} size="small" />}
                        {!sharedVersionTitle && <Chip label={`v${v.ccdVersion}`} size="small" />}
                        {showRowStatusChip && (
                          <Chip
                            label={formatCmsStatusLabel(v.ccdStatus)}
                            size="small"
                            color={rowStatus === 'published' ? 'success' : 'default'}
                          />
                        )}
                        {v.ccdPublishedAt && !isCurrentLiveVersion && (
                          <Chip
                            label={`Publicado: ${formatCmsAdminTimestamp(v.ccdPublishedAt)}`}
                            size="small"
                            variant="outlined"
                          />
                        )}
                      </Stack>
                    </Box>
                    <Stack direction="row" spacing={1}>
                      {rowActions.showPublish && !rowNeedsPublishReview && (
                        <Button
                          size="small"
                          variant="outlined"
                          onClick={() => publishMutation.mutate(v.ccdId)}
                          disabled={publishMutation.isPending}
                        >
                          Publicar
                        </Button>
                      )}
                      {rowActions.showLoadInEditor ? (
                        <Button size="small" variant="text" onClick={() => handleLoadVersion(v)}>
                          Editar en formulario
                        </Button>
                      ) : rowActions.loadedStateLabel ? (
                        <Typography
                          variant="body2"
                          color="text.secondary"
                          sx={{ alignSelf: 'center', px: 0.5 }}
                        >
                          {rowActions.loadedStateLabel}
                        </Typography>
                      ) : null}
                      {rowActions.showDelete && (
                        <Button
                          size="small"
                          variant="text"
                          color="error"
                          onClick={() => deleteMutation.mutate(v.ccdId)}
                          disabled={deleteMutation.isPending}
                        >
                          Borrar
                        </Button>
                      )}
                    </Stack>
                  </Stack>
                </Paper>
              );
            })}
            {showSingleLiveVersionSummary && !listQuery.isLoading && (
              <Typography color="text.secondary">
                La única versión guardada ya está resumida arriba; el historial aparecerá cuando guardes otra versión.
              </Typography>
            )}
            {showVersionHistoryEmptyState && (
              showVersionEmptyResetAction ? (
                <Alert
                  severity="info"
                  action={(
                    <Button color="inherit" size="small" onClick={resetVersionFilters}>
                      Limpiar filtros
                    </Button>
                  )}
                >
                  {versionListUiState.emptyMessage}
                </Alert>
              ) : (
                <Typography color="text.secondary">{versionListUiState.emptyMessage}</Typography>
              )
            )}
          </Stack>
        </Stack>
        </Paper>
      )}
    </Stack>
    </SessionGate>
  );
}
