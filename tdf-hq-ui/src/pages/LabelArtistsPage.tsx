import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  InputAdornment,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import EditIcon from '@mui/icons-material/Edit';
import PictureAsPdfIcon from '@mui/icons-material/PictureAsPdf';
import RefreshIcon from '@mui/icons-material/Refresh';
import SearchIcon from '@mui/icons-material/Search';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import VisibilityIcon from '@mui/icons-material/Visibility';
import GoogleDriveUploadWidget from '../components/GoogleDriveUploadWidget';
import PageShell, { EmptyState, SkeletonCards } from '../components/PageShell';
import LazyPaginatedList from '../components/LazyPaginatedList';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { DateTime } from 'luxon';
import { Admin } from '../api/admin';
import { Parties } from '../api/parties';
import type { PartySelectorOption } from '../api/partySelector';
import { PartySelector } from '../components/party-selector/PartySelector';
import type {
  ArtistProfileDTO,
  ArtistProfileUpsert,
  ArtistPromoDayReportDTO,
  ArtistPromoSlotDTO,
  ArtistPromoSlotUpsert,
} from '../api/types';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { Catalogs, type CatalogItem } from '../api/catalogs';

interface ArtistFormState {
  partyId: number | null;
  displayName: string;
  slug: string;
  city: string;
  bio: string;
  heroImageUrl: string;
  spotifyArtistId: string;
  spotifyUrl: string;
  youtubeChannelId: string;
  youtubeUrl: string;
  websiteUrl: string;
  featuredVideoUrl: string;
  genreIds: string[];
  highlights: string;
}

interface PromotionFormState {
  startTime: string;
  medium: string;
  program: string;
  interviewerHost: string;
  bandMembers: string;
  status: string;
  notes: string;
}

interface BannerState {
  severity: 'success' | 'error' | 'info' | 'warning';
  message: string;
}

const DEFAULT_PROMOTION_TIME = '09:00';

function buildEmptyForm(): ArtistFormState {
  return {
    partyId: null,
    displayName: '',
    slug: '',
    city: '',
    bio: '',
    heroImageUrl: '',
    spotifyArtistId: '',
    spotifyUrl: '',
    youtubeChannelId: '',
    youtubeUrl: '',
    websiteUrl: '',
    featuredVideoUrl: '',
    genreIds: [],
    highlights: '',
  };
}

function buildEmptyPromotionForm(): PromotionFormState {
  return {
    startTime: DEFAULT_PROMOTION_TIME,
    medium: '',
    program: '',
    interviewerHost: '',
    bandMembers: '',
    status: '',
    notes: '',
  };
}

const toNullableField = (value: string) => {
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : null;
};

const todayInTimezone = (timezone: string) => DateTime.now().setZone(timezone).toISODate() ?? '';

const sortPromotionSlots = (slots: ArtistPromoSlotDTO[]) =>
  [...slots].sort((a, b) => {
    const timeCmp = a.apsStartTime.localeCompare(b.apsStartTime);
    if (timeCmp !== 0) return timeCmp;
    const mediumCmp = a.apsMedium.localeCompare(b.apsMedium);
    if (mediumCmp !== 0) return mediumCmp;
    return a.apsProgram.localeCompare(b.apsProgram);
  });

const buildPromotionPdfFilename = (artist: ArtistProfileDTO | null, day: string) => {
  const base = [
    artist?.apSlug?.trim(),
    artist?.apDisplayName
      ?.trim()
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-+|-+$/g, ''),
  ].find((candidate) => Boolean(candidate)) ?? `artista-${artist?.apArtistId ?? 'sin-id'}`;
  return `promo-diario-${base}-${day}.pdf`;
};

const triggerBlobDownload = (blob: Blob, fileName: string) => {
  const downloadObjectUrl = URL.createObjectURL(blob);
  const downloadAnchor = document.createElement('a');
  downloadAnchor.href = downloadObjectUrl;
  downloadAnchor.download = fileName;
  downloadAnchor.click();
  URL.revokeObjectURL(downloadObjectUrl);
};

const openBlobPreview = (blob: Blob, fallbackFileName: string) => {
  const previewObjectUrl = URL.createObjectURL(blob);
  const previewWindow = window.open(previewObjectUrl, '_blank', 'noopener,noreferrer');
  if (!previewWindow) {
    triggerBlobDownload(blob, fallbackFileName);
    return false;
  }
  window.setTimeout(() => URL.revokeObjectURL(previewObjectUrl), 60_000);
  return true;
};

function promotionFormFromSlot(slot: ArtistPromoSlotDTO): PromotionFormState {
  return {
    startTime: slot.apsStartTime,
    medium: slot.apsMedium,
    program: slot.apsProgram,
    interviewerHost: slot.apsInterviewerHost,
    bandMembers: slot.apsBandMembers,
    status: slot.apsStatus ?? '',
    notes: slot.apsNotes ?? '',
  };
}

function formFromArtist(artist: ArtistProfileDTO): ArtistFormState {
  return {
    partyId: artist.apArtistId,
    displayName: artist.apDisplayName,
    slug: artist.apSlug ?? '',
    city: artist.apCity ?? '',
    bio: artist.apBio ?? '',
    heroImageUrl: artist.apHeroImageUrl ?? '',
    spotifyArtistId: artist.apSpotifyArtistId ?? '',
    spotifyUrl: artist.apSpotifyUrl ?? '',
    youtubeChannelId: artist.apYoutubeChannelId ?? '',
    youtubeUrl: artist.apYoutubeUrl ?? '',
    websiteUrl: artist.apWebsiteUrl ?? '',
    featuredVideoUrl: artist.apFeaturedVideoUrl ?? '',
    genreIds: artist.apGenreIds ?? [],
    highlights: artist.apHighlights ?? '',
  };
}

interface ArtistQuickNoteEditorProps {
  artist: ArtistProfileDTO;
  onBanner: (banner: BannerState) => void;
}

function ArtistQuickNoteEditor({ artist, onBanner }: ArtistQuickNoteEditorProps) {
  const qc = useQueryClient();
  const [noteDraft, setNoteDraft] = useState('');
  const [isDirty, setIsDirty] = useState(false);
  const partyQuery = useQuery({
    queryKey: ['party', artist.apArtistId],
    queryFn: () => Parties.getOne(artist.apArtistId),
    staleTime: 5 * 60 * 1000,
  });

  useEffect(() => {
    if (!isDirty && partyQuery.data) {
      setNoteDraft(partyQuery.data.notes ?? '');
    }
  }, [isDirty, partyQuery.data]);

  const noteMutation = useMutation({
    mutationFn: (note: string) => Parties.update(artist.apArtistId, { uNotes: note.trim() }),
    onSuccess: (party) => {
      qc.setQueryData(['party', artist.apArtistId], party);
      setNoteDraft(party.notes ?? '');
      setIsDirty(false);
      onBanner({ severity: 'success', message: 'Nota guardada.' });
    },
    onError: (err: unknown) => {
      onBanner({
        severity: 'error',
        message: err instanceof Error ? err.message : 'No se pudo guardar la nota.',
      });
    },
  });

  const partyLoadFailed = Boolean(partyQuery.error);

  return (
    <Box
      sx={{
        border: '1px solid rgba(148,163,184,0.35)',
        borderRadius: 2,
        p: 1.5,
        display: 'flex',
        flexDirection: { xs: 'column', sm: 'row' },
        gap: 1,
        alignItems: { xs: 'stretch', sm: 'center' },
      }}
    >
      <Box sx={{ minWidth: 220 }}>
        <Typography fontWeight={700}>{artist.apDisplayName}</Typography>
        <Typography variant="body2" color="text.secondary">
          {artist.apCity ?? 'Sin ciudad'}
        </Typography>
      </Box>
      <TextField
        value={noteDraft}
        onChange={(event) => {
          setNoteDraft(event.target.value);
          setIsDirty(true);
        }}
        inputProps={{ 'aria-label': `Nota o pendiente para ${artist.apDisplayName}` }}
        placeholder={partyQuery.isLoading ? 'Cargando nota…' : 'Agregar nota o pendiente'}
        error={partyLoadFailed}
        helperText={partyLoadFailed ? 'No pudimos cargar la nota del contacto.' : undefined}
        disabled={partyQuery.isLoading || partyLoadFailed || noteMutation.isPending}
        fullWidth
        size="small"
        multiline
        minRows={1}
      />
      <Button
        variant="contained"
        aria-label={`Guardar nota para ${artist.apDisplayName}`}
        onClick={() => noteMutation.mutate(noteDraft)}
        disabled={partyQuery.isLoading || partyLoadFailed || noteMutation.isPending}
        sx={{ minWidth: 140 }}
      >
        {noteMutation.isPending ? 'Guardando…' : 'Guardar'}
      </Button>
    </Box>
  );
}

export default function LabelArtistsPage() {
  const qc = useQueryClient();
  const { locale, timezone } = useLocalePreferences();
  const [search, setSearch] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [selectedArtist, setSelectedArtist] = useState<ArtistProfileDTO | null>(null);
  const [selectedParty, setSelectedParty] = useState<PartySelectorOption | null>(null);
  const [form, setForm] = useState<ArtistFormState>(buildEmptyForm);
  const [formError, setFormError] = useState<string | null>(null);
  const [banner, setBanner] = useState<BannerState | null>(null);
  const [heroImageFileName, setHeroImageFileName] = useState('');
  const [heroImageError, setHeroImageError] = useState<string | null>(null);
  const [promotionArtistId, setPromotionArtistId] = useState<number | null>(null);
  const [promotionDay, setPromotionDay] = useState(() => todayInTimezone(timezone));
  const [promotionForm, setPromotionForm] = useState<PromotionFormState>(buildEmptyPromotionForm);
  const [promotionFormError, setPromotionFormError] = useState<string | null>(null);
  const [editingPromotionId, setEditingPromotionId] = useState<number | null>(null);

  const artistsQuery = useQuery({
    queryKey: ['admin', 'artists'],
    queryFn: () => Admin.listArtistProfiles(),
  });
  const genresCatalogQuery = useQuery({
    queryKey: ['catalog', 'genres', locale],
    queryFn: () => Catalogs.listItems('genres', { locale, page: 1, pageSize: 500, includeInactive: true }),
    staleTime: 5 * 60 * 1000,
    retry: (failureCount, error) => {
      // Retry on network errors or 5xx, but not on 4xx (auth issues)
      if (error && typeof error === 'object' && 'status' in error) {
        const status = (error as { status: number }).status;
        if (status >= 400 && status < 500) return false;
      }
      return failureCount < 2;
    },
    retryDelay: 1000,
  });

  const artists = useMemo(() => artistsQuery.data ?? [], [artistsQuery.data]);
  const genreOptions = useMemo<CatalogItem[]>(
    () => (genresCatalogQuery.data?.items ?? [])
      .filter((genre) => genre.active && genre.workflowState === 'published')
      .sort((a, b) => a.sortOrder - b.sortOrder || a.name.localeCompare(b.name)),
    [genresCatalogQuery.data?.items],
  );
  const unavailableFormGenreIds = useMemo(
    () => form.genreIds.filter((genreId) => !genreOptions.some((genre) => genre.id === genreId)),
    [form.genreIds, genreOptions],
  );
  const selectedPromotionArtist = useMemo(
    () => artists.find((artist) => artist.apArtistId === promotionArtistId) ?? null,
    [artists, promotionArtistId],
  );

  const promotionsQuery = useQuery({
    queryKey: ['admin', 'artist-promotions', promotionArtistId, promotionDay],
    queryFn: () => Admin.listArtistPromoSlots(promotionArtistId!, promotionDay),
    enabled: Boolean(promotionArtistId && promotionDay),
  });
  const promotionReportQuery = useQuery({
    queryKey: ['admin', 'artist-promotion-report', promotionArtistId, promotionDay],
    queryFn: () => Admin.getArtistPromoDayReport(promotionArtistId!, promotionDay),
    enabled: Boolean(promotionArtistId && promotionDay),
  });

  const promotionSlots = useMemo(
    () => sortPromotionSlots(promotionsQuery.data ?? []),
    [promotionsQuery.data],
  );
  const promotionReport = useMemo<ArtistPromoDayReportDTO | null>(() => {
    const report = promotionReportQuery.data ?? (selectedPromotionArtist ? {
      apdArtistId: selectedPromotionArtist.apArtistId,
      apdArtistName: selectedPromotionArtist.apDisplayName,
      apdDay: promotionDay,
      apdTimezone: timezone,
      apdDayHeader: '',
      apdEntries: promotionSlots,
    } : null);
    if (!report) return null;
    const localizedDay = DateTime.fromISO(promotionDay, { zone: timezone }).setLocale(locale);
    return {
      ...report,
      apdTimezone: timezone,
      apdDayHeader: localizedDay.isValid ? localizedDay.toLocaleString(DateTime.DATE_FULL) : promotionDay,
    };
  }, [locale, promotionDay, promotionReportQuery.data, promotionSlots, selectedPromotionArtist, timezone]);

  const sortedArtists = useMemo(
    () => [...artists].sort((a, b) => a.apDisplayName.localeCompare(b.apDisplayName)),
    [artists],
  );

  useEffect(() => {
    if (artists.length === 0) {
      setPromotionArtistId(null);
      return;
    }
    setPromotionArtistId((prev) =>
      prev && artists.some((artist) => artist.apArtistId === prev) ? prev : artists[0]!.apArtistId,
    );
  }, [artists]);

  useEffect(() => {
    if (!form.heroImageUrl) {
      setHeroImageFileName('');
      return;
    }
    if (heroImageFileName) return;
    setHeroImageFileName(form.heroImageUrl.startsWith('data:') ? 'Imagen seleccionada' : 'Imagen existente');
  }, [form.heroImageUrl, heroImageFileName]);

  useEffect(() => {
    setPromotionForm(buildEmptyPromotionForm());
    setPromotionFormError(null);
    setEditingPromotionId(null);
  }, [promotionArtistId, promotionDay]);

  const filteredArtists = useMemo(() => {
    const term = search.trim().toLowerCase();
    if (!term) return sortedArtists;
    return sortedArtists.filter((artist) => {
      const haystack = [
        artist.apDisplayName,
        artist.apSlug ?? '',
        artist.apCity ?? '',
        artist.apGenres ?? '',
        artist.apHighlights ?? '',
      ]
        .join(' ')
        .toLowerCase();
      return haystack.includes(term);
    });
  }, [search, sortedArtists]);
  const hasArtistProfiles = artists.length > 0;
  const hasArtistSearch = search.trim().length > 0;
  const showArtistSearch = hasArtistProfiles || hasArtistSearch;
  const showArtistRefresh = hasArtistProfiles || Boolean(artistsQuery.error);
  const showQuickNotesCard = hasArtistProfiles;
  const showFirstArtistSetup = !artistsQuery.isLoading && !artistsQuery.error && !hasArtistProfiles;

  const handleHeroImageFileChange = (file: File | null) => {
    if (!file) {
      setForm((prev) => ({ ...prev, heroImageUrl: '' }));
      setHeroImageFileName('');
      return;
    }
    const maxBytes = 6 * 1024 * 1024;
    if (file.size > maxBytes) {
      setHeroImageError('El archivo supera 6 MB. Usa una imagen más liviana.');
      return;
    }
    const reader = new FileReader();
    reader.onload = () => {
      if (typeof reader.result === 'string') {
        const dataUrl = reader.result;
        setForm((prev) => ({ ...prev, heroImageUrl: dataUrl }));
        setHeroImageFileName(file.name);
        setHeroImageError(null);
      } else {
        setHeroImageError('No pudimos leer la imagen seleccionada.');
        setForm((prev) => ({ ...prev, heroImageUrl: '' }));
      }
    };
    reader.onerror = () => setHeroImageError('No pudimos leer la imagen seleccionada.');
    reader.readAsDataURL(file);
  };

  const upsertMutation = useMutation({
    mutationFn: async (payload: { draft: ArtistFormState; originalDisplayName: string }) => {
      const { draft, originalDisplayName } = payload;
      if (!draft.partyId) {
        throw new Error('Selecciona un contacto del CRM para enlazar el perfil de artista.');
      }
      const trimmedName = draft.displayName.trim();
      if (!trimmedName) {
        throw new Error('Agrega un nombre público para el artista.');
      }
      if (trimmedName !== originalDisplayName) {
        await Parties.update(draft.partyId, { uDisplayName: trimmedName });
      }
      const body: ArtistProfileUpsert = {
        apuArtistId: draft.partyId,
        apuSlug: toNullableField(draft.slug),
        apuBio: toNullableField(draft.bio),
        apuCity: toNullableField(draft.city),
        apuHeroImageUrl: toNullableField(draft.heroImageUrl),
        apuSpotifyArtistId: toNullableField(draft.spotifyArtistId),
        apuSpotifyUrl: toNullableField(draft.spotifyUrl),
        apuYoutubeChannelId: toNullableField(draft.youtubeChannelId),
        apuYoutubeUrl: toNullableField(draft.youtubeUrl),
        apuWebsiteUrl: toNullableField(draft.websiteUrl),
        apuFeaturedVideoUrl: toNullableField(draft.featuredVideoUrl),
        apuGenreIds: draft.genreIds,
        apuHighlights: toNullableField(draft.highlights),
      };
      return Admin.upsertArtistProfile(body);
    },
    onSuccess: (dto) => {
      setBanner({ severity: 'success', message: `Perfil de ${dto.apDisplayName} guardado.` });
      setDialogOpen(false);
      setSelectedArtist(null);
      setForm(buildEmptyForm());
      setFormError(null);
      void qc.invalidateQueries({ queryKey: ['admin', 'artists'] });
      void qc.invalidateQueries({ queryKey: ['fan-artists'] });
      void qc.invalidateQueries({ queryKey: ['parties'] });
    },
    onError: (err: unknown) => {
      setFormError(err instanceof Error ? err.message : 'No se pudo guardar el perfil.');
    },
  });

  const savePromotionMutation = useMutation({
    mutationFn: async ({
      artistId,
      day,
      draft,
      promotionId,
    }: {
      artistId: number;
      day: string;
      draft: PromotionFormState;
      promotionId?: number | null;
    }) => {
      const payload: ArtistPromoSlotUpsert = {
        apsuDay: day,
        apsuStartTime: draft.startTime,
        apsuMedium: draft.medium.trim(),
        apsuProgram: draft.program.trim(),
        apsuInterviewerHost: draft.interviewerHost.trim(),
        apsuBandMembers: draft.bandMembers.trim(),
        apsuStatus: toNullableField(draft.status),
        apsuNotes: toNullableField(draft.notes),
      };
      if (!payload.apsuStartTime.trim()) {
        throw new Error('Define una hora para el espacio promocional.');
      }
      if (!payload.apsuMedium) throw new Error('El medio es obligatorio.');
      if (!payload.apsuProgram) throw new Error('El programa es obligatorio.');
      if (!payload.apsuInterviewerHost) throw new Error('El entrevistador o host es obligatorio.');
      if (!payload.apsuBandMembers) throw new Error('Indica los miembros participantes.');
      return promotionId
        ? Admin.updateArtistPromoSlot(artistId, promotionId, payload)
        : Admin.createArtistPromoSlot(artistId, payload);
    },
    onSuccess: async (slot, variables) => {
      setBanner({
        severity: 'success',
        message: variables.promotionId ? 'Espacio promocional actualizado.' : 'Espacio promocional creado.',
      });
      setPromotionForm(buildEmptyPromotionForm());
      setPromotionFormError(null);
      setEditingPromotionId(null);
      await Promise.all([
        qc.invalidateQueries({ queryKey: ['admin', 'artist-promotions', slot.apsArtistId, variables.day] }),
        qc.invalidateQueries({ queryKey: ['admin', 'artist-promotion-report', slot.apsArtistId, variables.day] }),
      ]);
    },
    onError: (err: unknown) => {
      setPromotionFormError(err instanceof Error ? err.message : 'No se pudo guardar el espacio promocional.');
    },
  });

  const deletePromotionMutation = useMutation({
    mutationFn: ({ artistId, promotionId }: { artistId: number; promotionId: number; day: string }) =>
      Admin.deleteArtistPromoSlot(artistId, promotionId),
    onSuccess: async (_, variables) => {
      if (editingPromotionId === variables.promotionId) {
        setPromotionForm(buildEmptyPromotionForm());
        setPromotionFormError(null);
        setEditingPromotionId(null);
      }
      setBanner({ severity: 'success', message: 'Espacio promocional eliminado.' });
      await Promise.all([
        qc.invalidateQueries({ queryKey: ['admin', 'artist-promotions', variables.artistId, variables.day] }),
        qc.invalidateQueries({ queryKey: ['admin', 'artist-promotion-report', variables.artistId, variables.day] }),
      ]);
    },
    onError: (err: unknown) => {
      setBanner({ severity: 'error', message: err instanceof Error ? err.message : 'No se pudo eliminar el espacio promocional.' });
    },
  });

  const previewPdfMutation = useMutation({
    mutationFn: ({ artistId, day }: { artistId: number; day: string }) => Admin.getArtistPromoPdfBlob(artistId, day),
    onSuccess: (blob) => {
      const opened = openBlobPreview(blob, buildPromotionPdfFilename(selectedPromotionArtist, promotionDay));
      setBanner({
        severity: 'success',
        message: opened ? 'Vista previa PDF abierta en otra pestaña.' : 'No se pudo abrir la pestaña; descargamos el PDF.',
      });
    },
    onError: (err: unknown) => {
      setBanner({ severity: 'error', message: err instanceof Error ? err.message : 'No se pudo abrir la vista previa PDF.' });
    },
  });

  const downloadPdfMutation = useMutation({
    mutationFn: ({ artistId, day }: { artistId: number; day: string }) => Admin.getArtistPromoPdfBlob(artistId, day),
    onSuccess: (blob) => {
      triggerBlobDownload(blob, buildPromotionPdfFilename(selectedPromotionArtist, promotionDay));
      setBanner({ severity: 'success', message: 'PDF de promoción descargado.' });
    },
    onError: (err: unknown) => {
      setBanner({ severity: 'error', message: err instanceof Error ? err.message : 'No se pudo descargar el PDF de promoción.' });
    },
  });

  const handleOpenNew = () => {
    setSelectedArtist(null);
    setSelectedParty(null);
    setForm(buildEmptyForm());
    setFormError(null);
    setDialogOpen(true);
    setHeroImageFileName('');
    setHeroImageError(null);
  };

  const handleEdit = (artist: ArtistProfileDTO) => {
    setSelectedArtist(artist);
    setSelectedParty({ partyId: artist.apArtistId, partyType: 'person', displayName: artist.apDisplayName, username: null, avatarUrl: artist.apHeroImageUrl ?? null, secondaryLabel: 'Perfil de artista existente', accountStatus: 'no-account' });
    setForm(formFromArtist(artist));
    setFormError(null);
    setDialogOpen(true);
    setHeroImageError(null);
  };

  const handleCloseDialog = () => {
    if (upsertMutation.isPending) return;
    setDialogOpen(false);
    setSelectedArtist(null);
    setSelectedParty(null);
    setForm(buildEmptyForm());
    setFormError(null);
  };

  const handleSubmit = () => {
    if (!genresCatalogQuery.isSuccess) {
      setFormError('Espera a que el catálogo de géneros esté disponible antes de guardar.');
      return;
    }
    if (unavailableFormGenreIds.length > 0) {
      setFormError('Sustituye los géneros inactivos o reemplazados antes de guardar.');
      return;
    }
    const originalName = selectedParty?.displayName ?? selectedArtist?.apDisplayName ?? '';
    upsertMutation.mutate({ draft: form, originalDisplayName: originalName });
  };

  const handlePromotionEdit = (slot: ArtistPromoSlotDTO) => {
    setEditingPromotionId(slot.apsPromotionId);
    setPromotionForm(promotionFormFromSlot(slot));
    setPromotionFormError(null);
  };

  const handlePromotionCancel = () => {
    setEditingPromotionId(null);
    setPromotionForm(buildEmptyPromotionForm());
    setPromotionFormError(null);
  };

  const handlePromotionSubmit = () => {
    if (!promotionArtistId) {
      setPromotionFormError('Selecciona un artista para registrar la agenda promocional.');
      return;
    }
    savePromotionMutation.mutate({
      artistId: promotionArtistId,
      day: promotionDay,
      draft: promotionForm,
      promotionId: editingPromotionId,
    });
  };

  const handlePromotionDelete = (slot: ArtistPromoSlotDTO) => {
    if (!promotionArtistId) return;
    const confirmed = window.confirm(
      `Eliminar el espacio de ${slot.apsStartTime} en ${slot.apsMedium} para ${selectedPromotionArtist?.apDisplayName ?? 'este artista'}?`,
    );
    if (!confirmed) return;
    deletePromotionMutation.mutate({ artistId: promotionArtistId, promotionId: slot.apsPromotionId, day: promotionDay });
  };

  const handlePromotionPreviewRefresh = () => {
    void promotionReportQuery.refetch();
  };

  const handlePromotionPdfPreview = () => {
    if (!promotionArtistId) return;
    previewPdfMutation.mutate({ artistId: promotionArtistId, day: promotionDay });
  };

  const handlePromotionPdfDownload = () => {
    if (!promotionArtistId) return;
    downloadPdfMutation.mutate({ artistId: promotionArtistId, day: promotionDay });
  };

  const renderLinkChip = (label: string, url: string | null) => {
    if (!url) return null;
    return (
      <Chip
        key={label}
        label={label}
        size="small"
        component="a"
        href={url}
        target="_blank"
        rel="noreferrer"
        clickable
        icon={<OpenInNewIcon sx={{ fontSize: 16 }} />}
        variant="outlined"
      />
    );
  };

  return (
    <PageShell
      title="Artistas"
      subtitle="Administra los perfiles que alimentan la comunidad y los lanzamientos del label."
      actions={(
        <Button variant="contained" startIcon={<AddIcon />} onClick={handleOpenNew}>
          Nuevo perfil
        </Button>
      )}
    >
      <Stack spacing={3}>
      <Typography variant="caption" color="text.secondary">
        Label / Artistas
      </Typography>
      {banner && (
        <Alert severity={banner.severity} onClose={() => setBanner(null)}>
          {banner.message}
        </Alert>
      )}
      {(showArtistSearch || showArtistRefresh) && (
        <Stack direction="row" spacing={1} alignItems="center">
          {showArtistSearch && (
            <TextField
              size="small"
              aria-label="Buscar artistas"
              placeholder="Buscar por nombre, slug o ciudad"
              value={search}
              onChange={(event) => setSearch(event.target.value)}
              inputProps={{ 'aria-label': 'Buscar artistas' }}
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <SearchIcon fontSize="small" />
                  </InputAdornment>
                ),
              }}
              sx={{ minWidth: { xs: 200, md: 280 } }}
            />
          )}
          {showArtistRefresh && (
            <Button
              tabIndex={0}
              onClick={(event) => {
                const refreshButton = event.currentTarget;
                void artistsQuery.refetch().finally(() => {
                  window.setTimeout(() => {
                    if (refreshButton.isConnected) refreshButton.focus();
                  }, 0);
                });
              }}
              aria-label="Refrescar artistas"
              disabled={artistsQuery.isFetching}
              size="small"
              startIcon={<RefreshIcon />}
              variant="outlined"
            >
              {artistsQuery.isFetching ? 'Actualizando' : 'Refrescar'}
            </Button>
          )}
        </Stack>
      )}
      {showQuickNotesCard && (
        <Card>
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="h6">Notas rápidas por artista</Typography>
              <Typography variant="body2" color="text.secondary">
                Usa este espacio para pendientes breves; se guardan en las notas del contacto (Party.notes) y se reutilizan en el CRM.
              </Typography>
              <Stack spacing={1.5}>
                {filteredArtists.length === 0 && (
                  <EmptyState
                    title="Sin artistas"
                    description="Aún no hay perfiles de artista. Crea el primero para empezar."
                    actionLabel="Nuevo perfil"
                    actionOnClick={handleOpenNew}
                  />
                )}
                <LazyPaginatedList
                  items={filteredArtists}
                  pagination={{ itemLabel: 'artistas', initialRowsPerPage: 10, resetKey: search.trim() }}
                  renderItems={(visibleArtists) => (
                    <Stack spacing={1.5}>
                      {visibleArtists.map((artist) => (
                        <ArtistQuickNoteEditor
                          key={artist.apArtistId}
                          artist={artist}
                          onBanner={setBanner}
                        />
                      ))}
                    </Stack>
                  )}
                />
              </Stack>
            </Stack>
          </CardContent>
        </Card>
      )}

      <Card>
        <CardContent>
          {artistsQuery.isLoading && <SkeletonCards count={4} />}
          {artistsQuery.error && (
            <Alert severity="error">
              No pudimos cargar los artistas. Verifica tus permisos de admin.
            </Alert>
          )}
          {showFirstArtistSetup && (
            <EmptyState
              title="Sin artistas"
              description="Todavía no hay perfiles de artista. Usa Nuevo perfil para enlazar el primer contacto del CRM; la búsqueda, notas rápidas, refresco y tabla aparecerán cuando exista al menos un perfil."
            />
          )}
          {!showFirstArtistSetup && !artistsQuery.isLoading && filteredArtists.length === 0 && !artistsQuery.error && (
            <EmptyState
              title="Sin coincidencias"
              description="No hay perfiles de artista que coincidan con la búsqueda."
            />
          )}
          {filteredArtists.length > 0 && (
            <LazyPaginatedList
              items={filteredArtists}
              pagination={{ itemLabel: 'artistas', initialRowsPerPage: 25, resetKey: search.trim() }}
              renderItems={(visibleArtists) => (
                <Box sx={{ overflowX: 'auto' }}>
                  <Table size="small">
                    <TableHead>
                      <TableRow>
                        <TableCell>Artista</TableCell>
                        <TableCell>Slug</TableCell>
                        <TableCell>Fans</TableCell>
                        <TableCell>Cuenta</TableCell>
                        <TableCell>Ciudad</TableCell>
                        <TableCell>Enlaces</TableCell>
                        <TableCell align="right">Acciones</TableCell>
                      </TableRow>
                    </TableHead>
                    <TableBody>
                      {visibleArtists.map((artist) => {
                    const spotifyUrl =
                      artist.apSpotifyUrl ??
                      (artist.apSpotifyArtistId ? `https://open.spotify.com/artist/${artist.apSpotifyArtistId}` : null);
                    const youtubeUrl =
                      artist.apYoutubeUrl ??
                      (artist.apYoutubeChannelId
                        ? `https://www.youtube.com/channel/${artist.apYoutubeChannelId}`
                        : null);
                    const websiteUrl = artist.apWebsiteUrl ?? null;
                    const featuredVideoUrl = artist.apFeaturedVideoUrl ?? null;
                    return (
                      <TableRow key={artist.apArtistId} hover>
                        <TableCell>
                          <Stack spacing={0.5}>
                            <Typography variant="subtitle1" fontWeight={700}>
                              {artist.apDisplayName}
                            </Typography>
                            <Typography variant="body2" color="text.secondary">
                              ID {artist.apArtistId}
                            </Typography>
                            {(artist.apGenres ?? artist.apHighlights) && (
                              <Typography variant="body2" color="text.secondary">
                                {[artist.apGenres, artist.apHighlights].filter(Boolean).join(' · ')}
                              </Typography>
                            )}
                          </Stack>
                        </TableCell>
                        <TableCell>{artist.apSlug ?? '—'}</TableCell>
                        <TableCell>
                          <Chip label={`${artist.apFollowerCount} fans`} size="small" color="secondary" />
                        </TableCell>
                        <TableCell>
                          <Chip
                            label={artist.apHasUserAccount ? 'Con cuenta' : 'Sin cuenta'}
                            color={artist.apHasUserAccount ? 'success' : 'default'}
                            size="small"
                            variant={artist.apHasUserAccount ? 'filled' : 'outlined'}
                          />
                        </TableCell>
                        <TableCell>{artist.apCity ?? '—'}</TableCell>
                        <TableCell>
                          <Stack direction="row" spacing={0.5} flexWrap="wrap">
                            {renderLinkChip('Spotify', spotifyUrl)}
                            {renderLinkChip('YouTube', youtubeUrl)}
                            {renderLinkChip('Sitio', websiteUrl)}
                            {renderLinkChip('Video', featuredVideoUrl)}
                          </Stack>
                        </TableCell>
                        <TableCell align="right">
                          <Tooltip title="Editar perfil">
                            <IconButton
                              size="small"
                              onClick={() => handleEdit(artist)}
                              aria-label={`Editar perfil de ${artist.apDisplayName || `artista ${artist.apArtistId}`}`}
                            >
                              <EditIcon fontSize="small" />
                            </IconButton>
                          </Tooltip>
                        </TableCell>
                      </TableRow>
                    );
                      })}
                    </TableBody>
                  </Table>
                </Box>
              )}
            />
          )}
        </CardContent>
      </Card>

      {hasArtistProfiles && (
        <Card>
          <CardContent>
            <Stack spacing={2.5}>
              <Stack spacing={0.5}>
                <Typography variant="h6">Promoción diaria y reporte PDF</Typography>
                <Typography variant="body2" color="text.secondary">
                  Gestiona la agenda promocional por artista y genera el PDF diario ordenado por hora.
                </Typography>
              </Stack>

              <Alert severity="info" variant="outlined">
                El reporte usa tu zona horaria configurada ({timezone}) y un PDF por artista + día seleccionado.
              </Alert>

              <Stack direction={{ xs: 'column', lg: 'row' }} spacing={2} alignItems={{ xs: 'stretch', lg: 'center' }}>
                <Autocomplete
                  options={sortedArtists}
                  value={selectedPromotionArtist}
                  onChange={(_, value) => setPromotionArtistId(value?.apArtistId ?? null)}
                  getOptionLabel={(option) => option.apDisplayName}
                  isOptionEqualToValue={(option, value) => option.apArtistId === value.apArtistId}
                  sx={{ minWidth: { xs: '100%', md: 280 } }}
                  renderInput={(params) => (
                    <TextField
                      {...params}
                      label="Artista del reporte"
                      helperText="Cada PDF se genera para un artista y un día concretos."
                    />
                  )}
                />
                <TextField
                  label="Día"
                  type="date"
                  value={promotionDay}
                  onChange={(event) => setPromotionDay(event.target.value)}
                  InputLabelProps={{ shrink: true }}
                  sx={{ minWidth: { xs: '100%', md: 220 } }}
                />
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} flexWrap="wrap">
                  <Button
                    variant="outlined"
                    startIcon={<RefreshIcon />}
                    onClick={handlePromotionPreviewRefresh}
                    disabled={!promotionArtistId || promotionReportQuery.isFetching}
                  >
                    {promotionReportQuery.isFetching ? 'Actualizando…' : 'Vista previa'}
                  </Button>
                  <Button
                    variant="outlined"
                    startIcon={<VisibilityIcon />}
                    onClick={handlePromotionPdfPreview}
                    disabled={!promotionArtistId || previewPdfMutation.isPending}
                  >
                    {previewPdfMutation.isPending ? 'Generando PDF…' : 'Ver PDF'}
                  </Button>
                  <Button
                    variant="contained"
                    startIcon={<PictureAsPdfIcon />}
                    onClick={handlePromotionPdfDownload}
                    disabled={!promotionArtistId || downloadPdfMutation.isPending}
                  >
                    {downloadPdfMutation.isPending ? 'Descargando…' : 'Descargar PDF'}
                  </Button>
                </Stack>
              </Stack>

              <Stack direction={{ xs: 'column', xl: 'row' }} spacing={2} alignItems="stretch">
                <Card variant="outlined" sx={{ flex: 1 }}>
                  <CardContent>
                    <Stack spacing={2}>
                      <Typography variant="subtitle1" fontWeight={700}>
                        {editingPromotionId ? 'Editar espacio promocional' : 'Nuevo espacio promocional'}
                      </Typography>
                      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                        <TextField
                          label="Hora"
                          type="time"
                          value={promotionForm.startTime}
                          onChange={(event) => setPromotionForm((prev) => ({ ...prev, startTime: event.target.value }))}
                          InputLabelProps={{ shrink: true }}
                          inputProps={{ step: 60 }}
                          fullWidth
                        />
                        <TextField
                          label="Medio"
                          value={promotionForm.medium}
                          onChange={(event) => setPromotionForm((prev) => ({ ...prev, medium: event.target.value }))}
                          fullWidth
                        />
                      </Stack>
                      <TextField
                        label="Programa"
                        value={promotionForm.program}
                        onChange={(event) => setPromotionForm((prev) => ({ ...prev, program: event.target.value }))}
                        fullWidth
                      />
                      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                        <TextField
                          label="Entrevistador / host"
                          value={promotionForm.interviewerHost}
                          onChange={(event) =>
                            setPromotionForm((prev) => ({ ...prev, interviewerHost: event.target.value }))
                          }
                          fullWidth
                        />
                        <TextField
                          label="Miembros participantes"
                          value={promotionForm.bandMembers}
                          onChange={(event) => setPromotionForm((prev) => ({ ...prev, bandMembers: event.target.value }))}
                          fullWidth
                        />
                      </Stack>
                      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
                        <TextField
                          label="Estado (opcional)"
                          value={promotionForm.status}
                          onChange={(event) => setPromotionForm((prev) => ({ ...prev, status: event.target.value }))}
                          fullWidth
                        />
                        <TextField
                          label="Notas (opcional)"
                          value={promotionForm.notes}
                          onChange={(event) => setPromotionForm((prev) => ({ ...prev, notes: event.target.value }))}
                          fullWidth
                          multiline
                          minRows={2}
                        />
                      </Stack>
                      {promotionFormError && <Alert severity="error">{promotionFormError}</Alert>}
                      <Stack direction="row" spacing={1}>
                        <Button
                          variant="contained"
                          onClick={handlePromotionSubmit}
                          disabled={!promotionArtistId || savePromotionMutation.isPending}
                        >
                          {savePromotionMutation.isPending
                            ? 'Guardando…'
                            : editingPromotionId
                              ? 'Actualizar espacio'
                              : 'Guardar espacio'}
                        </Button>
                        {editingPromotionId && (
                          <Button variant="text" color="inherit" onClick={handlePromotionCancel}>
                            Cancelar edición
                          </Button>
                        )}
                      </Stack>
                    </Stack>
                  </CardContent>
                </Card>

                <Card variant="outlined" sx={{ flex: 1 }}>
                  <CardContent>
                    <Stack spacing={1.5}>
                      <Typography variant="subtitle1" fontWeight={700}>Vista previa del reporte</Typography>
                      {promotionReport && (
                        <>
                          <Typography variant="body1" fontWeight={700}>
                            {promotionReport.apdArtistName}
                          </Typography>
                          <Typography variant="body2" color="text.secondary">
                            {promotionReport.apdDayHeader} · {promotionReport.apdTimezone}
                          </Typography>
                        </>
                      )}
                      {promotionReportQuery.error && (
                        <Alert severity="error">
                          No pudimos cargar la vista previa del reporte para este día.
                        </Alert>
                      )}
                      {promotionReportQuery.isLoading && <Typography color="text.secondary">Cargando vista previa…</Typography>}
                      {promotionReport?.apdEntries.length === 0 && !promotionReportQuery.isLoading && (
                        <Alert severity="info" variant="outlined">
                          No hay espacios promocionales registrados para este artista en la fecha seleccionada.
                        </Alert>
                      )}
                      {promotionReport && promotionReport.apdEntries.length > 0 && (
                        <Box sx={{ overflowX: 'auto' }}>
                          <Table size="small">
                            <TableHead>
                              <TableRow>
                                <TableCell>Hora</TableCell>
                                <TableCell>Medio</TableCell>
                                <TableCell>Programa</TableCell>
                                <TableCell>Entrevistador / host</TableCell>
                                <TableCell>Miembros participantes</TableCell>
                                <TableCell>Estado</TableCell>
                                <TableCell>Notas</TableCell>
                              </TableRow>
                            </TableHead>
                            <TableBody>
                              {promotionReport.apdEntries.map((slot) => (
                                <TableRow key={`preview-${slot.apsPromotionId}`}>
                                  <TableCell>{slot.apsStartTime}</TableCell>
                                  <TableCell>{slot.apsMedium}</TableCell>
                                  <TableCell>{slot.apsProgram}</TableCell>
                                  <TableCell>{slot.apsInterviewerHost}</TableCell>
                                  <TableCell>{slot.apsBandMembers}</TableCell>
                                  <TableCell>{slot.apsStatus ?? '—'}</TableCell>
                                  <TableCell>{slot.apsNotes ?? '—'}</TableCell>
                                </TableRow>
                              ))}
                            </TableBody>
                          </Table>
                        </Box>
                      )}
                    </Stack>
                  </CardContent>
                </Card>
              </Stack>

              <Card variant="outlined">
                <CardContent>
                  <Stack spacing={1.5}>
                    <Typography variant="subtitle1" fontWeight={700}>Agenda editable del día</Typography>
                    <Typography variant="body2" color="text.secondary">
                      Esta tabla alimenta directamente la vista previa y el PDF diario. El backend la ordena por hora.
                    </Typography>
                    {promotionsQuery.error && (
                      <Alert severity="error">
                        No pudimos cargar la agenda promocional del artista seleccionado.
                      </Alert>
                    )}
                    {promotionsQuery.isLoading && <Typography color="text.secondary">Cargando agenda…</Typography>}
                    {!promotionsQuery.isLoading && !promotionsQuery.error && promotionSlots.length === 0 && (
                      <Alert severity="info" variant="outlined">
                        Todavía no hay espacios cargados para este día.
                      </Alert>
                    )}
                    {promotionSlots.length > 0 && (
                      <Box sx={{ overflowX: 'auto' }}>
                        <Table size="small">
                          <TableHead>
                            <TableRow>
                              <TableCell>Hora</TableCell>
                              <TableCell>Medio</TableCell>
                              <TableCell>Programa</TableCell>
                              <TableCell>Entrevistador / host</TableCell>
                              <TableCell>Miembros participantes</TableCell>
                              <TableCell>Estado</TableCell>
                              <TableCell>Notas</TableCell>
                              <TableCell align="right">Acciones</TableCell>
                            </TableRow>
                          </TableHead>
                          <TableBody>
                            {promotionSlots.map((slot) => (
                              <TableRow key={slot.apsPromotionId} hover>
                                <TableCell>{slot.apsStartTime}</TableCell>
                                <TableCell>{slot.apsMedium}</TableCell>
                                <TableCell>{slot.apsProgram}</TableCell>
                                <TableCell>{slot.apsInterviewerHost}</TableCell>
                                <TableCell>{slot.apsBandMembers}</TableCell>
                                <TableCell>{slot.apsStatus ?? '—'}</TableCell>
                                <TableCell>{slot.apsNotes ?? '—'}</TableCell>
                                <TableCell align="right">
                                  <Tooltip title="Editar espacio">
                                    <IconButton
                                      size="small"
                                      onClick={() => handlePromotionEdit(slot)}
                                      aria-label={`Editar espacio promocional ${slot.apsStartTime} ${slot.apsProgram}`}
                                    >
                                      <EditIcon fontSize="small" />
                                    </IconButton>
                                  </Tooltip>
                                  <Tooltip title="Eliminar espacio">
                                    <IconButton
                                      size="small"
                                      color="error"
                                      onClick={() => handlePromotionDelete(slot)}
                                      aria-label={`Eliminar espacio promocional ${slot.apsStartTime} ${slot.apsProgram}`}
                                      disabled={deletePromotionMutation.isPending}
                                    >
                                      <DeleteOutlineIcon fontSize="small" />
                                    </IconButton>
                                  </Tooltip>
                                </TableCell>
                              </TableRow>
                            ))}
                          </TableBody>
                        </Table>
                      </Box>
                    )}
                  </Stack>
                </CardContent>
              </Card>
            </Stack>
          </CardContent>
        </Card>
      )}

      <Dialog open={dialogOpen} onClose={handleCloseDialog} maxWidth="md" fullWidth>
        <DialogTitle>{selectedArtist ? 'Editar perfil de artista' : 'Nuevo perfil de artista'}</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2}>
            <PartySelector
              value={selectedParty}
              onChange={(value) => {
                setSelectedParty(value);
                setForm((prev) => ({
                  ...prev,
                  partyId: value?.partyId ?? null,
                  displayName: value?.displayName ?? prev.displayName,
                }));
              }}
              field={{ label: 'Contacto (CRM)', required: true, helperText: 'Busca por nombre o @username. Si falta, créalo en CRM → Contactos.' }}
              search={{ context: 'artist_link', kind: 'any', accountOnly: false }}
            />
            <TextField
              label="Nombre público (se guarda en el contacto)"
              value={form.displayName}
              onChange={(event) => setForm((prev) => ({ ...prev, displayName: event.target.value }))}
              required
            />
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
              <TextField
                label="Slug público"
                value={form.slug}
                onChange={(event) => setForm((prev) => ({ ...prev, slug: event.target.value }))}
                fullWidth
              />
              <TextField
                label="Ciudad"
                value={form.city}
                onChange={(event) => setForm((prev) => ({ ...prev, city: event.target.value }))}
                fullWidth
              />
            </Stack>
            <TextField
              label="Bio"
              multiline
              minRows={3}
              value={form.bio}
              onChange={(event) => setForm((prev) => ({ ...prev, bio: event.target.value }))}
            />
            <GoogleDriveUploadWidget
              label="Subir portada a Drive"
              helperText="Sube la imagen principal a Google Drive; guardaremos el enlace."
              onComplete={(files) => {
                const uploadedHeroImageUrl = files[0]?.publicUrl ?? files[0]?.webContentLink ?? files[0]?.webViewLink;
                if (uploadedHeroImageUrl) {
                  setForm((prev) => ({ ...prev, heroImageUrl: uploadedHeroImageUrl }));
                  setHeroImageFileName('Imagen en Drive');
                }
              }}
              accept="image/*"
              dense
            />
            <Stack spacing={1}>
              <Typography variant="body2" fontWeight={700}>
                Imagen principal
              </Typography>
              <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.5} alignItems="center">
                <Button component="label" startIcon={<UploadFileIcon />} variant="outlined">
                  Seleccionar imagen
                  <input
                    type="file"
                    accept="image/*"
                    hidden
                    onChange={(e) => handleHeroImageFileChange(e.target.files?.[0] ?? null)}
                  />
                </Button>
                {heroImageFileName && (
                  <Typography variant="body2" color="text.secondary">
                    {heroImageFileName}
                  </Typography>
                )}
                {form.heroImageUrl && (
                  <Button
                    variant="text"
                    color="inherit"
                    onClick={() => setForm((prev) => ({ ...prev, heroImageUrl: '' }))}
                  >
                    Quitar
                  </Button>
                )}
              </Stack>
              {form.heroImageUrl && (
                <Card
                  variant="outlined"
                  sx={{ maxWidth: 420, borderRadius: 2, borderColor: 'divider', overflow: 'hidden' }}
                >
                  <CardMedia component="img" height="180" image={form.heroImageUrl} alt="Vista previa" />
                </Card>
              )}
              {heroImageError && <Alert severity="warning">{heroImageError}</Alert>}
              <Typography variant="caption" color="text.secondary">
                Se guardará embebida (data URL). Usa imágenes livianas (&lt; 6 MB).
              </Typography>
            </Stack>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
              <TextField
                label="Spotify URL"
                value={form.spotifyUrl}
                onChange={(event) => setForm((prev) => ({ ...prev, spotifyUrl: event.target.value }))}
                fullWidth
              />
              <TextField
                label="Spotify Artist ID"
                value={form.spotifyArtistId}
                onChange={(event) => setForm((prev) => ({ ...prev, spotifyArtistId: event.target.value }))}
                fullWidth
              />
            </Stack>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
              <TextField
                label="YouTube URL"
                value={form.youtubeUrl}
                onChange={(event) => setForm((prev) => ({ ...prev, youtubeUrl: event.target.value }))}
                fullWidth
              />
              <TextField
                label="YouTube Channel ID"
                value={form.youtubeChannelId}
                onChange={(event) => setForm((prev) => ({ ...prev, youtubeChannelId: event.target.value }))}
                fullWidth
              />
            </Stack>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
              <TextField
                label="Sitio web"
                value={form.websiteUrl}
                onChange={(event) => setForm((prev) => ({ ...prev, websiteUrl: event.target.value }))}
                fullWidth
              />
              <TextField
                label="Video destacado"
                value={form.featuredVideoUrl}
                onChange={(event) => setForm((prev) => ({ ...prev, featuredVideoUrl: event.target.value }))}
                fullWidth
              />
            </Stack>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
              <Autocomplete
                multiple
                disabled={!genresCatalogQuery.isSuccess}
                options={genreOptions}
                value={genreOptions.filter((genre) => form.genreIds.includes(genre.id))}
                getOptionLabel={(genre) => genre.name}
                isOptionEqualToValue={(option, value) => option.id === value.id}
                onChange={(_event, selected) => setForm((prev) => ({
                  ...prev,
                  genreIds: selected.map((genre) => genre.id),
                }))}
                renderInput={(params) => (
                  <TextField
                    {...params}
                    label="Géneros"
                    helperText="Solo se pueden asignar géneros publicados y activos."
                  />
                )}
                fullWidth
              />
              <TextField
                label="Highlights"
                value={form.highlights}
                onChange={(event) => setForm((prev) => ({ ...prev, highlights: event.target.value }))}
                fullWidth
              />
            </Stack>
            {genresCatalogQuery.isError && (
              <Alert
                severity="error"
                action={
                  <Button
                    color="inherit"
                    size="small"
                    onClick={() => { void genresCatalogQuery.refetch(); }}
                    disabled={genresCatalogQuery.isRefetching}
                  >
                    {genresCatalogQuery.isRefetching ? 'Reintentando…' : 'Reintentar'}
                  </Button>
                }
              >
                No se pudo cargar el catálogo de géneros. Intenta nuevamente antes de guardar.
              </Alert>
            )}
            {unavailableFormGenreIds.length > 0 && !genresCatalogQuery.isLoading && (
              <Alert severity="warning">
                Este perfil referencia géneros inactivos o reemplazados. Sustitúyelos por valores vigentes.
              </Alert>
            )}
            {formError && <Alert severity="error">{formError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={handleSubmit}
            disabled={upsertMutation.isPending || !form.partyId || !genresCatalogQuery.isSuccess || unavailableFormGenreIds.length > 0}
          >
            {upsertMutation.isPending ? 'Guardando…' : 'Guardar'}
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
    </PageShell>
  );
}
