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
import EditIcon from '@mui/icons-material/Edit';
import RefreshIcon from '@mui/icons-material/Refresh';
import SearchIcon from '@mui/icons-material/Search';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Admin } from '../api/admin';
import { Parties } from '../api/parties';
import type { ArtistProfileDTO, ArtistProfileUpsert, PartyDTO } from '../api/types';

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
  genres: string;
  highlights: string;
}

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
    genres: '',
    highlights: '',
  };
}

const toNullableField = (value: string) => {
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : null;
};

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
    genres: artist.apGenres ?? '',
    highlights: artist.apHighlights ?? '',
  };
}

export default function LabelArtistsPage() {
  const qc = useQueryClient();
  const [search, setSearch] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [selectedArtist, setSelectedArtist] = useState<ArtistProfileDTO | null>(null);
  const [form, setForm] = useState<ArtistFormState>(buildEmptyForm);
  const [formError, setFormError] = useState<string | null>(null);
  const [bannerMessage, setBannerMessage] = useState<string | null>(null);
  const [heroImageFileName, setHeroImageFileName] = useState('');
  const [heroImageError, setHeroImageError] = useState<string | null>(null);

  const artistsQuery = useQuery({
    queryKey: ['admin', 'artists'],
    queryFn: () => Admin.listArtistProfiles(),
  });
  const partiesQuery = useQuery({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
  });

  const artists = useMemo(() => artistsQuery.data ?? [], [artistsQuery.data]);
  const parties = useMemo(() => partiesQuery.data ?? [], [partiesQuery.data]);

  const sortedArtists = useMemo(
    () => [...artists].sort((a, b) => a.apDisplayName.localeCompare(b.apDisplayName)),
    [artists],
  );
  useEffect(() => {
    if (!form.heroImageUrl) {
      setHeroImageFileName('');
      return;
    }
    if (heroImageFileName) return;
    setHeroImageFileName(form.heroImageUrl.startsWith('data:') ? 'Imagen seleccionada' : 'Imagen existente');
  }, [form.heroImageUrl, heroImageFileName]);

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

  const selectedParty = useMemo(
    () => parties.find((party) => party.partyId === form.partyId) ?? null,
    [parties, form.partyId],
  );

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
        apuGenres: toNullableField(draft.genres),
        apuHighlights: toNullableField(draft.highlights),
      };
      return Admin.upsertArtistProfile(body);
    },
    onSuccess: (dto) => {
      setBannerMessage(`Perfil de ${dto.apDisplayName} guardado.`);
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

  const handleOpenNew = () => {
    setSelectedArtist(null);
    setForm(buildEmptyForm());
    setFormError(null);
    setDialogOpen(true);
    setHeroImageFileName('');
    setHeroImageError(null);
  };

  const handleEdit = (artist: ArtistProfileDTO) => {
    setSelectedArtist(artist);
    setForm(formFromArtist(artist));
    setFormError(null);
    setDialogOpen(true);
    setHeroImageError(null);
  };

  const handleCloseDialog = () => {
    if (upsertMutation.isPending) return;
    setDialogOpen(false);
    setSelectedArtist(null);
    setForm(buildEmptyForm());
    setFormError(null);
  };

  const handleSubmit = () => {
    const originalName = selectedParty?.displayName ?? selectedArtist?.apDisplayName ?? '';
    upsertMutation.mutate({ draft: form, originalDisplayName: originalName });
  };

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'artists'] });
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
    <Stack spacing={3}>
      {bannerMessage && (
        <Alert severity="success" onClose={() => setBannerMessage(null)}>
          {bannerMessage}
        </Alert>
      )}
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems="flex-start">
        <Stack spacing={0.5}>
          <Typography variant="h4" fontWeight={700}>Label / Artistas</Typography>
          <Typography variant="body1" color="text.secondary">
            Administra los perfiles que alimentan el Fan Hub y los lanzamientos del label.
          </Typography>
        </Stack>
        <Stack direction="row" spacing={1} alignItems="center">
          <TextField
            size="small"
            placeholder="Buscar por nombre, slug o ciudad"
            value={search}
            onChange={(event) => setSearch(event.target.value)}
            InputProps={{
              startAdornment: (
                <InputAdornment position="start">
                  <SearchIcon fontSize="small" />
                </InputAdornment>
              ),
            }}
            sx={{ minWidth: { xs: 200, md: 280 } }}
          />
          <Tooltip title="Refrescar">
            <span>
              <IconButton onClick={handleRefresh} disabled={artistsQuery.isFetching}>
                <RefreshIcon />
              </IconButton>
            </span>
          </Tooltip>
          <Button variant="contained" startIcon={<AddIcon />} onClick={handleOpenNew}>
            Nuevo perfil
          </Button>
        </Stack>
      </Stack>

      <Card>
        <CardContent>
          {artistsQuery.isLoading && <Typography>Cargando artistas…</Typography>}
          {artistsQuery.error && (
            <Alert severity="error">
              No pudimos cargar los artistas. Verifica tus permisos de admin.
            </Alert>
          )}
          {!artistsQuery.isLoading && filteredArtists.length === 0 && !artistsQuery.error && (
            <Typography color="text.secondary">Aún no hay perfiles de artista.</Typography>
          )}
          {filteredArtists.length > 0 && (
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
                  {filteredArtists.map((artist) => {
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
                            {(artist.apGenres || artist.apHighlights) && (
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
                            <IconButton size="small" onClick={() => handleEdit(artist)}>
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
        </CardContent>
      </Card>

      <Dialog open={dialogOpen} onClose={handleCloseDialog} maxWidth="md" fullWidth>
        <DialogTitle>{selectedArtist ? 'Editar perfil de artista' : 'Nuevo perfil de artista'}</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2}>
            <Autocomplete
              options={parties}
              loading={partiesQuery.isLoading}
              getOptionLabel={(option) => `${option.displayName} · ID ${option.partyId}${option.primaryEmail ? ` · ${option.primaryEmail}` : ''}`}
              isOptionEqualToValue={(option, value) => option.partyId === value.partyId}
              value={selectedParty}
              onChange={(_, value) =>
                setForm((prev) => ({
                  ...prev,
                  partyId: value?.partyId ?? null,
                  displayName: value?.displayName ?? prev.displayName,
                }))
              }
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Contacto (CRM)"
                  required
                  helperText="Los artistas se enlazan a contactos existentes. Si falta, créalo en CRM → Contactos."
                />
              )}
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
              <TextField
                label="Géneros (coma separada)"
                value={form.genres}
                onChange={(event) => setForm((prev) => ({ ...prev, genres: event.target.value }))}
                fullWidth
              />
              <TextField
                label="Highlights"
                value={form.highlights}
                onChange={(event) => setForm((prev) => ({ ...prev, highlights: event.target.value }))}
                fullWidth
              />
            </Stack>
            {formError && <Alert severity="error">{formError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={handleSubmit}
            disabled={upsertMutation.isPending || !form.partyId}
          >
            {upsertMutation.isPending ? 'Guardando…' : 'Guardar'}
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
