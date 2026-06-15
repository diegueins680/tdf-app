import type { RefObject } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  Chip,
  CircularProgress,
  LinearProgress,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import CloudUploadIcon from '@mui/icons-material/CloudUpload';
import { Link as RouterLink } from 'react-router-dom';
import GoogleDriveUploadWidget from '../../components/GoogleDriveUploadWidget';
import StreamingPlayer from '../../components/StreamingPlayer';
import type { DriveFileInfo } from '../../services/googleDrive';
import { buildReleaseStreamingSources } from '../../utils/media';
import { formatReleaseDateLabel } from '../../utils/releaseDate';
import {
  getReleasePlaybackUrls,
  type ReleaseFeedItem,
  type ReleaseStreamingFallback,
} from './ReleasePlayerActions';

export function ReleaseFeed({
  audioFileInputRef,
  canManageReleases,
  canSeeReleaseFeed,
  enableFanRolePending,
  feedLimit,
  hasAuthToken,
  hasFollows,
  hasReleaseTargets,
  isAuthenticated,
  isFan,
  isHomeManagerView,
  loading,
  loginPath,
  pendingUploadRelease,
  releaseAudioMap,
  releaseFeed,
  releaseLinkDraft,
  streamingFallbacks,
  uploadError,
  uploadingReleaseId,
  visibleFeed,
  onCancelUpload,
  onDriveUploadComplete,
  onEnableFanRole,
  onPlayRelease,
  onReleaseLinkDraftChange,
  onSaveReleaseLink,
  onShowLess,
  onShowMore,
  onUploadTrigger,
}: {
  audioFileInputRef: RefObject<HTMLInputElement>;
  canManageReleases: boolean;
  canSeeReleaseFeed: boolean;
  enableFanRolePending: boolean;
  feedLimit: number;
  hasAuthToken: boolean;
  hasFollows: boolean;
  hasReleaseTargets: boolean;
  isAuthenticated: boolean;
  isFan: boolean;
  isHomeManagerView: boolean;
  loading: boolean;
  loginPath: string;
  pendingUploadRelease: ReleaseFeedItem | null;
  releaseAudioMap: Record<number, string>;
  releaseFeed: ReleaseFeedItem[];
  releaseLinkDraft: string;
  streamingFallbacks: Map<number, ReleaseStreamingFallback>;
  uploadError: string | null;
  uploadingReleaseId: number | null;
  visibleFeed: ReleaseFeedItem[];
  onCancelUpload: () => void;
  onDriveUploadComplete: (release: ReleaseFeedItem, files: DriveFileInfo[]) => void;
  onEnableFanRole: () => void;
  onPlayRelease: (release: ReleaseFeedItem) => void;
  onReleaseLinkDraftChange: (value: string) => void;
  onSaveReleaseLink: () => void | Promise<void>;
  onShowLess: () => void;
  onShowMore: () => void;
  onUploadTrigger: (release: ReleaseFeedItem) => void;
}) {
  const formatReleaseDate = (value?: string | null) => {
    return formatReleaseDateLabel(value, { month: 'short', day: 'numeric' });
  };

  return (
    <Card sx={{ p: 3, height: '100%', display: 'flex', flexDirection: 'column', gap: 2 }}>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography variant="h6">{isHomeManagerView ? 'Actividad del hub' : 'Novedades de tus artistas'}</Typography>
        {canSeeReleaseFeed && <Chip label={`${releaseFeed.length} lanzamientos`} size="small" />}
      </Stack>
      <Typography variant="body2" color="text.secondary">
        {isHomeManagerView
          ? 'Revisa qué lanzamientos ya tienen enlaces válidos y usa este bloque como control rápido antes de ir al módulo de lanzamientos.'
          : 'Reproduce lanzamientos sin salir del hub: si hay enlaces de Spotify o YouTube los cargamos en el reproductor embebido.'}
      </Typography>
      {!isAuthenticated && (
        <Alert
          severity="info"
          action={
            <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
              <Button
                component={RouterLink}
                to={loginPath}
                size="small"
                variant="contained"
                sx={{ textTransform: 'none' }}
              >
                Inicia sesión
              </Button>
              <Button
                component={RouterLink}
                to="/login?signup=1&roles=Fan&redirect=/fans"
                size="small"
                variant="outlined"
                sx={{ textTransform: 'none' }}
              >
                Crear cuenta fan
              </Button>
            </Stack>
          }
        >
          Ingresa con tu cuenta para ver lanzamientos personalizados y seguir artistas.
        </Alert>
      )}
      {isAuthenticated && !hasAuthToken && (
        <Alert
          severity="info"
          action={
            <Button component={RouterLink} to={loginPath} size="small" variant="contained">
              Reingresar
            </Button>
          }
        >
          Necesitamos renovar tu sesión para cargar tu feed personalizado.
        </Alert>
      )}
      {isAuthenticated && hasAuthToken && !canSeeReleaseFeed && (
        <Alert
          severity="info"
          action={
            !isFan && !canManageReleases ? (
              <Button
                size="small"
                variant="contained"
                onClick={onEnableFanRole}
                disabled={enableFanRolePending}
              >
                {enableFanRolePending ? 'Activando…' : 'Activar Fan'}
              </Button>
            ) : undefined
          }
        >
          {canManageReleases
            ? 'Tu cuenta puede gestionar lanzamientos, pero aún no hay artistas disponibles para cargar este feed.'
            : 'Activa tu rol Fan para recibir lanzamientos personalizados en este hub.'}
        </Alert>
      )}
      {canSeeReleaseFeed && loading && (
        <Box display="flex" justifyContent="center" py={3}>
          <CircularProgress size={20} />
        </Box>
      )}
      {canSeeReleaseFeed && !loading && releaseFeed.length === 0 && (
        <Alert severity="info" sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
          <Box sx={{ flexGrow: 1 }}>
            {hasReleaseTargets
              ? isFan
                ? hasFollows
                  ? 'No hay lanzamientos recientes de los artistas que sigues. Vuelve pronto o revisa los perfiles.'
                  : 'Sigue al menos un artista para ver novedades recientes aquí.'
                : isHomeManagerView
                  ? 'Todavía no hay lanzamientos visibles en el hub. Crea uno o completa los enlaces a plataformas para que aparezca aquí.'
                  : 'No hay lanzamientos aún. Adjunta enlaces de audio o crea un lanzamiento para verlo aquí.'
              : isFan
                ? 'Sigue al menos un artista para ver novedades recientes aquí.'
                : isHomeManagerView
                  ? 'Todavía no hay lanzamientos o artistas visibles para este hub.'
                  : 'No hay artistas disponibles para mostrar lanzamientos.'}
          </Box>
          {canManageReleases && (
            <Button
              component={RouterLink}
              to="/label/releases"
              size="small"
              variant="outlined"
            >
              Crear lanzamiento
            </Button>
          )}
        </Alert>
      )}
      {uploadError && (
        <Alert severity="warning">{uploadError}</Alert>
      )}
      {canSeeReleaseFeed && releaseFeed.length > 0 && (
        <Stack spacing={1.5}>
          {visibleFeed.map((release) => {
            const { spotifyUrl, youtubeUrl } = getReleasePlaybackUrls(
              release,
              releaseAudioMap,
              streamingFallbacks.get(release.arArtistId),
            );
            const hasSpotify = Boolean(spotifyUrl);
            const hasYoutube = Boolean(youtubeUrl);
            const hasLinks = hasSpotify || hasYoutube;
            const releaseWithFallback = {
              ...release,
              arSpotifyUrl: spotifyUrl,
              arYoutubeUrl: youtubeUrl,
            };
            const releaseSources = buildReleaseStreamingSources(releaseWithFallback);
            const hasStream = releaseSources.length > 0;
            return (
              <Box
                key={`${release.arArtistId}-${release.arReleaseId}`}
                sx={{
                  p: 2,
                  borderRadius: 2,
                  border: '1px solid',
                  borderColor: 'divider',
                  bgcolor: 'background.paper',
                }}
              >
                <Stack direction="row" justifyContent="space-between" alignItems="center">
                  <Box>
                    <Typography variant="subtitle1" fontWeight={700}>
                      {release.arTitle}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {release.artistName}
                    </Typography>
                  </Box>
                  <Chip label={formatReleaseDate(release.arReleaseDate)} size="small" />
                </Stack>
                {release.arDescription && (
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                    {release.arDescription.length > 140
                      ? `${release.arDescription.slice(0, 140)}…`
                      : release.arDescription}
                  </Typography>
                )}
                {releaseSources.length > 0 && (
                  <Box sx={{ mt: 1.5 }}>
                    <StreamingPlayer
                      title={release.arTitle}
                      artist={release.artistName}
                      posterUrl={release.arCoverImageUrl}
                      sources={releaseSources}
                      variant="compact"
                    />
                  </Box>
                )}
                {!hasLinks && !canManageReleases && (
                  <Typography variant="caption" color="text.secondary" sx={{ mt: 1 }}>
                    Sin enlaces de audio todavía.
                  </Typography>
                )}
                {(hasLinks || canManageReleases) && (
                  <Stack direction="row" spacing={1} sx={{ mt: 1 }} alignItems="center">
                    {hasLinks && (
                      <Button
                        variant="contained"
                        size="small"
                        startIcon={<PlayArrowIcon />}
                        onClick={() => onPlayRelease(release)}
                        disabled={!hasStream || uploadingReleaseId === release.arReleaseId}
                      >
                        {uploadingReleaseId === release.arReleaseId ? 'Subiendo…' : 'Escuchar'}
                      </Button>
                    )}
                    {!hasLinks && canManageReleases && (
                      <Button
                        variant="contained"
                        size="small"
                        startIcon={<CloudUploadIcon />}
                        onClick={() => onUploadTrigger(release)}
                      >
                        Adjuntar audio
                      </Button>
                    )}
                    {hasYoutube && (
                      <Button
                        variant="outlined"
                        size="small"
                        component="a"
                        href={youtubeUrl ?? undefined}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        Ver en YouTube
                      </Button>
                    )}
                    {canManageReleases && (
                      <GoogleDriveUploadWidget
                        label="Avanzado: Subir audio desde Drive"
                        helperText="Opcional: usa Drive si ya tienes el máster listo allí."
                        accept="audio/*"
                        multiple={false}
                        dense
                        onComplete={(files) => onDriveUploadComplete(release, files)}
                      />
                    )}
                  </Stack>
                )}
                {canManageReleases && pendingUploadRelease?.arReleaseId === release.arReleaseId && (
                  <Box
                    sx={{
                      mt: 2,
                      p: 2,
                      borderRadius: 2,
                      border: '1px dashed',
                      borderColor: 'divider',
                      bgcolor: 'background.default',
                    }}
                  >
                    <Stack spacing={1.5}>
                      <Typography variant="subtitle2" fontWeight={700}>
                        Adjuntar audio rápidamente
                      </Typography>
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                        <TextField
                          label="Enlace de Spotify o YouTube"
                          placeholder="Pega un enlace de audio"
                          value={releaseLinkDraft}
                          onChange={(event) => onReleaseLinkDraftChange(event.target.value)}
                          fullWidth
                        />
                        <Button
                          variant="contained"
                          onClick={() => void onSaveReleaseLink()}
                          disabled={uploadingReleaseId === release.arReleaseId}
                        >
                          Guardar enlace
                        </Button>
                        <Button
                          variant="outlined"
                          startIcon={<CloudUploadIcon />}
                          onClick={() => audioFileInputRef.current?.click()}
                          disabled={uploadingReleaseId === release.arReleaseId}
                        >
                          Subir audio
                        </Button>
                        <Button
                          variant="text"
                          onClick={onCancelUpload}
                        >
                          Cancelar
                        </Button>
                      </Stack>
                      {uploadingReleaseId === release.arReleaseId && <LinearProgress />}
                      <Typography variant="caption" color="text.secondary">
                        Guarda con enlace directo o sube el máster; usaremos el enlace más reciente.
                      </Typography>
                      {uploadError && <Alert severity="warning">{uploadError}</Alert>}
                    </Stack>
                  </Box>
                )}
              </Box>
            );
          })}
          {releaseFeed.length > feedLimit && (
            <Button
              variant="text"
              size="small"
              onClick={onShowMore}
              sx={{ alignSelf: 'flex-start' }}
            >
              Ver más lanzamientos
            </Button>
          )}
          {releaseFeed.length > 4 && feedLimit >= releaseFeed.length && (
            <Button
              variant="text"
              size="small"
              onClick={onShowLess}
              sx={{ alignSelf: 'flex-start' }}
            >
              Ver menos
            </Button>
          )}
        </Stack>
      )}
    </Card>
  );
}
