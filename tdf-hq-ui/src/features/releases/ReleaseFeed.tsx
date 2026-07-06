import { useId, useRef, useState, type RefObject } from 'react';
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

const playButtonIcon = <PlayArrowIcon />;
const uploadButtonIcon = <CloudUploadIcon />;

const COPY = {
  header: {
    managerTitle: 'Actividad del hub',
    fanTitle: 'Novedades de tus artistas',
    releaseCount: (count: number) => `${count} lanzamientos`,
    managerSummary: 'Control rápido de enlaces y audio antes de ir al módulo de lanzamientos.',
    fanSummary: 'Escucha novedades y abre videos sin salir del hub.',
  },
  auth: {
    signIn: 'Inicia sesión',
    createFanAccount: 'Crear cuenta fan',
    signedOut: 'Ingresa con tu cuenta para ver lanzamientos personalizados y seguir artistas.',
    renew: 'Reingresar',
    renewSession: 'Necesitamos renovar tu sesión para cargar tu feed personalizado.',
    activatingFan: 'Activando...',
    activateFan: 'Activar Fan',
    noManagerTargets: 'Tu cuenta puede gestionar lanzamientos, pero aún no hay artistas disponibles para cargar este feed.',
    fanRoleNeeded: 'Activa tu rol Fan para recibir lanzamientos personalizados en este hub.',
  },
  empty: {
    followedArtists: 'No hay lanzamientos recientes de los artistas que sigues. Vuelve pronto o revisa los perfiles.',
    noFollows: 'Sigue al menos un artista para ver novedades recientes aquí.',
    manager: 'Todavía no hay lanzamientos visibles en el hub. Crea uno o completa los enlaces a plataformas para que aparezca aquí.',
    label: 'No hay lanzamientos aún. Adjunta enlaces de audio o crea un lanzamiento para verlo aquí.',
    managerNoTargets: 'Todavía no hay lanzamientos o artistas visibles para este hub.',
    noTargets: 'No hay artistas disponibles para mostrar lanzamientos.',
    createRelease: 'Crear lanzamiento',
  },
  release: {
    noAudioLinks: 'Sin enlaces de audio todavía.',
    uploading: 'Subiendo...',
    listen: 'Escuchar',
    attachAudio: 'Adjuntar audio',
    youtube: 'Ver en YouTube',
    driveLabel: 'Avanzado: Subir audio desde Drive',
    driveHelp: 'Opcional: usa Drive si ya tienes el máster listo allí.',
  },
  upload: {
    title: 'Adjuntar audio rápidamente',
    linkLabel: 'Enlace de Spotify o YouTube',
    linkPlaceholder: 'Pega un enlace de audio',
    saving: 'Guardando...',
    save: 'Guardar enlace',
    uploadFile: 'Subir audio',
    cancel: 'Cancelar',
    help: 'Guarda con enlace directo o sube el máster; usaremos el enlace más reciente.',
  },
  feed: {
    showMore: 'Ver más lanzamientos',
    showLess: 'Ver menos',
  },
  focus: {
    ready: 'Feed de lanzamientos listo.',
    fanRole: 'Activando el rol Fan.',
    playing: (title: string) => `Reproduciendo ${title}.`,
    uploadOpen: (title: string) => `Formulario de audio abierto para ${title}.`,
    saveLink: 'Guardando enlace de audio.',
    fileUpload: 'Selector de archivo abierto.',
    uploadCanceled: 'Carga de audio cancelada.',
    showMore: 'Mostrando más lanzamientos.',
    showLess: 'Mostrando menos lanzamientos.',
  },
} as const;

const screenReaderOnlySx = {
  border: 0,
  clip: 'rect(0 0 0 0)',
  height: 1,
  margin: -1,
  overflow: 'hidden',
  padding: 0,
  position: 'absolute',
  whiteSpace: 'nowrap',
  width: 1,
};

type AudioInputRef = RefObject<HTMLInputElement>;
type ReleaseAudioMap = Record<number, string>;
type StreamingFallbackMap = Map<number, ReleaseStreamingFallback>;

export interface ReleaseFeedProps {
  audioFileInputRef: AudioInputRef;
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
  releaseAudioMap: ReleaseAudioMap;
  releaseFeed: ReleaseFeedItem[];
  releaseLinkDraft: string;
  streamingFallbacks: StreamingFallbackMap;
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
}

interface ReleaseActionContext {
  focus: (action: () => void | Promise<void>, message: string) => void;
}

interface ReleaseFeedHeaderProps {
  canSeeReleaseFeed: boolean;
  isHomeManagerView: boolean;
  releaseCount: number;
}

function ReleaseFeedHeader(props: ReleaseFeedHeaderProps) {
  const { canSeeReleaseFeed, isHomeManagerView, releaseCount } = props;
  return (
    <Stack direction="row" justifyContent="space-between" alignItems="flex-start" spacing={2}>
      <Box>
        <Typography variant="h6">
          {isHomeManagerView ? COPY.header.managerTitle : COPY.header.fanTitle}
        </Typography>
        <Typography variant="body2" color="text.secondary">
          {isHomeManagerView ? COPY.header.managerSummary : COPY.header.fanSummary}
        </Typography>
      </Box>
      {canSeeReleaseFeed && <Chip label={COPY.header.releaseCount(releaseCount)} size="small" />}
    </Stack>
  );
}

interface AuthNoticesProps {
  canManageReleases: boolean;
  canSeeReleaseFeed: boolean;
  enableFanRolePending: boolean;
  hasAuthToken: boolean;
  isAuthenticated: boolean;
  isFan: boolean;
  loginPath: string;
  actionContext: ReleaseActionContext;
  onEnableFanRole: () => void;
}

function AuthNotices(props: AuthNoticesProps) {
  const {
    canManageReleases,
    canSeeReleaseFeed,
    enableFanRolePending,
    hasAuthToken,
    isAuthenticated,
    isFan,
    loginPath,
    actionContext,
    onEnableFanRole,
  } = props;

  if (!isAuthenticated) {
    return (
      <Alert
        severity="info"
        action={
          <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
            <Button disabled={false} component={RouterLink} to={loginPath} size="small" variant="contained">
              {COPY.auth.signIn}
            </Button>
            <Button
              disabled={false}
              component={RouterLink}
              to="/login?signup=1&roles=Fan&redirect=/fans"
              size="small"
              variant="outlined"
            >
              {COPY.auth.createFanAccount}
            </Button>
          </Stack>
        }
      >
        {COPY.auth.signedOut}
      </Alert>
    );
  }

  if (!hasAuthToken) {
    return (
      <Alert
        severity="info"
        action={
          <Button disabled={false} component={RouterLink} to={loginPath} size="small" variant="contained">
            {COPY.auth.renew}
          </Button>
        }
      >
        {COPY.auth.renewSession}
      </Alert>
    );
  }

  if (canSeeReleaseFeed) {
    return null;
  }

  const action = !isFan && !canManageReleases ? (
    <Button
      disabled={enableFanRolePending}
      tabIndex={0}
      onClick={() => actionContext.focus(onEnableFanRole, COPY.focus.fanRole)}
      size="small"
      variant="contained"
    >
      {enableFanRolePending ? COPY.auth.activatingFan : COPY.auth.activateFan}
    </Button>
  ) : undefined;

  return (
    <Alert severity="info" action={action}>
      {canManageReleases ? COPY.auth.noManagerTargets : COPY.auth.fanRoleNeeded}
    </Alert>
  );
}

interface ReleaseEmptyStateProps {
  canManageReleases: boolean;
  hasFollows: boolean;
  hasReleaseTargets: boolean;
  isFan: boolean;
  isHomeManagerView: boolean;
}

function ReleaseEmptyState(props: ReleaseEmptyStateProps) {
  const { canManageReleases } = props;
  const emptyMessage = getReleaseEmptyMessage(props);

  return (
    <Alert
      severity="info"
      action={
        canManageReleases ? (
          <Button disabled={false} component={RouterLink} to="/label/releases" size="small" variant="outlined">
            {COPY.empty.createRelease}
          </Button>
        ) : undefined
      }
    >
      {emptyMessage}
    </Alert>
  );
}

function getReleaseEmptyMessage(props: ReleaseEmptyStateProps) {
  const { hasFollows, hasReleaseTargets, isFan, isHomeManagerView } = props;
  if (!hasReleaseTargets) {
    if (isFan) return COPY.empty.noFollows;
    if (isHomeManagerView) return COPY.empty.managerNoTargets;
    return COPY.empty.noTargets;
  }
  if (isFan && hasFollows) return COPY.empty.followedArtists;
  if (isFan) return COPY.empty.noFollows;
  if (isHomeManagerView) return COPY.empty.manager;
  return COPY.empty.label;
}

interface ReleaseListProps {
  audioFileInputRef: AudioInputRef;
  canManageReleases: boolean;
  feedLimit: number;
  pendingUploadRelease: ReleaseFeedItem | null;
  releaseAudioMap: ReleaseAudioMap;
  releaseFeed: ReleaseFeedItem[];
  releaseLinkDraft: string;
  streamingFallbacks: StreamingFallbackMap;
  uploadError: string | null;
  uploadingReleaseId: number | null;
  visibleFeed: ReleaseFeedItem[];
  actionContext: ReleaseActionContext;
  onCancelUpload: () => void;
  onDriveUploadComplete: (release: ReleaseFeedItem, files: DriveFileInfo[]) => void;
  onPlayRelease: (release: ReleaseFeedItem) => void;
  onReleaseLinkDraftChange: (value: string) => void;
  onSaveReleaseLink: () => void | Promise<void>;
  onShowLess: () => void;
  onShowMore: () => void;
  onUploadTrigger: (release: ReleaseFeedItem) => void;
}

function ReleaseList(props: ReleaseListProps) {
  const { feedLimit, releaseFeed, visibleFeed, actionContext, onShowLess, onShowMore } = props;
  return (
    <Stack spacing={1.5}>
      {visibleFeed.map((release) => (
        <ReleaseCard key={`${release.arArtistId}-${release.arReleaseId}`} {...props} release={release} />
      ))}
      {releaseFeed.length > feedLimit && (
        <Button
          disabled={false}
          tabIndex={0}
          onClick={() => actionContext.focus(onShowMore, COPY.focus.showMore)}
          variant="text"
          size="small"
          sx={{ alignSelf: 'flex-start' }}
        >
          {COPY.feed.showMore}
        </Button>
      )}
      {releaseFeed.length > 4 && feedLimit >= releaseFeed.length && (
        <Button
          disabled={false}
          tabIndex={0}
          onClick={() => actionContext.focus(onShowLess, COPY.focus.showLess)}
          variant="text"
          size="small"
          sx={{ alignSelf: 'flex-start' }}
        >
          {COPY.feed.showLess}
        </Button>
      )}
    </Stack>
  );
}

interface ReleaseCardProps extends ReleaseListProps {
  release: ReleaseFeedItem;
}

function ReleaseCard(props: ReleaseCardProps) {
  const {
    canManageReleases,
    pendingUploadRelease,
    release,
    releaseAudioMap,
    streamingFallbacks,
    uploadError,
    uploadingReleaseId,
  } = props;
  const { spotifyUrl, youtubeUrl } = getReleasePlaybackUrls(
    release,
    releaseAudioMap,
    streamingFallbacks.get(release.arArtistId),
  );
  const hasSpotify = Boolean(spotifyUrl);
  const hasYoutube = Boolean(youtubeUrl);
  const hasLinks = hasSpotify || hasYoutube;
  const releaseWithFallback = { ...release, arSpotifyUrl: spotifyUrl, arYoutubeUrl: youtubeUrl };
  const releaseSources = buildReleaseStreamingSources(releaseWithFallback);
  const isUploading = uploadingReleaseId === release.arReleaseId;

  return (
    <Box
      sx={{
        p: 2,
        borderRadius: 2,
        border: '1px solid',
        borderColor: 'divider',
        bgcolor: 'background.paper',
      }}
    >
      <ReleaseSummary release={release} />
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
        <Typography variant="caption" color="text.secondary" sx={{ mt: 1, display: 'block' }}>
          {COPY.release.noAudioLinks}
        </Typography>
      )}
      {(hasLinks || canManageReleases) && (
        <ReleaseActions {...props} hasLinks={hasLinks} hasYoutube={hasYoutube} isUploading={isUploading} youtubeUrl={youtubeUrl} />
      )}
      {canManageReleases && pendingUploadRelease?.arReleaseId === release.arReleaseId && (
        <QuickUploadPanel {...props} isUploading={isUploading} uploadError={uploadError} />
      )}
    </Box>
  );
}

interface ReleaseSummaryProps {
  release: ReleaseFeedItem;
}

function ReleaseSummary(props: ReleaseSummaryProps) {
  const { release } = props;
  return (
    <Stack spacing={0.75}>
      <Stack direction="row" justifyContent="space-between" alignItems="center" spacing={2}>
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
        <Typography variant="body2" color="text.secondary">
          {shortDescription(release.arDescription)}
        </Typography>
      )}
    </Stack>
  );
}

interface ReleaseActionsProps extends ReleaseCardProps {
  hasLinks: boolean;
  hasYoutube: boolean;
  isUploading: boolean;
  youtubeUrl?: string | null;
}

function ReleaseActions(props: ReleaseActionsProps) {
  const {
    actionContext,
    canManageReleases,
    hasLinks,
    hasYoutube,
    isUploading,
    onDriveUploadComplete,
    onPlayRelease,
    onUploadTrigger,
    release,
    uploadingReleaseId,
    youtubeUrl,
  } = props;
  const isBusy = isUploading || uploadingReleaseId !== null;

  return (
    <Stack direction="row" spacing={1} sx={{ mt: 1 }} alignItems="center" useFlexGap flexWrap="wrap">
      {hasLinks && (
        <Button
          disabled={isUploading}
          tabIndex={0}
          onClick={() => actionContext.focus(() => onPlayRelease(release), COPY.focus.playing(release.arTitle))}
          variant="contained"
          size="small"
          startIcon={playButtonIcon}
        >
          {COPY.release.listen}
        </Button>
      )}
      {!hasLinks && canManageReleases && (
        <Button
          disabled={isBusy}
          tabIndex={0}
          onClick={() => actionContext.focus(() => onUploadTrigger(release), COPY.focus.uploadOpen(release.arTitle))}
          variant="contained"
          size="small"
          startIcon={uploadButtonIcon}
        >
          {COPY.release.attachAudio}
        </Button>
      )}
      {hasYoutube && (
        <Button
          disabled={false}
          variant="outlined"
          size="small"
          component="a"
          href={youtubeUrl ?? undefined}
          target="_blank"
          rel="noopener noreferrer"
        >
          {COPY.release.youtube}
        </Button>
      )}
      {canManageReleases && (
        <GoogleDriveUploadWidget
          label={COPY.release.driveLabel}
          helperText={COPY.release.driveHelp}
          accept="audio/*"
          multiple={false}
          dense
          onComplete={(files) => onDriveUploadComplete(release, files)}
        />
      )}
    </Stack>
  );
}

interface QuickUploadPanelProps extends ReleaseCardProps {
  isUploading: boolean;
}

function QuickUploadPanel(props: QuickUploadPanelProps) {
  const {
    actionContext,
    audioFileInputRef,
    isUploading,
    onCancelUpload,
    onReleaseLinkDraftChange,
    onSaveReleaseLink,
    releaseLinkDraft,
    uploadError,
  } = props;
  const canSave = releaseLinkDraft.trim().length > 0 && !isUploading;

  return (
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
          {COPY.upload.title}
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <TextField
            label={COPY.upload.linkLabel}
            placeholder={COPY.upload.linkPlaceholder}
            value={releaseLinkDraft}
            onChange={(event) => onReleaseLinkDraftChange(event.target.value)}
            fullWidth
          />
          <Button
            disabled={!canSave}
            tabIndex={0}
            onClick={() => actionContext.focus(onSaveReleaseLink, COPY.focus.saveLink)}
            variant="contained"
          >
            {isUploading ? COPY.upload.saving : COPY.upload.save}
          </Button>
          <Button
            disabled={isUploading}
            tabIndex={0}
            onClick={() => actionContext.focus(() => audioFileInputRef.current?.click(), COPY.focus.fileUpload)}
            variant="outlined"
            startIcon={uploadButtonIcon}
          >
            {COPY.upload.uploadFile}
          </Button>
          <Button
            disabled={isUploading}
            tabIndex={0}
            onClick={() => actionContext.focus(onCancelUpload, COPY.focus.uploadCanceled)}
            variant="text"
          >
            {COPY.upload.cancel}
          </Button>
        </Stack>
        {isUploading && <LinearProgress />}
        <Typography variant="caption" color="text.secondary">
          {COPY.upload.help}
        </Typography>
        {uploadError && <Alert severity="warning">{uploadError}</Alert>}
      </Stack>
    </Box>
  );
}

function formatReleaseDate(value?: string | null) {
  return formatReleaseDateLabel(value, { month: 'short', day: 'numeric' });
}

function shortDescription(value: string) {
  return value.length > 140 ? `${value.slice(0, 140)}...` : value;
}

export function ReleaseFeed(props: ReleaseFeedProps) {
  const {
    canManageReleases,
    canSeeReleaseFeed,
    enableFanRolePending,
    hasAuthToken,
    hasFollows,
    hasReleaseTargets,
    isAuthenticated,
    isFan,
    isHomeManagerView,
    loading,
    loginPath,
    releaseFeed,
    uploadError,
  } = props;
  const focusStatusId = useId();
  const focusStatusRef = useRef(null as HTMLParagraphElement | null);
  const [focusMessage, setFocusMessage] = useState<string>(COPY.focus.ready);
  const actionContext: ReleaseActionContext = {
    focus: (action, message) => {
      setFocusMessage(message);
      const result = action();
      if (typeof window !== 'undefined') {
        window.setTimeout(() => focusStatusRef.current?.focus(), 0);
      }
      void result;
    },
  };

  return (
    <Card sx={{ p: 3, height: '100%', display: 'flex', flexDirection: 'column', gap: 2 }}>
      <Typography
        id={focusStatusId}
        ref={focusStatusRef}
        tabIndex={-1}
        aria-live="polite"
        sx={screenReaderOnlySx}
      >
        {focusMessage}
      </Typography>
      <ReleaseFeedHeader
        canSeeReleaseFeed={canSeeReleaseFeed}
        isHomeManagerView={isHomeManagerView}
        releaseCount={releaseFeed.length}
      />
      <AuthNotices
        canManageReleases={canManageReleases}
        canSeeReleaseFeed={canSeeReleaseFeed}
        enableFanRolePending={enableFanRolePending}
        hasAuthToken={hasAuthToken}
        isAuthenticated={isAuthenticated}
        isFan={isFan}
        loginPath={loginPath}
        actionContext={actionContext}
        onEnableFanRole={props.onEnableFanRole}
      />
      {canSeeReleaseFeed && loading && (
        <Box display="flex" justifyContent="center" py={3}>
          <CircularProgress size={20} />
        </Box>
      )}
      {canSeeReleaseFeed && !loading && releaseFeed.length === 0 && (
        <ReleaseEmptyState
          canManageReleases={canManageReleases}
          hasFollows={hasFollows}
          hasReleaseTargets={hasReleaseTargets}
          isFan={isFan}
          isHomeManagerView={isHomeManagerView}
        />
      )}
      {uploadError && <Alert severity="warning">{uploadError}</Alert>}
      {canSeeReleaseFeed && releaseFeed.length > 0 && <ReleaseList {...props} actionContext={actionContext} />}
    </Card>
  );
}
