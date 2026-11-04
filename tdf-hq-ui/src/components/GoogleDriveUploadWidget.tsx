import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { useTranslation } from 'react-i18next';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useOptionalSession } from '../session/SessionContext';
import { getAuthSessionEpoch } from '../session/authEvents';
import { newUploadKey, normalizeUploadFailure, uploadFailureMessage } from '../api/uploadFailure';
import {
  Alert,
  Box,
  Button,
  IconButton,
  LinearProgress,
  MenuItem,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Stack,
  Typography,
} from '@mui/material';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import DeleteIcon from '@mui/icons-material/Delete';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import type { DriveFileInfo } from '../services/googleDrive';
import { uploadToDrive } from '../api/drive';
import { uploadAssetPhoto } from '../api/inventory';
import { availableStorageBytes, deleteStorageFile, downloadStorageFile, loadStorageFiles, loadStorageAccount, saveStorageDestination, type StorageAccount, type StorageDestination, type StorageFile } from '../api/contentStorage';
import { useGoogleDriveAuth } from '../hooks/useGoogleDriveAuth';
import LazyPaginatedList from './LazyPaginatedList';

interface UploadItem {
  id: string;
  file: File;
  idempotencyKey: string;
  destination: StorageDestination;
  visibility: 'private' | 'public';
  progress: number;
  status: 'pending' | 'uploading' | 'done' | 'error';
  error?: string;
  driveFile?: DriveFileInfo;
}

export interface GoogleDriveUploadWidgetProps {
  label?: string;
  helperText?: string;
  onComplete?: (files: DriveFileInfo[]) => void;
  accept?: string;
  multiple?: boolean;
  dense?: boolean;
  authMode?: 'user' | 'server' | 'assets';
  visibility?: 'private' | 'public';
}

interface UploadPreviewProps {
  item: UploadItem;
}

function UploadPreview({ item }: UploadPreviewProps) {
  const previewUrl = useMemo(() => item.file.type.startsWith('image/') ? URL.createObjectURL(item.file) : undefined, [item.file]);
  useEffect(() => () => { if (previewUrl) URL.revokeObjectURL(previewUrl); }, [previewUrl]);
  return (
    <Box sx={{ width: 48, height: 48, borderRadius: 1, overflow: 'hidden', bgcolor: 'grey.100' }}>
      {item.file.type.startsWith('image/') ? (
        <img
          src={previewUrl}
          alt={item.file.name}
          style={{ width: '100%', height: '100%', objectFit: 'cover' }}
        />
      ) : (
        <Box
          sx={{
            width: '100%',
            height: '100%',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            bgcolor: 'grey.200',
            color: 'text.secondary',
            fontSize: 12,
          }}
        >
          File
        </Box>
      )}
    </Box>
  );
}

interface UploadItemRowProps {
  item: UploadItem;
  onRemove: (id: string) => void;
  onRetry: (item: UploadItem) => void;
  english: boolean;
}

function UploadItemRow({ item, onRemove, onRetry, english }: UploadItemRowProps) {
  const driveUrl = item.driveFile?.publicUrl ?? item.driveFile?.webContentLink ?? item.driveFile?.webViewLink;

  return (
    <Box
      sx={{
        border: '1px solid',
        borderColor: 'divider',
        borderRadius: 1.5,
        p: 1,
        display: 'flex',
        alignItems: 'center',
        gap: 1,
      }}
    >
      <UploadPreview item={item} />
      <Stack spacing={0.25} flex={1} minWidth={0}>
        <Typography variant="body2" noWrap>
          {item.file.name}
        </Typography>
        <Typography variant="caption" color="text.secondary">
          {(item.file.size / 1024 / 1024).toFixed(2)} MB
        </Typography>
        <LinearProgress
          aria-label={`${english ? 'Uploading' : 'Subiendo'} ${item.file.name}`}
          variant="determinate"
          value={item.status === 'error' ? 0 : item.progress}
          color={item.status === 'error' ? 'error' : 'primary'}
        />
        {item.status === 'done' && driveUrl && (
          <Typography variant="caption" color="text.secondary">
            {driveUrl}
          </Typography>
        )}
        {item.status === 'error' && (
          <Typography variant="caption" color="error">
            {item.error ?? (english ? 'Upload failed' : 'Error al subir')}
          </Typography>
        )}
      </Stack>
      {item.status === 'done' && <CheckCircleIcon color="success" fontSize="small" />}
      {item.status === 'error' && (
        <IconButton aria-label={`${english ? 'Retry' : 'Reintentar'} ${item.file.name}`} onClick={() => onRetry(item)}><RefreshIcon /></IconButton>
      )}
      {item.status !== 'done' && (
        <IconButton
          size="small"
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onRemove(item.id);
          }}
          aria-label={`Quitar ${item.file.name} de la lista de cargas`}
        >
          <DeleteIcon fontSize="small" />
        </IconButton>
      )}
    </Box>
  );
}

export default function GoogleDriveUploadWidget({
  label = 'Subir fotos',
  helperText = 'Elige dónde guardar y selecciona los archivos.',
  onComplete,
  accept = 'image/*',
  multiple = true,
  dense = false,
  authMode,
  visibility = 'private',
}: GoogleDriveUploadWidgetProps) {
  const ownerPartyId = useOptionalSession()?.session?.partyId;
  const { i18n } = useTranslation();
  const english = i18n.language?.startsWith('en') ?? false;
  const inputRef = useRef<HTMLInputElement | null>(null);
  const controllers = useRef(new Map<string, AbortController>());
  const cancelled = useRef(new Set<string>());
  const accountReadVersion = useRef(0);
  useEffect(() => {
    const active = controllers.current;
    return () => { active.forEach((controller) => controller.abort()); active.clear(); };
  }, []);
  const [items, setItems] = useState<UploadItem[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [isDragging, setIsDragging] = useState(false);
  const [account, setAccount] = useState<StorageAccount | null>(null);
  const [destination, setDestination] = useState<StorageDestination | ''>('');
  const [accountError, setAccountError] = useState(false);
  const [managerOpen, setManagerOpen] = useState(false);
  const [managedFiles, setManagedFiles] = useState<StorageFile[]>([]);
  const [managerBusy, setManagerBusy] = useState(false);
  const [managerError, setManagerError] = useState<string | null>(null);
  const [hasMoreFiles, setHasMoreFiles] = useState(false);
  const readManagedFiles = async (append = false) => {
    const epoch = getAuthSessionEpoch();
    setManagerBusy(true); setManagerError(null);
    try {
      const files = await loadStorageFiles(append ? managedFiles.at(-1)?.fileId : undefined);
      if (epoch !== getAuthSessionEpoch()) return;
      setManagedFiles((previous) => append ? [...previous, ...files] : files); setHasMoreFiles(files.length === 20);
    } catch { setManagerError(english ? 'Could not load your files. Retry shortly.' : 'No pudimos consultar tus archivos. Reintenta en un momento.'); }
    finally { setManagerBusy(false); }
  };
  useEffect(() => {
    controllers.current.forEach((controller) => controller.abort());
    controllers.current.clear();
    setItems([]); setAccount(null); setDestination(''); setAccountError(false);
    setManagerOpen(false); setManagedFiles([]); setManagerError(null);
  }, [ownerPartyId]);
  const explicitDestination = authMode === 'user' ? 'google_drive' : authMode ? 'tdf' : destination;
  const effectiveMode = explicitDestination === 'google_drive' ? 'user' : 'server';
  const usesUserAuth = effectiveMode === 'user';
  const needsUserAuth = usesUserAuth || items.some((item) => item.destination === 'google_drive');
  useEffect(() => {
    if (authMode) return;
    let current = true;
    void loadStorageAccount().then((value) => { if (current) { setAccount(value); setDestination(value.defaultDestination ?? ''); } })
      .catch(() => { if (current) setAccountError(true); });
    return () => { current = false; };
  }, [authMode, ownerPartyId]);
  const { status: driveStatus, error: driveError, startAuth, ensureToken } = useGoogleDriveAuth({
    enabled: needsUserAuth,
  });

  const handleDriveAuth = useCallback(() => {
    if (!needsUserAuth) return;
    void startAuth(undefined, { popup: true }).catch(() => undefined);
  }, [startAuth, needsUserAuth]);

  const runUploads = useCallback(async (uploads: UploadItem[]) => {
    const selectedDestination = uploads[0]?.destination;
    if (!selectedDestination) return;
    const personal = selectedDestination === 'google_drive';
    const requestEpoch = getAuthSessionEpoch();
    if (account && selectedDestination === 'tdf' && uploads.reduce((size, item) => size + item.file.size, 0) > availableStorageBytes(account)) {
      const message = english ? 'Not enough TDF storage. Choose your own storage or free up space.' : 'No hay espacio suficiente en TDF. Elige tu propio almacenamiento o libera espacio.';
      setError(message);
      setItems((prev) => prev.map((item) => uploads.some((upload) => upload.id === item.id) ? { ...item, status: 'error', error: message } : item));
      return;
    }
    let accessToken: string | undefined;
    try {
      if (personal) accessToken = (await ensureToken()).accessToken;
      if (requestEpoch !== getAuthSessionEpoch()) return;
    } catch (error) {
      const message = uploadFailureMessage(normalizeUploadFailure(error, 'user'), english);
      setItems((prev) => prev.map((item) => uploads.some((upload) => upload.id === item.id)
        ? { ...item, status: 'error', error: message } : item));
      return;
    }
    // At most three uploads at once; retries are explicit and retain the same key.
    let next = 0;
    const worker = async () => {
      while (next < uploads.length) {
        if (requestEpoch !== getAuthSessionEpoch()) return;
        const item = uploads[next++];
        if (!item || controllers.current.has(item.id)) continue;
        if (cancelled.current.has(item.id)) { cancelled.current.delete(item.id); continue; }
        const controller = new AbortController();
        controllers.current.set(item.id, controller);
        setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, status: 'uploading', progress: 0, error: undefined } : entry));
        try {
          const onProgress = (progress: number) => setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, progress: Math.min(99, Math.max(0, progress)) } : entry));
          const driveFile = authMode === 'assets' ? await uploadAssetPhoto(item.file, { name: item.file.name, onProgress }) : await uploadToDrive(item.file, {
              accessToken, onProgress, authMode: personal ? 'user' : 'server', destination: item.destination, visibility: item.visibility,
              idempotencyKey: item.idempotencyKey, signal: controller.signal,
            });
          if (controller.signal.aborted || requestEpoch !== getAuthSessionEpoch()) continue;
          setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, status: 'done', progress: 100, driveFile } : entry));
          if (selectedDestination === 'tdf') {
            const readVersion = ++accountReadVersion.current;
            void loadStorageAccount().then((current) => {
              if (requestEpoch === getAuthSessionEpoch() && readVersion === accountReadVersion.current) setAccount(current);
            }).catch(() => undefined);
          }
          onComplete?.([driveFile]);
        } catch (error) {
          if (!controller.signal.aborted && requestEpoch === getAuthSessionEpoch()) {
            const message = uploadFailureMessage(normalizeUploadFailure(error, personal ? 'user' : 'server'), english);
            setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, status: 'error', error: message } : entry));
          }
        } finally {
          if (controllers.current.get(item.id) === controller) controllers.current.delete(item.id);
        }
      }
    };
    await Promise.all(Array.from({ length: Math.min(3, uploads.length) }, worker));
  }, [account, authMode, ensureToken, english, onComplete]);

  const handleFiles = useCallback((files: FileList | File[]) => {
    if (!explicitDestination) return;
    const uploads: UploadItem[] = Array.from(files).map((file) => ({
      id: crypto.randomUUID(), idempotencyKey: newUploadKey(), destination: explicitDestination, visibility: authMode === 'assets' ? 'public' : visibility, file, progress: 0, status: 'pending',
    }));
    setError(null);
    setItems((prev) => [...prev, ...uploads]);
    void runUploads(uploads);
  }, [authMode, explicitDestination, runUploads, visibility]);

  const handleDrop = (evt: React.DragEvent) => {
    evt.preventDefault();
    setIsDragging(false);
    if (evt.dataTransfer.files && evt.dataTransfer.files.length > 0) {
      void handleFiles(evt.dataTransfer.files);
      evt.dataTransfer.clearData();
    }
  };

  const handleBrowse = () => { if (!explicitDestination) { setError(english ? 'Choose where to store the file first.' : 'Primero elige dónde guardar el archivo.'); return; } inputRef.current?.click(); };
  const removeItem = (id: string) => { cancelled.current.add(id); controllers.current.get(id)?.abort(); setItems((prev) => prev.filter((it) => it.id !== id)); };

  return (
    <Stack spacing={1}>
      {!authMode && <>
        <TextField select label={english ? 'Storage destination' : 'Dónde guardar'} value={destination} size="small"
          onChange={(event) => setDestination(event.target.value as StorageDestination)}
          helperText={english ? 'Changing the destination does not move existing files.' : 'Cambiar el destino no mueve tus archivos anteriores.'}>
          <MenuItem value="tdf">{english ? 'TDF · up to 5 GB free' : 'TDF · hasta 5 GB gratis'}</MenuItem>
          <MenuItem value="google_drive">{english ? 'My Google Drive · no TDF quota used' : 'Mi Google Drive · no consume espacio TDF'}</MenuItem>
        </TextField>
        {account && !account.legacyUsageVerified && destination === 'tdf' && <Alert severity="info">{english ? 'TDF must verify the space used by older files before activating uploads for this account.' : 'TDF debe verificar el espacio de tus archivos anteriores antes de activar las cargas de esta cuenta.'}</Alert>}
        {account && destination === 'tdf' && <Typography variant="caption">
          {(account.usedBytes / 1e9).toFixed(2)} GB {english ? 'used in registered files' : 'usados en archivos registrados'} · {(account.reservedBytes / 1e9).toFixed(2)} GB {english ? 'reserved' : 'reservados'} · {(availableStorageBytes(account) / 1e9).toFixed(2)} GB {english ? 'available' : 'disponibles'}
        </Typography>}
        {account && destination && destination !== account.defaultDestination && <Button size="small" onClick={() => {
          void saveStorageDestination(destination, account.revision).then(setAccount).catch(() => setError(english ? 'Could not save your default destination. Reload its current value before retrying.' : 'No pudimos guardar el destino predeterminado. Recarga su valor actual antes de reintentar.'));
        }}>{english ? 'Use as my default' : 'Usar como destino predeterminado'}</Button>}
        {accountError && <Alert severity="warning">{english ? 'Could not check your storage. Retry when the service is available.' : 'No pudimos consultar tu almacenamiento. Reintenta cuando el servicio esté disponible.'}</Alert>}
      </>}
      {authMode !== 'assets' && <Button size="small" onClick={() => { setManagerOpen(true); void readManagedFiles(); }}>{english ? 'Manage TDF storage' : 'Administrar almacenamiento TDF'}</Button>}
      <Dialog open={managerOpen} onClose={() => setManagerOpen(false)} fullWidth maxWidth="sm">
        <DialogTitle>{english ? 'Your TDF files' : 'Tus archivos en TDF'}</DialogTitle>
        <DialogContent>
          {managerError && <Alert severity="warning">{managerError}</Alert>}
          {!managerBusy && managedFiles.length === 0 && <Typography>{english ? 'There are no registered files.' : 'No hay archivos registrados.'}</Typography>}
          <Stack spacing={1}>
            {managedFiles.map((file) => <Stack key={file.fileId} spacing={0.5} sx={{ borderBottom: '1px solid', borderColor: 'divider', py: 1 }}>
              <Typography sx={{ overflowWrap: 'anywhere' }}>{file.fileName}</Typography>
              <Typography variant="caption">{(file.byteSize / 1e6).toFixed(2)} MB · {file.status === 'completed' ? (english ? 'Uploaded' : 'Cargado') : (english ? 'Recovery pending' : 'Recuperación pendiente')}</Typography>
              <Stack direction="row" spacing={1}>
                {file.status === 'completed' && <Button disabled={managerBusy} onClick={() => {
                  void downloadStorageFile(file).catch(() => setManagerError(english ? 'Could not download this file.' : 'No pudimos descargar este archivo.'));
                }}>{english ? 'Download' : 'Descargar'}</Button>}
                <Button color="error" disabled={managerBusy} onClick={() => {
                  if (!window.confirm(english ? 'Delete this file? Its links will stop working.' : '¿Borrar este archivo? Sus enlaces dejarán de funcionar.')) return;
                  const epoch = getAuthSessionEpoch(); setManagerBusy(true);
                  void deleteStorageFile(file.fileId).then(async () => {
                    if (epoch !== getAuthSessionEpoch()) return;
                    const current = await loadStorageAccount();
                    if (epoch !== getAuthSessionEpoch()) return;
                    setAccount(current); await readManagedFiles();
                  }).catch(() => setManagerError(english ? 'Deletion is not confirmed. If the upload is still running, wait before retrying.' : 'No pudimos confirmar el borrado. Si la carga sigue en proceso, espera antes de reintentar.'))
                    .finally(() => setManagerBusy(false));
                }}>{english ? 'Delete' : 'Borrar'}</Button>
              </Stack>
            </Stack>)}
          </Stack>
          {hasMoreFiles && <Button disabled={managerBusy} onClick={() => void readManagedFiles(true)}>{english ? 'More files' : 'Más archivos'}</Button>}
        </DialogContent>
        <DialogActions><Button onClick={() => setManagerOpen(false)}>{english ? 'Close' : 'Cerrar'}</Button></DialogActions>
      </Dialog>
      <Typography variant="caption">{visibility === 'public' || authMode === 'assets' ? (english ? 'These images will be accessible by link.' : 'Estas imágenes serán accesibles mediante su enlace.') : (english ? 'TDF files remain private.' : 'Los archivos guardados en TDF conservan acceso privado.')}</Typography>
      <Stack direction="row" alignItems="center" spacing={1} sx={{ flexWrap: 'wrap' }}>
        <Button
          variant="outlined"
          disabled={!explicitDestination}
          startIcon={<UploadFileIcon />}
          tabIndex={0}
          onClick={(event) => {
            handleBrowse();
            event.currentTarget.focus();
          }}
          size={dense ? 'small' : 'medium'}
        >
          {label}
        </Button>
        {needsUserAuth && (
          <Button
            variant="text"
            tabIndex={0}
            onClick={(event) => {
              handleDriveAuth();
              event.currentTarget.focus();
            }}
            size={dense ? 'small' : 'medium'}
            disabled={driveStatus === 'authenticating'}
          >
            {english ? (driveStatus === 'ready' ? 'Reconnect Drive' : 'Connect Drive') : (driveStatus === 'ready' ? 'Reautorizar Drive' : 'Conectar Drive')}
          </Button>
        )}
        {!dense && (
          <Typography variant="body2" color="text.secondary">
            {helperText}
          </Typography>
        )}
      </Stack>

      <Box
        onDragOver={(e) => {
          e.preventDefault();
          setIsDragging(true);
        }}
        onDragLeave={(e) => {
          e.preventDefault();
          setIsDragging(false);
        }}
        onDrop={handleDrop}
        role="button"
        aria-disabled={!explicitDestination}
        tabIndex={0}
        onClick={(event) => {
          handleBrowse();
          event.currentTarget.focus();
        }}
        onKeyDown={(event) => {
          if (event.key === 'Enter' || event.key === ' ') {
            event.preventDefault();
            handleBrowse();
          }
        }}
        sx={{
          border: '1px dashed',
          borderColor: isDragging ? 'primary.main' : 'divider',
          borderRadius: 2,
          p: dense ? 2 : 3,
          bgcolor: isDragging ? 'action.hover' : 'background.paper',
          cursor: 'pointer',
          transition: 'all 0.15s ease',
        }}
      >
        <Typography variant="body2" color="text.secondary">
          {english ? 'Drop files here or click to select them' : 'Arrastra archivos aquí o haz clic para seleccionarlos'}
        </Typography>
        <input
          ref={inputRef}
          type="file"
          accept={accept}
          multiple={multiple}
          hidden
          onChange={(e) => {
            if (e.target.files) {
              void handleFiles(e.target.files);
              e.target.value = '';
            }
          }}
        />
      </Box>

      {error && <Alert severity="error">{error}</Alert>}
      {usesUserAuth && driveError && <Alert severity="warning">{uploadFailureMessage(normalizeUploadFailure(new Error(driveError), 'user'), english)}</Alert>}

      {items.length > 0 && (
        <LazyPaginatedList
          items={items}
          pagination={{ itemLabel: 'archivos', initialRowsPerPage: 5 }}
          renderItems={(visibleItems) => (
            <Stack spacing={1}>
              {visibleItems.map((item) => (
                <UploadItemRow key={item.id} item={item} onRemove={removeItem} onRetry={(entry) => void runUploads([entry])} english={english} />
              ))}
            </Stack>
          )}
        />
      )}
    </Stack>
  );
}
