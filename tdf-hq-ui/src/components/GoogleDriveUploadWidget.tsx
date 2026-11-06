import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { useTranslation } from 'react-i18next';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useOptionalSession } from '../session/SessionContext';
import { useSessionOwner } from '../session/useSessionOwner';
import { useOwnedTabDraft } from '../session/useOwnedTabDraft';
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
import { sharedFileUploadQueue } from '../api/uploadQueue';
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
  attempted?: boolean;
}

interface RecoverableUpload {
  id: string;
  idempotencyKey: string;
  fileName: string;
  byteSize: number;
  mediaType: string;
  destination: StorageDestination;
  visibility: 'private' | 'public';
}

function parseRecoverableUploads(value: unknown): RecoverableUpload[] | undefined {
  if (!Array.isArray(value) || value.length > 100) return undefined;
  const result: RecoverableUpload[] = [];
  for (const entry of value as unknown[]) {
    if (!entry || typeof entry !== 'object') return undefined;
    const data = entry as Record<string, unknown>;
    const id = data['id'], key = data['idempotencyKey'], name = data['fileName'], bytes = data['byteSize'];
    const mime = data['mediaType'], destination = data['destination'], visibility = data['visibility'];
    if (typeof id !== 'string' || !/^[a-f0-9-]{36}$/i.test(id) || typeof key !== 'string' || !/^[a-f0-9]{64}$/.test(key)
      || typeof name !== 'string' || name.length > 1024 || typeof bytes !== 'number' || !Number.isSafeInteger(bytes) || bytes <= 0
      || typeof mime !== 'string' || mime.length > 256 || (destination !== 'tdf' && destination !== 'google_drive')
      || (visibility !== 'public' && visibility !== 'private')) return undefined;
    result.push({ id, idempotencyKey: key, fileName: name, byteSize: bytes, mediaType: mime, destination, visibility });
  }
  return new Set(result.map((entry) => entry.id)).size === result.length ? result : undefined;
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
  /** Stable editor/resource identity, independent of its label or selected file. */
  recoveryScope?: string;
  formRecoveryAvailable?: boolean;
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

export default function GoogleDriveUploadWidget(props: GoogleDriveUploadWidgetProps) {
  const auth = useOptionalSession();
  const owner = auth?.loading ? 'pending' : auth?.session?.partyId ?? 'anonymous';
  const scope = props.recoveryScope ?? window.location.pathname;
  return <ScopedDriveUploadWidget key={`${owner}:${scope}`} {...props} />;
}

function ScopedDriveUploadWidget({
  label = 'Subir fotos',
  helperText = 'Elige dónde guardar y selecciona los archivos.',
  onComplete,
  accept = 'image/*',
  multiple = true,
  dense = false,
  authMode,
  visibility = 'private',
  recoveryScope,
  formRecoveryAvailable = true,
}: GoogleDriveUploadWidgetProps) {
  const ownerSession = useOptionalSession();
  const ownerPartyId = ownerSession?.loading ? undefined : ownerSession?.session?.partyId;
  const scope = recoveryScope ?? window.location.pathname;
  const { isCurrentOwner: ownsUpload } = useSessionOwner(ownerPartyId ?? null, scope);
  const recovery = useOwnedTabDraft<RecoverableUpload[]>([], {
    ownerId: authMode === 'assets' ? null : ownerPartyId ?? null, scope: `upload:${scope}`, parse: parseRecoverableUploads,
  });
  const { value: recoverableUploads, setValue: setRecoverableUploads } = recovery;
  const { i18n } = useTranslation();
  const english = i18n.language?.startsWith('en') ?? false;
  const inputRef = useRef<HTMLInputElement | null>(null);
  const recoveryInputRef = useRef<HTMLInputElement | null>(null);
  const reselecting = useRef<RecoverableUpload | null>(null);
  const controllers = useRef(new Map<string, AbortController>());
  const cancelled = useRef(new Set<string>());
  const previousEpoch = useRef(getAuthSessionEpoch());
  const currentEpoch = getAuthSessionEpoch();
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
  useEffect(() => {
    if (previousEpoch.current === currentEpoch) return;
    previousEpoch.current = currentEpoch;
    controllers.current.forEach((controller) => controller.abort()); controllers.current.clear();
    setItems((previous) => previous.map((item) => item.status === 'uploading' || item.status === 'pending'
      ? { ...item, status: 'error', error: english ? 'Your TDF session was updated. Retry the same file.' : 'Tu sesión TDF se actualizó. Reintenta el mismo archivo.' } : item));
  }, [currentEpoch, english]);
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
    reselecting.current = null;
  }, [ownerPartyId, scope]);
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
  const { status: driveStatus, error: driveError, startAuth, ensureToken, resetAuth } = useGoogleDriveAuth({
    enabled: needsUserAuth,
  });

  const handleDriveAuth = useCallback(() => {
    if (!needsUserAuth) return;
    void startAuth(undefined, { popup: true }).catch(() => undefined);
  }, [startAuth, needsUserAuth]);

  const runUploads = useCallback(async (uploads: UploadItem[]) => {
    if (!ownsUpload()) return;
    const selectedDestination = uploads[0]?.destination;
    if (!selectedDestination) return;
    const personal = selectedDestination === 'google_drive';
    const requestEpoch = getAuthSessionEpoch();
    if (account && selectedDestination === 'tdf' && uploads.filter((item) => !item.attempted).reduce((size, item) => size + item.file.size, 0) > availableStorageBytes(account)) {
      const message = english ? 'Not enough TDF storage. Choose your own storage or free up space.' : 'No hay espacio suficiente en TDF. Elige tu propio almacenamiento o libera espacio.';
      setError(message);
      setItems((prev) => prev.map((item) => uploads.some((upload) => upload.id === item.id) ? { ...item, status: 'error', error: message } : item));
      return;
    }
    let accessToken: string | undefined;
    try {
      if (personal) accessToken = (await ensureToken()).accessToken;
      if (requestEpoch !== getAuthSessionEpoch() || !ownsUpload()) return;
    } catch (error) {
      if (requestEpoch !== getAuthSessionEpoch() || !ownsUpload()) return;
      const message = uploadFailureMessage(normalizeUploadFailure(error, 'user'), english);
      setItems((prev) => prev.map((item) => uploads.some((upload) => upload.id === item.id)
        ? { ...item, status: 'error', error: message } : item));
      return;
    }
    // At most three uploads at once; retries are explicit and retain the same key.
    let next = 0;
    const worker = async () => {
      while (next < uploads.length) {
        if (requestEpoch !== getAuthSessionEpoch() || !ownsUpload()) return;
        const item = uploads[next++];
        if (!item || controllers.current.has(item.id)) continue;
        if (cancelled.current.has(item.id)) { cancelled.current.delete(item.id); continue; }
        const controller = new AbortController();
        controllers.current.set(item.id, controller);
        setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, status: 'pending', progress: 0, error: undefined } : entry));
        try {
          const onProgress = (progress: number) => setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, progress: Math.min(99, Math.max(0, progress)) } : entry));
          const driveFile = await sharedFileUploadQueue.run(async () => {
            if (!ownsUpload()) throw new DOMException('Cancelled', 'AbortError');
            setItems((previous) => previous.map((entry) => entry.id === item.id ? { ...entry, status: 'uploading', attempted: true } : entry));
            return authMode === 'assets' ? uploadAssetPhoto(item.file, { name: item.file.name, onProgress, signal: controller.signal }) : uploadToDrive(item.file, {
              accessToken, onProgress, authMode: personal ? 'user' : 'server', destination: item.destination, visibility: item.visibility,
              idempotencyKey: item.idempotencyKey, signal: controller.signal,
            });
          }, controller.signal);
          if (controller.signal.aborted || requestEpoch !== getAuthSessionEpoch() || !ownsUpload()) continue;
          setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, status: 'done', progress: 100, driveFile } : entry));
          if (selectedDestination === 'tdf') {
            const readVersion = ++accountReadVersion.current;
            void loadStorageAccount().then((current) => {
              if (requestEpoch === getAuthSessionEpoch() && readVersion === accountReadVersion.current) setAccount(current);
            }).catch(() => undefined);
          }
          onComplete?.([driveFile]);
        } catch (error) {
          if (!controller.signal.aborted && requestEpoch === getAuthSessionEpoch() && ownsUpload()) {
            const message = uploadFailureMessage(normalizeUploadFailure(error, personal ? 'user' : 'server'), english);
            setItems((prev) => prev.map((entry) => entry.id === item.id ? { ...entry, status: 'error', error: message } : entry));
          }
        } finally {
          if (controllers.current.get(item.id) === controller) controllers.current.delete(item.id);
        }
      }
    };
    await Promise.all(Array.from({ length: Math.min(3, uploads.length) }, worker));
  }, [account, authMode, ensureToken, english, onComplete, ownsUpload]);

  const handleFiles = useCallback((files: FileList | File[]) => {
    if (!explicitDestination || !ownsUpload()) return;
    if (authMode !== 'assets' && Array.from(files).some((file) => file.size <= 0 || file.size > 50 * 1024 * 1024)) {
      setError(english ? 'Choose a non-empty file of at most 50 MiB. Your form has been preserved.' : 'Elige un archivo con contenido de hasta 50 MiB. Conservamos tu formulario.');
      return;
    }
    if (!multiple && files.length > 1) {
      setError(english ? 'Choose one file for this field.' : 'Elige un solo archivo para este campo.');
      return;
    }
    if (authMode !== 'assets' && files.length + recoverableUploads.length > 100) {
      setError(english ? 'Keep at most 100 files in this upload list. Remove old recovery entries before adding more.' : 'Conserva hasta 100 archivos en esta lista. Quita recuperaciones anteriores antes de añadir más.');
      return;
    }
    const uploads: UploadItem[] = Array.from(files).map((file) => ({
      id: crypto.randomUUID(), idempotencyKey: newUploadKey(), destination: explicitDestination, visibility: authMode === 'assets' ? 'public' : visibility, file, progress: 0, status: 'pending',
    }));
    setError(null);
    setItems((prev) => [...prev, ...uploads]);
    if (authMode !== 'assets') setRecoverableUploads((previous) => [...previous, ...uploads.map((item) => ({
      id: item.id, idempotencyKey: item.idempotencyKey, fileName: item.file.name, byteSize: item.file.size,
      mediaType: item.file.type || 'application/octet-stream', destination: item.destination, visibility: item.visibility,
    }))]);
    void runUploads(uploads);
  }, [authMode, explicitDestination, runUploads, visibility, ownsUpload, recoverableUploads.length, english, setRecoverableUploads, multiple]);

  const handleDrop = (evt: React.DragEvent) => {
    evt.preventDefault();
    setIsDragging(false);
    if (evt.dataTransfer.files && evt.dataTransfer.files.length > 0) {
      void handleFiles(evt.dataTransfer.files);
      evt.dataTransfer.clearData();
    }
  };

  const handleBrowse = () => { if (!explicitDestination) { setError(english ? 'Choose where to store the file first.' : 'Primero elige dónde guardar el archivo.'); return; } inputRef.current?.click(); };
  const removeItem = (id: string) => {
    cancelled.current.add(id); controllers.current.get(id)?.abort(); setItems((prev) => prev.filter((it) => it.id !== id));
    setRecoverableUploads((previous) => previous.filter((entry) => entry.id !== id));
  };

  return (
    <Stack spacing={1}>
      {ownerPartyId && (!recovery.persistenceAvailable || !formRecoveryAvailable) && <Alert severity="warning">{english
        ? 'This browser could not retain the draft for reload recovery. Keep this tab open until you save.'
        : 'El navegador no pudo conservar el borrador para recuperarlo al recargar. Mantén esta pestaña abierta hasta guardar.'}</Alert>}
      {recoverableUploads.filter((entry) => !items.some((item) => item.id === entry.id)).map((entry) => <Alert key={entry.id} severity="info"
        action={<Stack direction="row"><Button onClick={() => { reselecting.current = entry; recoveryInputRef.current?.click(); }}>
          {english ? 'Select file again' : 'Reseleccionar archivo'}</Button><Button onClick={() => removeItem(entry.id)}>{english ? 'Dismiss' : 'Quitar'}</Button></Stack>}>
        {entry.fileName} · {entry.destination === 'tdf' ? 'TDF' : 'Google Drive'} · {entry.visibility === 'public' ? (english ? 'Public' : 'Público') : (english ? 'Private' : 'Privado')} · {english ? 'The browser no longer has its bytes. Select the original file to verify or recover this upload. Removing this entry does not delete the provider file.' : 'El navegador ya no conserva sus bytes. Selecciona el archivo original para comprobar o recuperar esta carga. Quitar esta entrada no borra el archivo del proveedor.'}
      </Alert>)}
      <input ref={recoveryInputRef} data-testid="storage-upload-reselect" type="file" hidden onChange={(event) => {
        const original = reselecting.current, file = event.target.files?.[0];
        event.target.value = '';
        if (!original || !file || !ownsUpload()) return;
        if (file.name !== original.fileName || file.size !== original.byteSize || (file.type || 'application/octet-stream') !== original.mediaType) {
          setError(english ? 'Choose the original file. Your form and upload identity have been preserved.' : 'Elige el archivo original. Conservamos tu formulario y la identidad de la carga.');
          return;
        }
        const item: UploadItem = { ...original, file, status: 'pending', progress: 0, attempted: true };
        setItems((previous) => [...previous.filter((entry) => entry.id !== item.id), item]);
        reselecting.current = null;
        setError(null);
        void runUploads([item]);
      }} />
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
      <Typography variant="caption">{visibility === 'public' || authMode === 'assets' ? (english ? 'These files will be accessible by link.' : 'Estos archivos serán accesibles mediante su enlace.') : (english ? 'TDF files remain private.' : 'Los archivos guardados en TDF conservan acceso privado.')}</Typography>
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
          <>
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
          {driveStatus === 'ready' && <Button onClick={() => { void resetAuth().catch(() => setError(english ? 'Could not disconnect Drive. Retry shortly.' : 'No pudimos desconectar Drive. Reintenta.')); }}>
            {english ? 'Disconnect Drive' : 'Desconectar Drive'}
          </Button>}
          </>
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
          data-testid="storage-upload-select"
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

      {items.filter((item) => item.status === 'error').length > 1 && <Button onClick={() => {
        const groups = new Map<StorageDestination, UploadItem[]>();
        for (const item of items.filter((entry) => entry.status === 'error')) {
          const group = groups.get(item.destination) ?? [];
          group.push({ ...item, attempted: true }); groups.set(item.destination, group);
        }
        for (const group of groups.values()) void runUploads(group);
      }}>{english ? 'Retry failed uploads' : 'Reintentar cargas fallidas'}</Button>}
      {items.length > 0 && (
        <LazyPaginatedList
          items={items}
          pagination={{ itemLabel: 'archivos', initialRowsPerPage: 5 }}
          renderItems={(visibleItems) => (
            <Stack spacing={1}>
              {visibleItems.map((item) => (
                <UploadItemRow key={item.id} item={item} onRemove={removeItem} onRetry={(entry) => void runUploads([{ ...entry, attempted: true }])} english={english} />
              ))}
            </Stack>
          )}
        />
      )}
    </Stack>
  );
}
