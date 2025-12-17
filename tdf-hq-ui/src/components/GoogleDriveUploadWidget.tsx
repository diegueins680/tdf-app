import { useCallback, useRef, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  IconButton,
  LinearProgress,
  Stack,
  Typography,
} from '@mui/material';
import UploadFileIcon from '@mui/icons-material/UploadFile';
import DeleteIcon from '@mui/icons-material/Delete';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import type { DriveFileInfo } from '../services/googleDrive';
import { uploadToDrive } from '../api/drive';

interface UploadItem {
  id: string;
  file: File;
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
}

export default function GoogleDriveUploadWidget({
  label = 'Subir fotos',
  helperText = 'Arrastra imágenes o haz clic para seleccionar. Se guardan en Google Drive.',
  onComplete,
  accept = 'image/*',
  multiple = true,
  dense = false,
}: GoogleDriveUploadWidgetProps) {
  const inputRef = useRef<HTMLInputElement | null>(null);
  const [items, setItems] = useState<UploadItem[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [isDragging, setIsDragging] = useState(false);

  const handleFiles = useCallback(
    async (files: FileList | File[]) => {
      const fileArr = Array.from(files);
      setError(null);
      const uploads = fileArr.map<UploadItem>((file) => ({
        id: crypto.randomUUID(),
        file,
        progress: 0,
        status: 'pending',
      }));
      setItems((prev) => [...prev, ...uploads]);
      uploads.forEach((item) => {
        setItems((prev) =>
          prev.map((it) => (it.id === item.id ? { ...it, status: 'uploading', progress: 5 } : it)),
        );
        void (async () => {
          try {
            const driveFile = await uploadToDrive(item.file, {
              onProgress: (pct) =>
                setItems((prev) =>
                  prev.map((it) => (it.id === item.id ? { ...it, progress: Math.max(pct, it.progress) } : it)),
                ),
            });
            setItems((prev) =>
              prev.map((it) =>
                it.id === item.id ? { ...it, status: 'done', progress: 100, driveFile } : it,
              ),
            );
            if (onComplete) {
              onComplete([driveFile]);
            }
          } catch (err) {
            setItems((prev) =>
              prev.map((it) =>
                it.id === item.id
                  ? {
                      ...it,
                      status: 'error',
                      error: err instanceof Error ? err.message : 'No se pudo subir el archivo.',
                    }
                  : it,
              ),
            );
          }
        })();
      });
    },
    [onComplete],
  );

  const handleDrop = (evt: React.DragEvent<HTMLDivElement>) => {
    evt.preventDefault();
    setIsDragging(false);
    if (evt.dataTransfer.files && evt.dataTransfer.files.length > 0) {
      void handleFiles(evt.dataTransfer.files);
      evt.dataTransfer.clearData();
    }
  };

  const handleBrowse = () => inputRef.current?.click();

  return (
    <Stack spacing={1}>
      <Stack direction="row" alignItems="center" spacing={1}>
        <Button
          variant="outlined"
          startIcon={<UploadFileIcon />}
          onClick={handleBrowse}
          size={dense ? 'small' : 'medium'}
        >
          {label}
        </Button>
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
        onClick={handleBrowse}
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
          Arrastra archivos aquí o haz clic para seleccionarlos
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

      <Stack spacing={1}>
        {items.map((item) => (
          <Box
            key={item.id}
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
            <Box sx={{ width: 48, height: 48, borderRadius: 1, overflow: 'hidden', bgcolor: 'grey.100' }}>
              {item.file.type.startsWith('image/') ? (
                <img
                  src={URL.createObjectURL(item.file)}
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
            <Stack spacing={0.25} flex={1} minWidth={0}>
              <Typography variant="body2" noWrap>
                {item.file.name}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                {(item.file.size / 1024 / 1024).toFixed(2)} MB
              </Typography>
              <LinearProgress
                variant={item.status === 'done' ? 'determinate' : 'determinate'}
                value={item.status === 'error' ? 0 : item.progress}
                color={item.status === 'error' ? 'error' : 'primary'}
              />
              {item.status === 'done' && (item.driveFile?.publicUrl || item.driveFile?.webContentLink || item.driveFile?.webViewLink) && (
                <Typography variant="caption" color="text.secondary">
                  {item.driveFile.publicUrl ?? item.driveFile.webContentLink ?? item.driveFile.webViewLink}
                </Typography>
              )}
              {item.status === 'error' && (
                <Typography variant="caption" color="error">
                  {item.error ?? 'Error al subir'}
                </Typography>
              )}
            </Stack>
            {item.status === 'done' && <CheckCircleIcon color="success" fontSize="small" />}
            {item.status === 'error' && (
              <IconButton size="small" onClick={() => setItems((prev) => prev.filter((it) => it.id !== item.id))}>
                <DeleteIcon fontSize="small" />
              </IconButton>
            )}
          </Box>
        ))}
      </Stack>
    </Stack>
  );
}
