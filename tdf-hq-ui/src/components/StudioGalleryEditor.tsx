import { useEffect, useRef, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, Checkbox, FormControlLabel, Stack, TextField, Typography } from '@mui/material';
import { uploadToDrive } from '../api/drive';
import { newUploadKey, UploadFailure, uploadFailureMessage } from '../api/uploadFailure';
import { isStudioImageUrl, STUDIO_GALLERY_LIMIT, type StudioGalleryPayload, type StudioPhoto } from '../utils/studioGallery';

interface Props {
  value: StudioGalleryPayload;
  onChange: (value: StudioGalleryPayload) => void;
  onBusyChange: (busy: boolean) => void;
  disabled?: boolean;
}

export default function StudioGalleryEditor({ value, onChange, onBusyChange, disabled = false }: Props) {
  const [uploading, setUploading] = useState(false);
  const [error, setError] = useState('');
  const [imageUrl, setImageUrl] = useState('');
  const [pendingRemoval, setPendingRemoval] = useState<string | null>(null);
  const [editingPhotoId, setEditingPhotoId] = useState(value.photos[0]?.id ?? null);
  const latest = useRef({ value, onChange, onBusyChange });
  latest.current = { value, onChange, onBusyChange };
  const upload = useRef<AbortController | null>(null);
  const uploadAttempt = useRef<{ file: File; key: string } | null>(null);
  useEffect(() => () => { upload.current?.abort(); latest.current.onBusyChange(false); }, []);

  const update = (id: string, patch: Partial<StudioPhoto>) =>
    onChange({ ...value, photos: value.photos.map(photo => photo.id === id ? { ...photo, ...patch } : photo) });
  const move = (index: number, offset: number) => {
    const photos = [...value.photos];
    const photo = photos.splice(index, 1)[0];
    if (!photo) return;
    photos.splice(index + offset, 0, photo);
    onChange({ ...value, photos });
  };
  const add = (url: string, title: string) => {
    const state = latest.current;
    if (state.value.photos.length >= STUDIO_GALLERY_LIMIT) { setError('La galería ya tiene 200 fotos.'); return; }
    state.onChange({ ...state.value, photos: [...state.value.photos, {
      id: crypto.randomUUID(), title, alt: title, imageUrl: url, visible: true,
    }] });
  };
  const uploadFile = async (file: File) => {
    setError('');
    if (!['image/jpeg', 'image/png', 'image/webp'].includes(file.type) || file.size > 10 * 1024 * 1024) {
      setError('Elige una foto JPG, PNG o WebP de hasta 10 MB.'); return;
    }
    const controller = new AbortController();
    const previous = uploadAttempt.current;
    const sameFile = previous && previous.file.name === file.name && previous.file.size === file.size && previous.file.lastModified === file.lastModified;
    const key = sameFile ? previous.key : newUploadKey();
    uploadAttempt.current = { file, key };
    upload.current = controller;
    setUploading(true); onBusyChange(true);
    try {
      const result = await uploadToDrive(file, { destination: 'tdf', visibility: 'public', idempotencyKey: key, signal: controller.signal });
      if (controller.signal.aborted) return;
      if (!result.publicUrl?.startsWith('https://')) throw new Error('El almacenamiento no devolvió una URL pública HTTPS.');
      add(result.publicUrl, file.name.replace(/\.[^.]+$/, ''));
      uploadAttempt.current = null;
    } catch (reason) {
      if (!controller.signal.aborted) setError(reason instanceof UploadFailure ? uploadFailureMessage(reason) : reason instanceof Error ? reason.message : 'No se pudo subir la foto. Reintenta.');
    } finally {
      if (!controller.signal.aborted) { setUploading(false); latest.current.onBusyChange(false); }
    }
  };
  const locked = disabled || uploading;
  return <Stack spacing={2} aria-label="Organizar fotos del estudio" sx={{ color: 'text.primary' }}>
    <Typography variant="h6">Fotos del estudio ({value.photos.length})</Typography>
    <Alert severity="info">Ordena las fotos, completa sus descripciones y guarda un borrador. Otro administrador o webmaster debe revisar y publicar la versión para actualizar la galería.</Alert>
    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
      <TextField fullWidth label="URL pública de la nueva foto" value={imageUrl} disabled={locked} onChange={event => setImageUrl(event.target.value)} />
      <Button disabled={locked || !imageUrl.trim() || value.photos.length >= STUDIO_GALLERY_LIMIT} onClick={() => {
        try {
          const url = new URL(imageUrl.trim());
          if (url.protocol !== 'https:' || url.username || url.password) throw new Error();
          add(url.href, 'Nueva foto'); setImageUrl(''); setError('');
        } catch { setError('Usa una URL pública que comience con https://.'); }
      }}>Añadir por URL</Button>
    </Stack>
    <Button component="label" variant="outlined" disabled={locked || value.photos.length >= STUDIO_GALLERY_LIMIT}>
      {uploading ? 'Subiendo foto…' : 'Subir foto pública'}
      <input aria-label="Subir foto pública" type="file" accept="image/jpeg,image/png,image/webp" hidden disabled={locked} onChange={event => {
        const file = event.target.files?.[0]; event.target.value = ''; if (file) void uploadFile(file);
      }} />
    </Button>
    <Typography variant="caption">JPG, PNG o WebP, hasta 10 MB. El archivo subido tendrá una dirección pública. Aparecerá en la galería al publicar la versión. Quitar una foto de la galería conserva el archivo original.</Typography>
    {error && <Alert severity="error">{error}</Alert>}
    {value.photos.length === 0 && <Typography>La galería está vacía. Añade una foto o publica esta versión para retirar todas las fotos.</Typography>}
    {value.photos.map((photo, index) => <Card component="section" aria-label={`Foto ${index + 1}: ${photo.title}`} key={photo.id} variant="outlined">
      <CardContent><Stack spacing={1.5}>
        <Stack direction="row" spacing={2} alignItems="center">
          {isStudioImageUrl(photo.thumbnailUrl || photo.imageUrl) && <Box component="img" src={photo.thumbnailUrl || photo.imageUrl} alt="" loading="lazy" referrerPolicy="no-referrer" sx={{ width: 88, height: 66, objectFit: 'cover', borderRadius: 1 }} />}
          <Typography fontWeight={600}>{index + 1}. {photo.title || 'Sin título'}</Typography>
        </Stack>
        <Button aria-label={`Editar foto: ${photo.title}`} aria-expanded={editingPhotoId === photo.id} aria-controls={`studio-photo-fields-${photo.id}`} onClick={() => setEditingPhotoId(editingPhotoId === photo.id ? null : photo.id)}>Editar datos</Button>
        {editingPhotoId === photo.id && <Stack spacing={1.5} id={`studio-photo-fields-${photo.id}`}>
        <TextField label="Título de la foto" value={photo.title} disabled={locked} onChange={event => update(photo.id, { title: event.target.value })} inputProps={{ maxLength: 200 }} />
        <TextField label="Descripción de la imagen" helperText="Describe lo que se ve para quienes usan lectores de pantalla." value={photo.alt} disabled={locked} multiline onChange={event => update(photo.id, { alt: event.target.value })} inputProps={{ maxLength: 1000 }} />
        <TextField label="URL de la imagen completa" value={photo.imageUrl} disabled={locked} onChange={event => update(photo.id, { imageUrl: event.target.value, thumbnailUrl: undefined })} />
        <TextField label="Créditos" value={photo.credit ?? ''} disabled={locked} onChange={event => update(photo.id, { credit: event.target.value })} />
        <TextField label="Fecha de captura" placeholder="AAAA-MM o AAAA-MM-DD" helperText="Puedes indicar solo el mes; deja vacío si no conoces la fecha." value={photo.capturedAt ?? ''} disabled={locked} onChange={event => update(photo.id, { capturedAt: event.target.value })} />
        <TextField label="Enlace a la publicación original" value={photo.sourceUrl ?? ''} disabled={locked} onChange={event => update(photo.id, { sourceUrl: event.target.value })} />
        </Stack>}
        <FormControlLabel label="Visible en la galería al publicar" control={<Checkbox checked={photo.visible} disabled={locked} onChange={(_, visible) => update(photo.id, { visible })} />} />
        <Stack direction="row" spacing={1} flexWrap="wrap">
          <Button disabled={locked || index === 0} aria-label={`Subir foto: ${photo.title}`} onClick={() => move(index, -1)}>Subir</Button>
          <Button disabled={locked || index === value.photos.length - 1} aria-label={`Bajar foto: ${photo.title}`} onClick={() => move(index, 1)}>Bajar</Button>
          <Button color="error" disabled={locked} onClick={() => setPendingRemoval(photo.id)}>Quitar foto</Button>
        </Stack>
        {pendingRemoval === photo.id && <Alert severity="warning">¿Quitar esta foto de la galería? Podrás recuperarla desde el historial de versiones.
          <Button color="error" disabled={locked} onClick={() => {
            onChange({ ...value, photos: value.photos.filter(item => item.id !== photo.id) }); setPendingRemoval(null);
          }}>Confirmar quitar</Button><Button onClick={() => setPendingRemoval(null)}>Conservar foto</Button>
        </Alert>}
      </Stack></CardContent>
    </Card>)}
  </Stack>;
}
