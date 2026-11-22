import { useRef, useState } from 'react';
import { Alert, Button, Checkbox, FormControlLabel, Stack, TextField, Typography } from '@mui/material';
import { reviewRequest, type Submission } from '../api/studioSubmissions';
import {
  STUDIO_GALLERY_LIMIT,
  type StudioGalleryPayload,
  type StudioPhoto,
} from '../utils/studioGallery';

interface Props {
  value: StudioGalleryPayload;
  onChange: (value: StudioGalleryPayload) => void;
  disabled: boolean;
  onBusyChange: (busy: boolean) => void;
}
const photoId = (row: Submission) => `studio-${row.batch}-${row.position}-${row.id}`;
const orderOf = (id: string) => /^studio-(\d+)-(\d+)-/.exec(id);

export function addReviewedPhotos(
  value: StudioGalleryPayload,
  rows: Submission[],
): StudioGalleryPayload {
  const photos = [...value.photos];
  for (const row of rows) {
    const id = photoId(row);
    if (photos.some((photo) => photo.id === id)) continue;
    const url = `/studio-submissions/images/${row.id}/`;
    const title = row.name.replace(/\.[^.]+$/, '').slice(0, 200) || 'Estudio TDF';
    const photo: StudioPhoto = {
      id,
      title,
      alt: title,
      imageUrl: url + 'large',
      thumbnailUrl: url + 'small',
      credit: row.credit,
      visible: true,
    };
    const index = photos.findIndex((existing) => {
      const order = orderOf(existing.id);
      return (
        !order ||
        Number(order[1]) < row.batch ||
        (Number(order[1]) === row.batch && Number(order[2]) > row.position)
      );
    });
    photos.splice(index < 0 ? photos.length : index, 0, photo);
  }
  if (photos.length > STUDIO_GALLERY_LIMIT)
    throw new Error('El borrador superaría el límite de 200 fotos.');
  return { ...value, photos };
}

export default function StudioSubmissionReview({ value, onChange, disabled, onBusyChange }: Props) {
  const latest = useRef({ value, onChange });
  latest.current = { value, onChange };
  const [open, setOpen] = useState(false);
  const [rows, setRows] = useState<Submission[]>([]);
  const [selected, setSelected] = useState<string[]>([]);
  const [link, setLink] = useState('');
  const [previews, setPreviews] = useState<Record<string, string>>({});
  const [error, setError] = useState('');
  const [notice, setNotice] = useState('');
  const [busy, setBusy] = useState(false);
  const run = async (operation: () => Promise<void>) => {
    setError('');
    setNotice('');
    setBusy(true);
    onBusyChange(true);
    try {
      await operation();
    } catch (reason) {
      setError(reason instanceof Error ? reason.message : 'No se pudo completar la acción.');
    } finally {
      setBusy(false);
      onBusyChange(false);
    }
  };
  const loadLink = async (action: string) => {
    const { token } = await reviewRequest<{ token: string | null }>(action);
    setLink(token ? `${window.location.origin}/estudio/enviar#enlace=${token}` : '');
  };
  const refresh = async () => {
    setRows(await reviewRequest<Submission[]>('queue'));
    setSelected([]);
  };
  const review = async (ids: string[], accepted: boolean) => {
    const members = rows.filter((row) => ids.includes(row.id));
    if (accepted) addReviewedPhotos(latest.current.value, members);
    await reviewRequest('review', { ids, state: accepted ? 'accepted' : 'rejected' });
    if (accepted) latest.current.onChange(addReviewedPhotos(latest.current.value, members));
    setNotice(
      accepted
        ? 'Fotos añadidas al editor. Revisa sus descripciones, guarda el borrador y solicita su publicación.'
        : 'Fotos rechazadas.',
    );
    await refresh();
  };
  const locked = busy || disabled;
  const queue = rows.filter(
    (row) => row.state === 'pending' || !value.photos.some((photo) => photo.id === photoId(row)),
  );
  return (
    <Stack spacing={1} className="ph-no-capture">
      <Button
        disabled={locked}
        onClick={() => {
          if (open) {
            setOpen(false);
            return;
          }
          setOpen(true);
          void run(async () => {
            await loadLink('link');
            await refresh();
          });
        }}
      >
        {open ? 'Cerrar contribuciones' : 'Revisar contribuciones y gestionar enlace'}
      </Button>
      {open && (
        <>
          <Typography variant="h6">Enlace de contribución</Typography>
          {link && (
            <TextField
              label="Enlace privado para compartir"
              value={link}
              inputProps={{ readOnly: true }}
            />
          )}
          <Stack direction="row" flexWrap="wrap">
            {link && (
              <Button
                disabled={locked}
                onClick={() =>
                  void run(async () => {
                    await navigator.clipboard.writeText(link);
                    setNotice('Enlace copiado.');
                  })
                }
              >
                Copiar enlace
              </Button>
            )}
            <Button disabled={locked} onClick={() => void run(() => loadLink('replace'))}>
              {link ? 'Reemplazar e invalidar enlace anterior' : 'Crear enlace'}
            </Button>
            {link && (
              <Button disabled={locked} onClick={() => void run(() => loadLink('disable'))}>
                Desactivar enlace
              </Button>
            )}
          </Stack>
          <Typography variant="h6">Fotos enviadas</Typography>
          <Button disabled={locked} onClick={() => void run(refresh)}>
            Actualizar envíos
          </Button>
          {!queue.length && <Typography>No hay fotos por incorporar.</Typography>}
          {[...new Set(queue.map((row) => row.batch))].map((batch) => {
            const group = queue.filter((row) => row.batch === batch);
            const ids = group.map((row) => row.id);
            const pending = group.filter((row) => row.state === 'pending').map((row) => row.id);
            return (
              <Stack spacing={1} key={batch}>
                <Typography fontWeight={600}>Lote {batch}</Typography>
                <Button disabled={locked} onClick={() => void run(() => review(ids, true))}>
                  Incorporar lote al borrador
                </Button>
                <Button
                  disabled={locked || !pending.length}
                  onClick={() => void run(() => review(pending, false))}
                >
                  Rechazar pendientes del lote
                </Button>
                {group.map((row) => (
                  <Stack key={row.id} spacing={0.5}>
                    <FormControlLabel
                      control={
                        <Checkbox
                          disabled={locked}
                          checked={selected.includes(row.id)}
                          onChange={(_, checked) =>
                            setSelected(
                              checked ? [...selected, row.id] : selected.filter((id) => id !== row.id),
                            )
                          }
                        />
                      }
                      label={row.name + (row.credit ? ' · ' + row.credit : '')}
                    />
                    {previews[row.id] ? (
                      <img src={previews[row.id]} alt={row.name} style={{ maxWidth: 240 }} />
                    ) : (
                      <Button
                        disabled={locked}
                        onClick={() =>
                          void run(async () => {
                            const { jpeg } = await reviewRequest<{ jpeg: string }>('preview', {
                              id: row.id,
                            });
                            setPreviews((previous) => ({
                              ...previous,
                              [row.id]: 'data:image/jpeg;base64,' + jpeg,
                            }));
                          })
                        }
                      >
                        Ver foto
                      </Button>
                    )}
                  </Stack>
                ))}
              </Stack>
            );
          })}
          <Button
            disabled={locked || !selected.length || selected.length > 30}
            onClick={() => void run(() => review(selected, true))}
          >
            Incorporar selección al borrador
          </Button>
          <Button
            disabled={
              locked ||
              !selected.length ||
              selected.length > 30 ||
              rows.some((row) => selected.includes(row.id) && row.state !== 'pending')
            }
            onClick={() => void run(() => review(selected, false))}
          >
            Rechazar selección
          </Button>
        </>
      )}
      {error && <Alert severity="error">{error}</Alert>}
      {notice && <Alert severity="success">{notice}</Alert>}
    </Stack>
  );
}
