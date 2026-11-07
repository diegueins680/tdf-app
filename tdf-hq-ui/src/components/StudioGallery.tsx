import { useState } from 'react';
import { Alert, Box, Button, ButtonBase, Dialog, DialogActions, DialogContent, DialogTitle, Typography } from '@mui/material';

// Bundled editorial assets selected from the owner's approved folders.
// Source IDs and derivative hashes: reports/studio-gallery-2026-10-26.json.
const PHOTOS = [
  { id: 'control-room', title: 'Sala de control', alt: 'Consola de mezcla frente a dos pantallas y monitores de estudio.' },
  { id: 'recording-room', title: 'Sala de grabación', alt: 'Sala con batería, guitarras, amplificadores y micrófonos.' },
  { id: 'instruments', title: 'Instrumentos y sonido', alt: 'Sintetizadores, micrófono y equipos junto a la consola de mezcla.' },
  { id: 'mixing-detail', title: 'Trabajo en consola', alt: 'Mano ajustando los faders de una consola de mezcla.' },
  { id: 'drum-session', title: 'Una sesión de batería', alt: 'Músico tocando una batería rodeada de micrófonos en el estudio.' },
  { id: 'studio-community', title: 'La música nos reúne', alt: 'Grupo de músicos reunidos junto a la consola en el estudio.' },
] as const;

type StudioPhoto = typeof PHOTOS[number];
const source = (photo: StudioPhoto, width: number) => `/assets/tdf-studio/${photo.id}-${width}.webp`;

export default function StudioGallery() {
  const [selected, setSelected] = useState<StudioPhoto | null>(null);
  const [failedThumbnails, setFailedThumbnails] = useState<ReadonlySet<string>>(new Set());
  const [failedPreview, setFailedPreview] = useState(false);
  const [loadingPreview, setLoadingPreview] = useState(true);
  const [attempt, setAttempt] = useState(0);
  const open = (photo: StudioPhoto) => {
    setFailedPreview(false);
    setLoadingPreview(true);
    setAttempt(0);
    setSelected(photo);
  };
  const close = () => setSelected(null);

  return (
    <Box role="group" aria-label="Fotos del estudio TDF" sx={{ mt: 4 }}>
      <Typography sx={{ mb: 2, color: '#e2e8f0' }}>
        Espacios, equipos y momentos compartidos. Abre una foto para verla completa.
      </Typography>
      <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))', md: 'repeat(3, minmax(0, 1fr))' }, gap: 2 }}>
        {PHOTOS.map(photo => (
          <Box component="figure" key={photo.id} sx={{ m: 0, minWidth: 0 }}>
            <ButtonBase onClick={() => open(photo)} aria-label={`Ampliar foto: ${photo.title}`}
              sx={{ width: '100%', display: 'block', borderRadius: 2, overflow: 'hidden', bgcolor: '#111b24', color: '#f8fafc', '&.Mui-focusVisible': { outline: '3px solid #e6c785', outlineOffset: 3 } }}>
              <Box sx={{ aspectRatio: '4 / 3', display: 'grid', placeItems: 'center' }}>
                {failedThumbnails.has(photo.id) ? (
                  <Typography sx={{ p: 2 }}>Foto no disponible. Abre para reintentar.</Typography>
                ) : (
                  <Box component="img" src={source(photo, 640)} alt={photo.alt} width={640} height={480} loading="lazy" decoding="async"
                    onError={() => setFailedThumbnails(previous => new Set(previous).add(photo.id))}
                    sx={{ width: '100%', height: '100%', objectFit: 'cover' }} />
                )}
              </Box>
            </ButtonBase>
            <Typography component="figcaption" sx={{ mt: 1, color: '#e2e8f0', fontWeight: 600 }}>{photo.title}</Typography>
          </Box>
        ))}
      </Box>
      <Typography sx={{ mt: 2, color: '#cbd5e1', fontSize: '0.9rem' }}>
        Fotos del archivo de TDF. Consulta la configuración y los equipos disponibles para tu proyecto al reservar.
      </Typography>
      <Dialog open={selected !== null} onClose={close} aria-labelledby="studio-photo-title" maxWidth="lg" fullWidth>
        {selected && <>
          <DialogTitle id="studio-photo-title">{selected.title}</DialogTitle>
          <DialogContent>
            {failedPreview ? (
              <Alert severity="warning" action={<Button color="inherit" onClick={() => { setAttempt(previous => previous + 1); setLoadingPreview(true); setFailedPreview(false); }}>Reintentar foto</Button>}>
                No se pudo cargar esta foto. Puedes reintentar o cerrar y seguir explorando.
              </Alert>
            ) : (
              <Box sx={{ position: 'relative', aspectRatio: '4 / 3', maxHeight: '70vh', bgcolor: '#111b24' }}>
                {loadingPreview && <Typography role="status" sx={{ position: 'absolute', top: '45%', width: '100%', textAlign: 'center', color: '#f8fafc' }}>Cargando foto…</Typography>}
                <Box component="img" key={`${selected.id}-${attempt}`} src={`${source(selected, 1280)}${attempt ? `?retry=${attempt}` : ''}`} alt={selected.alt}
                  decoding="async" onLoad={() => setLoadingPreview(false)} onError={() => setFailedPreview(true)}
                  sx={{ display: 'block', width: '100%', height: '100%', objectFit: 'contain' }} />
              </Box>
            )}
          </DialogContent>
          <DialogActions><Button onClick={close} sx={{ minHeight: 44 }}>Cerrar foto</Button></DialogActions>
        </>}
      </Dialog>
    </Box>
  );
}
