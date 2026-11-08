import { useState } from 'react';
import { Alert, Box, Button, ButtonBase, Dialog, DialogActions, DialogContent, DialogTitle, Pagination, Typography } from '@mui/material';
import { useCmsContent } from '../hooks/useCmsContent';
import { parseStudioGallery, STUDIO_GALLERY_SLUG, studioPhotoCaption, type StudioPhoto } from '../utils/studioGallery';

const PAGE_SIZE = 6;

export default function StudioGallery() {
  const content = useCmsContent(STUDIO_GALLERY_SLUG, 'es');
  if (content.isPending) return <Typography role="status" sx={{ mt: 4 }}>Cargando fotos del estudio…</Typography>;
  if (content.isError) return <Alert severity="warning" sx={{ mt: 4 }} action={<Button color="inherit" onClick={() => { void content.refetch(); }}>Reintentar</Button>}>No se pudo cargar la galería del estudio.</Alert>;
  if (content.data.ccdStatus !== 'published') return <Typography sx={{ mt: 4 }}>Todavía no hay fotos publicadas.</Typography>;
  try {
    const gallery = parseStudioGallery(content.data.ccdPayload);
    return <StudioGalleryPhotos key={`${content.data.ccdId}:${content.data.ccdVersion}`} photos={gallery.photos.filter(photo => photo.visible)} />;
  } catch {
    return <Alert severity="warning" sx={{ mt: 4 }}>La galería no está disponible en este momento.</Alert>;
  }
}

export function StudioGalleryPhotos({ photos }: { photos: StudioPhoto[] }) {
  const [page, setPage] = useState(1);
  const pageCount = Math.ceil(photos.length / PAGE_SIZE);
  const currentPage = Math.min(page, Math.max(1, pageCount));
  const firstPhoto = (currentPage - 1) * PAGE_SIZE;
  const visiblePhotos = photos.slice(firstPhoto, firstPhoto + PAGE_SIZE);
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

  if (photos.length === 0) return <Typography sx={{ mt: 4 }}>Todavía no hay fotos publicadas.</Typography>;
  return (
    <Box role="group" aria-label="Fotos del estudio TDF" sx={{ mt: 4 }}>
      <Typography sx={{ mb: 2, color: '#e2e8f0' }}>
        Espacios, equipos y momentos compartidos. Abre una foto para verla completa.
      </Typography>
      <Box sx={{ display: 'grid', gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))', md: 'repeat(3, minmax(0, 1fr))' }, gap: 2 }}>
        {visiblePhotos.map(photo => (
          <Box component="figure" key={photo.id} sx={{ m: 0, minWidth: 0 }}>
            <ButtonBase onClick={() => open(photo)} aria-label={`Ampliar foto: ${photo.title}`}
              sx={{ width: '100%', display: 'block', borderRadius: 2, overflow: 'hidden', bgcolor: '#111b24', color: '#f8fafc', '&.Mui-focusVisible': { outline: '3px solid #e6c785', outlineOffset: 3 } }}>
              <Box sx={{ position: 'relative', aspectRatio: '4 / 3', display: 'grid', placeItems: 'center', overflow: 'hidden' }}>
                {failedThumbnails.has(photo.id) ? (
                  <Typography sx={{ p: 2 }}>Foto no disponible. Abre para reintentar.</Typography>
                ) : (
                  <Box component="img" src={photo.thumbnailUrl?.length ? photo.thumbnailUrl : photo.imageUrl} alt={photo.alt} width={640} height={480} loading="lazy" decoding="async" referrerPolicy="no-referrer"
                    onError={() => setFailedThumbnails(previous => new Set(previous).add(photo.id))}
                    sx={{ position: 'absolute', inset: 0, width: '100%', height: '100%', objectFit: 'cover' }} />
                )}
              </Box>
            </ButtonBase>
            <Typography component="figcaption" sx={{ mt: 1, color: '#e2e8f0', fontWeight: 600 }}>{photo.title}</Typography>
            {studioPhotoCaption(photo) && <Typography sx={{ mt: 0.5, color: '#cbd5e1', fontSize: '0.8rem' }}>{studioPhotoCaption(photo)}</Typography>}
          </Box>
        ))}
      </Box>
      <Box sx={{ mt: 3, display: 'flex', flexDirection: 'column', alignItems: 'center', gap: 1.5 }}>
        <Typography role="status" aria-live="polite" aria-atomic="true" sx={{ color: '#cbd5e1', fontSize: '0.9rem' }}>
          Fotos {firstPhoto + 1}–{Math.min(firstPhoto + PAGE_SIZE, photos.length)} de {photos.length}
        </Typography>
        {pageCount > 1 && <Pagination
          aria-label="Páginas de fotos del estudio"
          count={pageCount}
          page={currentPage}
          onChange={(_, nextPage) => setPage(nextPage)}
          getItemAriaLabel={(type, itemPage, selectedPage) => {
            if (type === 'previous') return 'Página anterior de fotos';
            if (type === 'next') return 'Página siguiente de fotos';
            return `${selectedPage ? 'Página actual' : 'Ir a la página'} ${itemPage} de fotos`;
          }}
          sx={{ '& .MuiPaginationItem-root': { color: '#e2e8f0', minWidth: 44, minHeight: 44 }, '& .MuiPaginationItem-root.Mui-selected': { bgcolor: '#c4b5fd', color: '#111827', '&:hover': { bgcolor: '#ddd6fe' } } }}
        />}
      </Box>
      <Typography sx={{ mt: 2, color: '#cbd5e1', fontSize: '0.9rem' }}>
        Fotos del archivo de TDF. Consulta la configuración y los equipos disponibles para tu proyecto al reservar.
      </Typography>
      <Dialog open={selected !== null} onClose={close} aria-labelledby="studio-photo-title" maxWidth="lg" fullWidth>
        {selected && <>
          <DialogTitle id="studio-photo-title">{selected.title}</DialogTitle>
          <DialogContent>
            <Typography sx={{ mb: 2 }}>
              {studioPhotoCaption(selected)}{' '}
              {selected.sourceUrl && <Box component="a" href={selected.sourceUrl} target="_blank" rel="noopener noreferrer" sx={{ color: 'inherit', textDecoration: 'underline' }}>Ver publicación y créditos</Box>}
            </Typography>
            {failedPreview ? (
              <Alert severity="warning" action={<Button color="inherit" onClick={() => { setAttempt(previous => previous + 1); setLoadingPreview(true); setFailedPreview(false); }}>Reintentar foto</Button>}>
                No se pudo cargar esta foto. Puedes reintentar o cerrar y seguir explorando.
              </Alert>
            ) : (
              <Box sx={{ position: 'relative', height: 'min(70vh, 800px)', bgcolor: '#111b24' }}>
                {loadingPreview && <Typography role="status" sx={{ position: 'absolute', top: '45%', width: '100%', textAlign: 'center', color: '#f8fafc' }}>Cargando foto…</Typography>}
                <Box component="img" key={`${selected.id}-${attempt}`} src={`${selected.imageUrl}${attempt ? `${selected.imageUrl.includes('?') ? '&' : '?'}retry=${attempt}` : ''}`} alt={selected.alt}
                  decoding="async" referrerPolicy="no-referrer" onLoad={() => setLoadingPreview(false)} onError={() => setFailedPreview(true)}
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
