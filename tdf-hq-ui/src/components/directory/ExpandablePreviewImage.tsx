import { useEffect, useState, type ComponentProps } from 'react';
import CloseIcon from '@mui/icons-material/Close';
import { Box, ButtonBase, Dialog, IconButton } from '@mui/material';
import DirectoryPreviewImage from './DirectoryPreviewImage';
import { resolveDirectoryPreviewImage } from '../../services/directoryPreviewImage';

type ExpandablePreviewImageProps = ComponentProps<typeof DirectoryPreviewImage> & {
  /** Accessible name of the control that opens the full image. */
  expandLabel: string;
  closeLabel: string;
};

/**
 * The cropped preview, plus a viewer that shows the whole image at the largest
 * size the window allows. Without real media the placeholder stays inert:
 * there is nothing larger to show.
 */
export default function ExpandablePreviewImage({
  expandLabel,
  closeLabel,
  ...preview
}: ExpandablePreviewImageProps) {
  const resolved = resolveDirectoryPreviewImage(preview.imageUrl);
  const [open, setOpen] = useState(false);
  const [failed, setFailed] = useState(false);
  useEffect(() => { setOpen(false); setFailed(false); }, [resolved]);

  if (!resolved || failed) return <DirectoryPreviewImage {...preview} />;
  return (
    <>
      <ButtonBase
        aria-label={expandLabel}
        aria-haspopup="dialog"
        onClick={() => setOpen(true)}
        sx={{
          display: 'block',
          flexShrink: 0,
          width: { xs: '100%', sm: 'auto' },
          borderRadius: 3,
          cursor: 'zoom-in',
          '&:focus-visible': { outline: '2px solid', outlineColor: 'primary.main', outlineOffset: 2 },
        }}
      >
        <DirectoryPreviewImage {...preview} />
      </ButtonBase>
      <Dialog
        open={open}
        onClose={() => setOpen(false)}
        maxWidth={false}
        PaperProps={{ 'aria-label': preview.alt, sx: {
          m: 0, width: '100%', maxWidth: '100%', height: '100%', maxHeight: '100%',
          bgcolor: 'transparent', boxShadow: 'none', borderRadius: 0,
        } }}
      >
        <Box sx={{ position: 'relative', height: '100%', minHeight: 0, lineHeight: 0 }}>
          <Box
            component="img"
            src={resolved}
            alt={preview.alt}
            onClick={() => setOpen(false)}
            onError={() => { setOpen(false); setFailed(true); }}
            sx={{
              display: 'block',
              boxSizing: 'border-box', width: '100%', height: '100%', p: 1,
              objectFit: 'contain',
              borderRadius: 2,
              cursor: 'zoom-out',
            }}
          />
          <IconButton
            aria-label={closeLabel}
            onClick={() => setOpen(false)}
            sx={{
              position: 'absolute',
              top: 8,
              right: 8,
              color: 'common.white',
              bgcolor: 'rgba(0, 0, 0, 0.6)',
              '&:hover': { bgcolor: 'rgba(0, 0, 0, 0.8)' },
            }}
          >
            <CloseIcon />
          </IconButton>
        </Box>
      </Dialog>
    </>
  );
}
