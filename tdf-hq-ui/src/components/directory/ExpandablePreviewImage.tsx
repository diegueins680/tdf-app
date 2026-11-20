import { useEffect, useState, type ComponentProps } from 'react';
import CloseIcon from '@mui/icons-material/Close';
import { Box, ButtonBase, Dialog, IconButton } from '@mui/material';
import DirectoryPreviewImage from './DirectoryPreviewImage';
import { resolveDirectoryPreviewImage } from '../../utils/directoryPreviewImage';

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
        PaperProps={{ 'aria-label': preview.alt, sx: { m: { xs: 1, sm: 3 }, bgcolor: 'transparent', boxShadow: 'none', overflow: 'visible' } }}
      >
        <Box sx={{ position: 'relative', lineHeight: 0 }}>
          <Box
            component="img"
            src={resolved}
            alt={preview.alt}
            onClick={() => setOpen(false)}
            onError={() => { setOpen(false); setFailed(true); }}
            sx={{
              display: 'block',
              maxWidth: { xs: 'calc(100vw - 16px)', sm: 'calc(100vw - 48px)' },
              maxHeight: { xs: 'calc(100dvh - 16px)', sm: 'calc(100dvh - 48px)' },
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
