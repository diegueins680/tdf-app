import { useEffect, useState } from 'react';
import { styled, type SxProps, type Theme } from '@mui/material';
import {
  directoryFallbackImageUrl,
  resolveDirectoryPreviewImage,
  type DirectoryPreviewKind,
} from '../../utils/directoryPreviewImage';

// A native img keeps width/height as intrinsic attributes (Box would consume
// them as style props), so the browser reserves space before loading.
const PreviewImg = styled('img')({});

interface DirectoryPreviewImageProps {
  kind: DirectoryPreviewKind;
  imageUrl?: string | null;
  alt: string;
  fallbackAlt: string;
  /** Intrinsic size hints; the box keeps this aspect ratio to avoid layout shift. */
  width?: number;
  height?: number;
  sizes?: string;
  eager?: boolean;
  sx?: SxProps<Theme>;
}

export default function DirectoryPreviewImage({
  kind,
  imageUrl,
  alt,
  fallbackAlt,
  width = 640,
  height = 480,
  sizes,
  eager = false,
  sx,
}: DirectoryPreviewImageProps) {
  const resolved = resolveDirectoryPreviewImage(imageUrl);
  const fallback = directoryFallbackImageUrl(kind);
  const [failed, setFailed] = useState(false);
  useEffect(() => { setFailed(false); }, [resolved]);
  const showFallback = !resolved || failed;
  return (
    <PreviewImg
      src={showFallback ? fallback : resolved}
      alt={showFallback ? fallbackAlt : alt}
      width={width}
      height={height}
      sizes={sizes}
      loading={eager ? 'eager' : 'lazy'}
      decoding="async"
      data-preview-source={showFallback ? 'placeholder' : 'media'}
      onError={() => { if (!showFallback) setFailed(true); }}
      sx={[
        {
          display: 'block',
          aspectRatio: `${width} / ${height}`,
          objectFit: 'cover',
          objectPosition: 'center',
          bgcolor: 'action.hover',
          flexShrink: 0,
        },
        ...(Array.isArray(sx) ? sx : [sx]),
      ]}
    />
  );
}
