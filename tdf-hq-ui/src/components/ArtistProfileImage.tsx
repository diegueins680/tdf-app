import { useState } from 'react';
import { Box, styled, type SxProps, type Theme } from '@mui/material';
import { artistPlaceholderImage, getArtistHeroImage, isArtistPlaceholderImage } from '../utils/artistFallbacks';

const Image = styled('img')({});

interface ArtistProfileImageProps {
  artistName: string;
  imageUrl?: string | null;
  slug?: string | null;
  alt?: string;
  width?: number;
  height?: number;
  eager?: boolean;
  avifSourceSet?: string | null;
  webpSourceSet?: string | null;
  sizes?: string;
  objectPosition?: string;
  sx?: SxProps<Theme>;
}

/** One bounded image attempt per source version, then a local initials avatar.
 * A retired image's error cannot invalidate its replacement. No availability or
 * identity verdict is persisted by this presentation-only fallback.
 */
export default function ArtistProfileImage({ artistName, imageUrl, slug, alt,
  width = 256, height = 256, eager = false, avifSourceSet, webpSourceSet,
  sizes, objectPosition = 'center', sx }: ArtistProfileImageProps) {
  const resolved = getArtistHeroImage(imageUrl, slug, artistName);
  const sourceKey = JSON.stringify([resolved, avifSourceSet, webpSourceSet]);
  const [failedSource, setFailedSource] = useState<string | null>(null);
  const placeholder = isArtistPlaceholderImage(resolved) || failedSource === sourceKey;
  return (
    <Box component="picture" sx={[
      { display: 'block', width, height, overflow: 'hidden', flexShrink: 0 },
      ...(Array.isArray(sx) ? sx : [sx]),
    ]}>
      {!placeholder && avifSourceSet && <source type="image/avif" srcSet={avifSourceSet} sizes={sizes} />}
      {!placeholder && webpSourceSet && <source type="image/webp" srcSet={webpSourceSet} sizes={sizes} />}
      <Image
        key={sourceKey}
        src={placeholder ? artistPlaceholderImage(artistName) : resolved}
        alt={alt === '' ? '' : placeholder ? `Avatar provisional de ${artistName}` : alt ?? artistName}
        width={width}
        height={height}
        loading={eager ? 'eager' : 'lazy'}
        decoding="async"
        data-artist-image={placeholder ? 'initials' : 'photo'}
        onError={() => { if (!placeholder) setFailedSource(sourceKey); }}
        sx={{ display: 'block', width: '100%', height: '100%', objectFit: placeholder ? 'contain' : 'cover', objectPosition }}
      />
    </Box>
  );
}
