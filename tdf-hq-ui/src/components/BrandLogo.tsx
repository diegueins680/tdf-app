import Box from '@mui/material/Box';
import type { SxProps, Theme } from '@mui/material/styles';
import glyphLogo from '../assets/tdf-isotype.svg';
import altLogo from '../assets/tdf-logo-alt.svg';
import wordmarkLogo from '../assets/tdf-logo-wordmark.svg';

type LogoVariant = 'glyph' | 'alt' | 'wordmark';

const LOGO_MAP: Record<LogoVariant, string> = {
  glyph: glyphLogo,
  alt: altLogo,
  wordmark: wordmarkLogo,
};

export interface BrandLogoProps {
  variant?: LogoVariant;
  size?: number;
  sx?: SxProps<Theme>;
  className?: string;
  'aria-label'?: string;
}

export function BrandLogo({ variant = 'glyph', size = 32, sx, className, ...rest }: BrandLogoProps) {
  const src = LOGO_MAP[variant];

  return (
    <Box
      component="img"
      src={src}
      alt="TDF Records logo"
      sx={{
        display: 'block',
        height: size,
        width: 'auto',
        pointerEvents: 'none',
        ...sx,
      }}
      className={className}
      {...rest}
    />
  );
}

export default BrandLogo;
