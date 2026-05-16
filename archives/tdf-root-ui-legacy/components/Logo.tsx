import * as React from 'react';
import { useColorMode } from '../theme/ColorModeProvider';

const THEMED_SOURCES = {
  light: '/assets/tdf-ui/tdf_logo_black.svg',
  dark: '/assets/tdf-ui/tdf_logo_white.svg',
} as const;

const FALLBACK_CANDIDATES = [
  '/logo_tdf_records.svg',
  '/tdf-logo.svg',
  '/logo-tdf-records.svg',
  '/logo.svg',
];

type LogoTone = keyof typeof THEMED_SOURCES | 'auto';

type LogoProps = React.ImgHTMLAttributes<HTMLImageElement> & {
  tone?: LogoTone;
};

export function Logo({ style, alt, onError, tone = 'auto', ...props }: LogoProps) {
  const { mode } = useColorMode();
  const desiredTone = tone === 'auto' ? mode : tone;
  const themedSource = THEMED_SOURCES[desiredTone] ?? THEMED_SOURCES.light;
  const candidates = React.useMemo(() => {
    const seen = new Set<string>();
    return [themedSource, THEMED_SOURCES.dark, THEMED_SOURCES.light, ...FALLBACK_CANDIDATES].filter((candidate) => {
      if (!candidate || seen.has(candidate)) {
        return false;
      }
      seen.add(candidate);
      return true;
    });
  }, [themedSource]);

  const [index, setIndex] = React.useState(0);

  React.useEffect(() => {
    setIndex(0);
  }, [themedSource]);

  const src = candidates[index] ?? themedSource;

  return (
    <img
      {...props}
      alt={alt ?? 'TDF Records'}
      src={src}
      onError={(event) => {
        setIndex((i) => Math.min(i + 1, Math.max(candidates.length - 1, 0)));
        onError?.(event);
      }}
      style={{ maxHeight: 80, objectFit: 'contain', ...(style || {}) }}
    />
  );
}
