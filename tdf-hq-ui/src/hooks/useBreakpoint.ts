import { useMediaQuery, useTheme } from '@mui/material';

export function useBreakpoint() {
  const theme = useTheme();
  const isMobile = useMediaQuery(theme.breakpoints.down('md'));
  const isTablet = useMediaQuery(theme.breakpoints.between('md', 'lg'));
  const isDesktop = useMediaQuery(theme.breakpoints.up('lg'));
  const isWide = useMediaQuery(theme.breakpoints.up('xl'));

  return { isMobile, isTablet, isDesktop, isWide };
}
