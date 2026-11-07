import { createTheme, type PaletteMode } from '@mui/material';

/**
 * Builds the app's MUI theme for a palette mode. Exported so self-contained
 * dark surfaces (e.g. public landing shells) can nest a ThemeProvider with the
 * same brand tokens instead of recoloring individual descendants by hand.
 */
export function createAppTheme(mode: PaletteMode) {
  return createTheme({
    palette: {
      mode,
      // Keep the brighter brand hues as `light`, while using AA-safe
      // action shades whenever MUI places normal-size white text on top.
      primary: mode === 'light'
        ? { main: '#6d28d9', light: '#7c3aed', dark: '#5b21b6', contrastText: '#ffffff' }
        : { main: '#c4b5fd', light: '#ddd6fe', dark: '#a78bfa', contrastText: '#17111d' },
      secondary: mode === 'light'
        ? { main: '#be123c', light: '#e11d48', dark: '#9f1239', contrastText: '#ffffff' }
        : { main: '#fda4af', light: '#fecdd3', dark: '#fb7185', contrastText: '#1f1115' },
      info: mode === 'light'
        ? { main: '#01579b', light: '#03a9f4', dark: '#003c6d', contrastText: '#ffffff' }
        : { main: '#4fc3f7', light: '#81d4fa', dark: '#29b6f6', contrastText: '#071923' },
      warning: mode === 'light'
        ? { main: '#9a4600', light: '#ed6c02', dark: '#783500', contrastText: '#ffffff' }
        : { main: '#ffb74d', light: '#ffcc80', dark: '#ffa726', contrastText: '#211100' },
      background: {
        default: mode === 'light' ? '#f8f7f5' : '#0a0a0f',
        paper: mode === 'light' ? '#ffffff' : '#12121a',
      },
      text: {
        primary: mode === 'light' ? '#111113' : '#f4f4f5',
        secondary: mode === 'light' ? '#595963' : '#a1a1aa',
      },
      divider: mode === 'light' ? 'rgba(0,0,0,0.10)' : 'rgba(255,255,255,0.10)',
    },
    shape: { borderRadius: 8 },
    typography: {
      fontFamily: '"Inter", system-ui, -apple-system, sans-serif',
      h1: { fontSize: '2.5rem', fontWeight: 800, lineHeight: 1.2 },
      h2: { fontSize: '2rem', fontWeight: 700, lineHeight: 1.3 },
      h3: { fontSize: '1.75rem', fontWeight: 700, lineHeight: 1.2, letterSpacing: '-0.02em' },
      h4: { fontSize: '1.25rem', fontWeight: 600, lineHeight: 1.3 },
      h5: { fontSize: '1rem', fontWeight: 600, lineHeight: 1.4 },
      h6: { fontSize: '0.875rem', fontWeight: 600, lineHeight: 1.4 },
      body1: { fontSize: '0.9375rem', lineHeight: 1.5 },
      body2: { fontSize: '0.875rem', lineHeight: 1.5 },
      caption: {
        fontSize: '0.75rem',
        letterSpacing: '0.06em',
        textTransform: 'uppercase',
        fontWeight: 600,
        lineHeight: 1.4,
      },
      button: { textTransform: 'none', fontWeight: 600, letterSpacing: '0.01em' },
    },
    components: {
      MuiAlert: {
        styleOverrides: {
          root: { '@media (max-width: 599.95px)': { flexWrap: 'wrap' } },
          message: {
            minWidth: 0,
            overflowWrap: 'anywhere',
            '@media (max-width: 599.95px)': { flex: '1 1 calc(100% - 44px)' },
          },
          action: {
            '@media (max-width: 599.95px)': {
              flexBasis: '100%', marginLeft: 0, marginRight: 0, paddingLeft: 34, paddingTop: 0,
            },
          },
        },
      },
      MuiPaper: {
        styleOverrides: {
          root: {
            borderRadius: 12,
            backgroundImage: 'none',
            boxShadow:
              mode === 'light'
                ? '0 1px 3px rgba(0,0,0,0.04), 0 1px 2px rgba(0,0,0,0.02)'
                : '0 1px 3px rgba(0,0,0,0.2), 0 1px 2px rgba(0,0,0,0.12)',
            border: '1px solid',
            borderColor: mode === 'light' ? 'rgba(0,0,0,0.04)' : 'rgba(255,255,255,0.04)',
          },
        },
      },
      MuiButton: {
        styleOverrides: {
          root: {
            borderRadius: 8,
            transition: 'all 0.15s ease',
          },
          containedPrimary: {
            backgroundColor: mode === 'light' ? '#6d28d9' : '#c4b5fd',
            color: mode === 'light' ? '#ffffff' : '#17111d',
            '&:hover': { backgroundColor: mode === 'light' ? '#5b21b6' : '#a78bfa' },
          },
          containedSecondary: {
            backgroundColor: mode === 'light' ? '#be123c' : '#fda4af',
            color: mode === 'light' ? '#ffffff' : '#1f1115',
            '&:hover': { backgroundColor: mode === 'light' ? '#9f1239' : '#fb7185' },
          },
        },
      },
      MuiCard: {
        styleOverrides: {
          root: {
            borderRadius: 12,
          },
        },
      },
      MuiOutlinedInput: {
        styleOverrides: {
          root: {
            borderRadius: 8,
            transition: 'box-shadow 0.15s ease',
            '&:hover .MuiOutlinedInput-notchedOutline': {
              borderColor: mode === 'light' ? 'rgba(0,0,0,0.54)' : 'rgba(255,255,255,0.54)',
            },
            '&.Mui-focused .MuiOutlinedInput-notchedOutline': {
              borderWidth: 2,
            },
          },
        },
      },
      MuiChip: {
        styleOverrides: {
          root: { borderRadius: 6, fontWeight: 600, fontSize: '0.75rem' },
        },
      },
      MuiAvatar: {
        styleOverrides: {
          root: { borderRadius: 10 },
        },
      },
      MuiListItemButton: {
        styleOverrides: {
          root: {
            borderRadius: 8,
            transition: 'background-color 0.15s ease',
          },
        },
      },
      MuiAppBar: {
        styleOverrides: {
          root: {
            backgroundImage: 'none',
            boxShadow: 'none',
          },
        },
      },
      MuiCssBaseline: {
        styleOverrides: {
          '*': {
            scrollbarWidth: 'thin',
            scrollbarColor:
              mode === 'light' ? 'rgba(0,0,0,0.15) transparent' : 'rgba(255,255,255,0.15) transparent',
          },
          '::-webkit-scrollbar': { width: '6px', height: '6px' },
          '::-webkit-scrollbar-track': { background: 'transparent' },
          '::-webkit-scrollbar-thumb': {
            backgroundColor: mode === 'light' ? 'rgba(0,0,0,0.15)' : 'rgba(255,255,255,0.15)',
            borderRadius: '999px',
          },
          ':focus-visible': {
            outline: `3px solid ${mode === 'light' ? '#6d28d9' : '#a78bfa'}`,
            outlineOffset: 2,
          },
          '@media (prefers-reduced-motion: reduce)': {
            '*, *::before, *::after': {
              animationDuration: '0.01ms !important',
              animationIterationCount: '1 !important',
              scrollBehavior: 'auto !important',
              transitionDuration: '0.01ms !important',
            },
          },
        },
      },
    },
  });
}
