import { createContext, useContext, useEffect, useMemo, useState, type ReactNode } from 'react';
import { CssBaseline, ThemeProvider, createTheme, type PaletteMode } from '@mui/material';

export type ThemeModePreference = PaletteMode | 'system';

interface ThemeModeContextValue {
  /** Resolved palette mode currently rendered by MUI. */
  mode: PaletteMode;
  /** Persisted user choice. `system` follows the OS in real time. */
  preference: ThemeModePreference;
  toggleMode: () => void;
  setMode: (mode: ThemeModePreference) => void;
}

const ThemeModeContext = createContext<ThemeModeContextValue | undefined>(undefined);
const STORAGE_KEY = 'tdf-hq-ui/theme-mode';

function readStoredMode(): ThemeModePreference {
  if (typeof window === 'undefined') return 'system';
  const stored = window.localStorage.getItem(STORAGE_KEY);
  if (stored === 'light' || stored === 'dark' || stored === 'system') return stored;
  return 'system';
}

function readSystemMode(): PaletteMode {
  if (typeof window === 'undefined') return 'light';
  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

interface AppThemeProviderProps {
  children: ReactNode;
}

export function AppThemeProvider({ children }: AppThemeProviderProps) {
  const [preference, setPreference] = useState<ThemeModePreference>(() => readStoredMode());
  const [systemMode, setSystemMode] = useState<PaletteMode>(() => readSystemMode());
  const mode = preference === 'system' ? systemMode : preference;

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem(STORAGE_KEY, preference);
  }, [preference]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    const media = window.matchMedia('(prefers-color-scheme: dark)');
    const updateSystemMode = (event: MediaQueryListEvent | MediaQueryList) => {
      setSystemMode(event.matches ? 'dark' : 'light');
    };
    updateSystemMode(media);
    media.addEventListener('change', updateSystemMode);
    return () => media.removeEventListener('change', updateSystemMode);
  }, []);

  useEffect(() => {
    if (typeof document === 'undefined') return;
    document.documentElement.style.colorScheme = mode;
    document.documentElement.dataset['theme'] = mode;
  }, [mode]);

  const theme = useMemo(
    () =>
      createTheme({
        palette: {
          mode,
          // Keep the brighter brand hues as `light`, while using AA-safe
          // action shades whenever MUI places normal-size white text on top.
          primary: { main: '#7c3aed', light: '#8b5cf6', dark: '#6d28d9', contrastText: '#ffffff' },
          secondary: { main: '#e11d48', light: '#f43f5e', dark: '#be123c', contrastText: '#ffffff' },
          background: {
            default: mode === 'light' ? '#f8f7f5' : '#0a0a0f',
            paper: mode === 'light' ? '#ffffff' : '#12121a',
          },
          text: {
            primary: mode === 'light' ? '#111113' : '#f4f4f5',
            secondary: mode === 'light' ? '#6b6b74' : '#a1a1aa',
          },
          divider: mode === 'light' ? 'rgba(0,0,0,0.06)' : 'rgba(255,255,255,0.06)',
        },
        shape: { borderRadius: 8 },
        typography: {
          fontFamily: '"Inter", system-ui, -apple-system, sans-serif',
          h3: { fontSize: '1.75rem', fontWeight: 700, lineHeight: 1.2, letterSpacing: '-0.02em' },
          h4: { fontSize: '1.25rem', fontWeight: 600, lineHeight: 1.3 },
          h5: { fontSize: '1rem', fontWeight: 600, lineHeight: 1.4 },
          h6: { fontSize: '0.875rem', fontWeight: 600, lineHeight: 1.4 },
          body1: { fontSize: '0.9375rem', lineHeight: 1.5 },
          body2: { fontSize: '0.875rem', lineHeight: 1.5 },
          caption: {
            fontSize: '0.6875rem',
            letterSpacing: '0.06em',
            textTransform: 'uppercase',
            fontWeight: 600,
            lineHeight: 1.4,
          },
          button: { textTransform: 'none', fontWeight: 600, letterSpacing: '0.01em' },
        },
        components: {
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
                '&:hover': {
                  transform: 'translateY(-1px)',
                  boxShadow: '0 4px 12px rgba(0,0,0,0.08)',
                },
              },
              containedPrimary: {
                backgroundColor: '#7c3aed',
                '&:hover': { backgroundColor: '#6d28d9' },
              },
              containedSecondary: {
                backgroundColor: '#e11d48',
                '&:hover': { backgroundColor: '#be123c' },
              },
            },
          },
          MuiCard: {
            styleOverrides: {
              root: {
                borderRadius: 12,
                transition: 'transform 0.2s ease, box-shadow 0.2s ease',
                '&:hover': {
                  transform: 'translateY(-2px)',
                  boxShadow:
                    mode === 'light'
                      ? '0 8px 24px rgba(0,0,0,0.06)'
                      : '0 8px 24px rgba(0,0,0,0.25)',
                },
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
      }),
    [mode],
  );

  const value = useMemo<ThemeModeContextValue>(
    () => ({
      mode,
      preference,
      toggleMode: () => setPreference(mode === 'light' ? 'dark' : 'light'),
      setMode: setPreference,
    }),
    [mode, preference],
  );

  return (
    <ThemeModeContext.Provider value={value}>
      <ThemeProvider theme={theme}>
        <CssBaseline />
        {children}
      </ThemeProvider>
    </ThemeModeContext.Provider>
  );
}

export function useThemeMode(): ThemeModeContextValue {
  const context = useContext(ThemeModeContext);
  if (!context) {
    throw new Error('useThemeMode must be used within AppThemeProvider');
  }
  return context;
}
