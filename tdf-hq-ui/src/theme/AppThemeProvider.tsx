import { createContext, useContext, useEffect, useMemo, useState, type ReactNode } from 'react';
import { CssBaseline, ThemeProvider, createTheme, type PaletteMode } from '@mui/material';

interface ThemeModeContextValue {
  mode: PaletteMode;
  toggleMode: () => void;
  setMode: (mode: PaletteMode) => void;
}

const ThemeModeContext = createContext<ThemeModeContextValue | undefined>(undefined);
const STORAGE_KEY = 'tdf-hq-ui/theme-mode';

function readStoredMode(): PaletteMode {
  if (typeof window === 'undefined') {
    return 'light';
  }
  const stored = window.localStorage.getItem(STORAGE_KEY);
  if (stored === 'light' || stored === 'dark') return stored;
  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

interface AppThemeProviderProps {
  children: ReactNode;
}

export function AppThemeProvider({ children }: AppThemeProviderProps) {
  const [mode, setMode] = useState<PaletteMode>(() => readStoredMode());

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem(STORAGE_KEY, mode);
  }, [mode]);

  const theme = useMemo(
    () =>
      createTheme({
        palette: {
          mode,
          primary: { main: '#8b5cf6', contrastText: '#ffffff' },
          secondary: { main: '#f43f5e', contrastText: '#ffffff' },
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
                background: 'linear-gradient(135deg, #8b5cf6 0%, #7c3aed 100%)',
              },
              containedSecondary: {
                background: 'linear-gradient(135deg, #f43f5e 0%, #e11d48 100%)',
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
                  borderColor: mode === 'light' ? 'rgba(0,0,0,0.2)' : 'rgba(255,255,255,0.2)',
                },
                '&.Mui-focused .MuiOutlinedInput-notchedOutline': {
                  borderWidth: 1.5,
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
            },
          },
        },
      }),
    [mode],
  );

  const value = useMemo<ThemeModeContextValue>(
    () => ({
      mode,
      toggleMode: () => setMode((prev) => (prev === 'light' ? 'dark' : 'light')),
      setMode,
    }),
    [mode],
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
