import { createContext, useContext, useEffect, useMemo, useState, type ReactNode } from 'react';
import { CssBaseline, ThemeProvider, createTheme, type PaletteMode } from '@mui/material';

type ThemeModeContextValue = {
  mode: PaletteMode;
  toggleMode: () => void;
  setMode: (mode: PaletteMode) => void;
};

const ThemeModeContext = createContext<ThemeModeContextValue | undefined>(undefined);
const STORAGE_KEY = 'tdf-hq-ui/theme-mode';

function readStoredMode(): PaletteMode {
  if (typeof window === 'undefined') {
    return 'dark';
  }
  const stored = window.localStorage.getItem(STORAGE_KEY);
  if (stored === 'light' || stored === 'dark') return stored;
  return 'dark';
}

export function AppThemeProvider({ children }: { children: ReactNode }) {
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
          primary: { main: '#4c8bf5' },
          secondary: { main: '#6366f1' },
          background: {
            default: mode === 'light' ? '#f5f6fa' : '#1c1f2b',
            paper: mode === 'light' ? '#ffffff' : '#232633',
          },
          text: {
            primary: mode === 'light' ? '#0f172a' : '#f8fafc',
            secondary: mode === 'light' ? '#475569' : '#cbd5f5',
          },
        },
        shape: { borderRadius: 20 },
        typography: {
          fontFamily: '"Inter", "Segoe UI", sans-serif',
          h5: { fontWeight: 600 },
          h4: { fontWeight: 600 },
        },
        components: {
          MuiPaper: {
            styleOverrides: {
              root: {
                borderRadius: 24,
                border: '1px solid rgba(148,163,184,0.15)',
                backgroundImage: 'none',
              },
            },
          },
          MuiButton: {
            styleOverrides: {
              root: { borderRadius: 999 },
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
