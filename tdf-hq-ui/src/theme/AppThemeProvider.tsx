import { createContext, useCallback, useContext, useEffect, useMemo, useState, type ReactNode } from 'react';
import { CssBaseline, ThemeProvider, createTheme, type PaletteMode } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Catalogs, type CatalogItem } from '../api/catalogs';

export type ThemeModePreference = PaletteMode | 'system';

interface ThemeModeContextValue {
  /** Resolved palette mode currently rendered by MUI. */
  mode: PaletteMode;
  /** Persisted user choice. `system` follows the OS in real time. */
  preference: ThemeModePreference;
  preferenceId: string;
  options: readonly ThemeModeOption[];
  catalogSource: 'network' | 'emergency';
  catalogLoading: boolean;
  catalogError: boolean;
  toggleMode: () => void;
  setModeById: (modeId: string) => void;
}

export interface ThemeModeOption {
  id: string;
  code: ThemeModePreference;
  label: string;
}

interface StoredThemeSelection {
  id: string | null;
  code: ThemeModePreference;
}

const ThemeModeContext = createContext<ThemeModeContextValue | undefined>(undefined);
const STORAGE_KEY = 'tdf-hq-ui/theme-mode';
const EMERGENCY_THEME_OPTIONS: readonly ThemeModeOption[] = [
  { id: 'emergency:appearance-modes:system', code: 'system', label: 'Usar configuración del sistema' },
  { id: 'emergency:appearance-modes:light', code: 'light', label: 'Tema claro' },
  { id: 'emergency:appearance-modes:dark', code: 'dark', label: 'Tema oscuro' },
];

function isThemeModePreference(value: unknown): value is ThemeModePreference {
  return value === 'light' || value === 'dark' || value === 'system';
}

export function readStoredMode(): StoredThemeSelection {
  if (typeof window === 'undefined') return { id: null, code: 'system' };
  const stored = window.localStorage.getItem(STORAGE_KEY);
  if (isThemeModePreference(stored)) return { id: null, code: stored };
  if (stored) {
    try {
      const parsed = JSON.parse(stored) as unknown;
      if (parsed && typeof parsed === 'object') {
        const value = parsed as Record<string, unknown>;
        if (isThemeModePreference(value['code'])) {
          return {
            id: typeof value['id'] === 'string' && value['id'] ? value['id'] : null,
            code: value['code'],
          };
        }
      }
    } catch {
      // Invalid legacy storage falls through to the emergency default.
    }
  }
  return { id: null, code: 'system' };
}

function readSystemMode(): PaletteMode {
  if (typeof window === 'undefined') return 'light';
  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

interface AppThemeProviderProps {
  children: ReactNode;
}

export function AppThemeProvider({ children }: AppThemeProviderProps) {
  const { i18n } = useTranslation();
  const [selection, setSelection] = useState<StoredThemeSelection>(() => readStoredMode());
  const [systemMode, setSystemMode] = useState<PaletteMode>(() => readSystemMode());
  const catalogQuery = useQuery({
    queryKey: ['catalogs', 'appearance-modes', i18n.resolvedLanguage ?? i18n.language],
    queryFn: () => Catalogs.listPublicBatch(['appearance-modes'], {
      locale: i18n.resolvedLanguage ?? i18n.language,
      page: 1,
      pageSize: 50,
    }),
    staleTime: 1000 * 60 * 10,
  });
  const catalogPages = useMemo(
    () => Array.isArray(catalogQuery.data?.catalogs) ? catalogQuery.data.catalogs : [],
    [catalogQuery.data?.catalogs],
  );
  const appearancePage = useMemo(
    () => catalogPages.find((page) => page?.catalog?.code === 'appearance-modes'),
    [catalogPages],
  );
  const networkItems = useMemo(
    () => Array.isArray(appearancePage?.items)
      ? appearancePage.items.filter((item): item is CatalogItem & { code: ThemeModePreference } =>
        isThemeModePreference(item.code))
      : [],
    [appearancePage?.items],
  );
  const appearanceDefaults = useMemo(
    () => (Array.isArray(appearancePage?.defaults) ? appearancePage.defaults : []).filter(
      (entry) => entry.scopeKind === 'appearance-mode' && entry.scopeId === 'global' && !entry.localeId,
    ),
    [appearancePage?.defaults],
  );
  const validNetworkCatalog = Boolean(
    appearancePage
      && networkItems.length
      && networkItems.length === appearancePage.items.length
      && networkItems.every((item) => item.active && item.workflowState === 'published')
      && new Set(networkItems.map((item) => item.id)).size === networkItems.length
      && new Set(networkItems.map((item) => item.code)).size === networkItems.length
      && appearanceDefaults.length === 1
      && networkItems.some((item) => item.id === appearanceDefaults[0]?.entityId),
  );
  const options = useMemo<readonly ThemeModeOption[]>(
    () => validNetworkCatalog
      ? networkItems.map((item) => ({ id: item.id, code: item.code, label: item.name }))
      : EMERGENCY_THEME_OPTIONS,
    [networkItems, validNetworkCatalog],
  );
  const defaultEntityId = validNetworkCatalog
    ? appearanceDefaults[0]!.entityId
    : EMERGENCY_THEME_OPTIONS[0]!.id;
  const defaultOption = options.find((option) => option.id === defaultEntityId) ?? options[0]!;
  const preference = selection.code;
  const mode = preference === 'system' ? systemMode : preference;

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(selection));
  }, [selection]);

  useEffect(() => {
    const selected = options.find((option) => option.id === selection.id)
      ?? options.find((option) => option.code === selection.code)
      ?? defaultOption;
    if (selection.id !== selected.id || selection.code !== selected.code) {
      setSelection({ id: selected.id, code: selected.code });
    }
  }, [defaultOption, options, selection.code, selection.id]);

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
          primary: mode === 'light'
            ? { main: '#6d28d9', light: '#7c3aed', dark: '#5b21b6', contrastText: '#ffffff' }
            : { main: '#c4b5fd', light: '#ddd6fe', dark: '#a78bfa', contrastText: '#17111d' },
          secondary: mode === 'light'
            ? { main: '#be123c', light: '#e11d48', dark: '#9f1239', contrastText: '#ffffff' }
            : { main: '#fda4af', light: '#fecdd3', dark: '#fb7185', contrastText: '#1f1115' },
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
      }),
    [mode],
  );

  const setModeById = useCallback((modeId: string) => {
    const next = options.find((option) => option.id === modeId);
    if (next) setSelection({ id: next.id, code: next.code });
  }, [options]);

  const toggleMode = useCallback(() => {
    const targetCode: ThemeModePreference = mode === 'light' ? 'dark' : 'light';
    const next = options.find((option) => option.code === targetCode) ?? defaultOption;
    setSelection({ id: next.id, code: next.code });
  }, [defaultOption, mode, options]);

  const value = useMemo<ThemeModeContextValue>(
    () => ({
      mode,
      preference,
      preferenceId: selection.id ?? defaultOption.id,
      options,
      catalogSource: validNetworkCatalog ? 'network' : 'emergency',
      catalogLoading: catalogQuery.isLoading,
      catalogError: catalogQuery.isError || Boolean(appearancePage && !validNetworkCatalog),
      toggleMode,
      setModeById,
    }),
    [
      appearancePage,
      catalogQuery.isError,
      catalogQuery.isLoading,
      defaultOption.id,
      mode,
      options,
      preference,
      selection.id,
      setModeById,
      toggleMode,
      validNetworkCatalog,
    ],
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
