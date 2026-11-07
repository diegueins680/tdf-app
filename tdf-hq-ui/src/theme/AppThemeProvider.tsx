import { createContext, useCallback, useContext, useEffect, useMemo, useState, type ReactNode } from 'react';
import { CssBaseline, ThemeProvider, type PaletteMode } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Catalogs, type CatalogItem } from '../api/catalogs';
import { createAppTheme } from './appTheme';

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
  let stored: string | null;
  try {
    stored = window.localStorage.getItem(STORAGE_KEY);
  } catch {
    return { id: null, code: 'system' };
  }
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
    try {
      window.localStorage.setItem(STORAGE_KEY, JSON.stringify(selection));
    } catch {
      // Theme changes still work in memory when persistence is unavailable.
    }
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

  const theme = useMemo(() => createAppTheme(mode), [mode]);

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
