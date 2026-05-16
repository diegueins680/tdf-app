import {
  CssBaseline,
  ThemeProvider,
  createTheme,
  type PaletteMode,
} from '@mui/material'
import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
  type PropsWithChildren,
} from 'react'

type ColorMode = PaletteMode

type ColorModeContextValue = {
  mode: ColorMode
  toggle: () => void
  setMode: (mode: ColorMode) => void
}

const STORAGE_KEY = 'tdf-ui-color-scheme'

const ColorModeContext = createContext<ColorModeContextValue | undefined>(undefined)

function readStoredMode(): ColorMode | null {
  if (typeof window === 'undefined') return null
  try {
    const stored = window.localStorage.getItem(STORAGE_KEY)
    if (stored === 'light' || stored === 'dark') {
      return stored
    }
  } catch {
    // ignore storage errors
  }
  return null
}

function getSystemMode(): ColorMode {
  if (typeof window === 'undefined' || typeof window.matchMedia !== 'function') {
    return 'light'
  }
  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
}

function syncHtmlClass(mode: ColorMode) {
  if (typeof document === 'undefined') return
  const root = document.documentElement
  root.classList.toggle('theme-dark', mode === 'dark')
  root.classList.toggle('theme-light', mode === 'light')
}

function buildTheme(mode: ColorMode) {
  return createTheme({
    palette: {
      mode,
      primary: { main: '#1f6feb' },
      secondary: { main: '#ff6b4a' },
      background: {
        default: mode === 'dark' ? '#111827' : '#f7f8fa',
        paper: mode === 'dark' ? '#1f2937' : '#ffffff',
      },
    },
    typography: {
      fontFamily: 'Inter, system-ui, -apple-system, "Segoe UI", Roboto, Arial, sans-serif',
      h5: { fontWeight: 700 },
    },
    shape: { borderRadius: 10 },
    components: {
      MuiPaper: { styleOverrides: { root: { borderRadius: 12 } } },
      MuiButton: { styleOverrides: { root: { textTransform: 'none', fontWeight: 600 } } },
    },
  })
}

export function ColorModeProvider({ children }: PropsWithChildren) {
  const stored = useMemo(() => readStoredMode(), [])
  const [mode, setMode] = useState<ColorMode>(stored ?? getSystemMode())
  const hasExplicitChoice = useRef<boolean>(stored !== null)

  useEffect(() => {
    syncHtmlClass(mode)
  }, [mode])

  useEffect(() => {
    if (typeof window === 'undefined' || typeof window.matchMedia !== 'function') return
    const mql = window.matchMedia('(prefers-color-scheme: dark)')
    const listener = (event: MediaQueryListEvent) => {
      if (hasExplicitChoice.current) return
      setMode(event.matches ? 'dark' : 'light')
    }

    if (typeof mql.addEventListener === 'function') {
      mql.addEventListener('change', listener)
      return () => mql.removeEventListener('change', listener)
    }
    if (typeof mql.addListener === 'function') {
      mql.addListener(listener)
      return () => mql.removeListener(listener)
    }
    return undefined
  }, [])

  useEffect(() => {
    if (!hasExplicitChoice.current) return
    if (typeof window === 'undefined') return
    try {
      window.localStorage.setItem(STORAGE_KEY, mode)
    } catch {
      // ignore storage errors
    }
  }, [mode])

  const setModeExplicit = useCallback((next: ColorMode) => {
    hasExplicitChoice.current = true
    setMode(next)
  }, [])

  const toggle = useCallback(() => {
    setModeExplicit(mode === 'light' ? 'dark' : 'light')
  }, [mode, setModeExplicit])

  const contextValue = useMemo<ColorModeContextValue>(() => ({
    mode,
    toggle,
    setMode: setModeExplicit,
  }), [mode, toggle, setModeExplicit])

  const theme = useMemo(() => buildTheme(mode), [mode])

  return (
    <ColorModeContext.Provider value={contextValue}>
      <ThemeProvider theme={theme}>
        <CssBaseline />
        {children}
      </ThemeProvider>
    </ColorModeContext.Provider>
  )
}

export function useColorMode() {
  const context = useContext(ColorModeContext)
  if (!context) {
    throw new Error('useColorMode must be used within a ColorModeProvider')
  }
  return context
}
