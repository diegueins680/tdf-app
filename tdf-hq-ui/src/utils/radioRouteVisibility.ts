const HIDDEN_RADIO_PATH_PREFIXES = [
  '/login',
  '/reservar',
  '/marketplace',
  '/inscripcion',
  '/configuracion/inscripciones-curso',
  '/mi-artista/campanas',
  '/whatsapp/consentimiento',
  '/whatsapp/ok',
];

const RADIO_ON_DEMAND_PATH_PREFIXES = [
  '/tdf',
  '/inicio',
  '/fans',
  '/records',
];

export const shouldHideRadioForRoute = (pathname: string, hash = '') => (
  pathname === '/musica'
  || pathname.startsWith('/musica/')
  || HIDDEN_RADIO_PATH_PREFIXES.some((prefix) => pathname.startsWith(prefix))
  || (
    RADIO_ON_DEMAND_PATH_PREFIXES.some((prefix) => pathname.startsWith(prefix))
    && hash !== '#radio'
  )
);

// Initial discovery policy. Once opened, RoutedRadioWidget retains playback
// across these routes until the authenticated session ends.
export const shouldRenderRadioWidget = (
  pathname: string,
  hash: string,
  authenticated: boolean,
  sessionLoading: boolean,
) => (
  authenticated
  && !sessionLoading
  && !shouldHideRadioForRoute(pathname, hash)
);
