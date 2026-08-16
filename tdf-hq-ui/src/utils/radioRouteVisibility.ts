const HIDDEN_RADIO_PATH_PREFIXES = [
  '/login',
  '/reservar',
  '/dj-booth',
  '/marketplace',
  '/inscripcion',
  '/configuracion/inscripciones-curso',
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
  HIDDEN_RADIO_PATH_PREFIXES.some((prefix) => pathname.startsWith(prefix))
  || (
    RADIO_ON_DEMAND_PATH_PREFIXES.some((prefix) => pathname.startsWith(prefix))
    && hash !== '#radio'
  )
);

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
