const es = {
  Parties: 'Personas',
  Bookings: 'Agenda',
  Kanban: 'Pipelines',
  common: {
    save: 'Guardar',
    saving: 'Guardando…',
    saved: 'Preferencias guardadas.',
    error: 'Ocurrió un error.',
  },
  sessionMenu: {
    noRoles: 'Sin roles asignados',
    logout: 'Cerrar sesión',
    open: 'Abrir menú de sesión',
  },
  preferences: {
    title: 'Idioma y región',
    subtitle: 'Elige cómo se muestran el idioma, el dinero, las fechas y las horas.',
    language: 'Idioma',
    currency: 'Moneda',
    timezone: 'Zona horaria',
    country: 'Código de país',
    countryHint: 'Código ISO opcional de dos letras, por ejemplo EC o ES.',
  },
  system: {
    title: 'Estado del sistema',
    subtitle: 'Versión, salud y metadatos del backend.',
    application: 'Aplicación',
    version: 'Versión',
    status: 'Estado',
    built: 'Compilado',
    codebase: 'Código base',
  },
  notifications: {
    title: 'Notificaciones',
    markAll: 'Leer todo',
    markAllLoading: 'Marcando…',
    loading: 'Cargando notificaciones',
    empty: 'Sin notificaciones',
  },
  login: {
    resetDialog: { title: 'Recuperar acceso' },
    signupDialog: { title: 'Crear cuenta' },
  },
  tdfPlatform: {
    cta: {
      createAccount: 'Crear cuenta',
      fanProfile: 'Perfil fan',
      artistProfile: 'Perfil artista',
      createFanProfile: 'Crear perfil fan',
      createArtistProfile: 'Crear perfil de artista',
      viewArtistProfile: 'Ver perfil',
      explore: 'Explorar',
      reserveExperience: 'Reservar experiencia',
      viewLocation: 'Ver ubicación',
      viewReleases: 'Ver lanzamientos',
    },
    empty: {
      artists: 'El carrusel se llenará automáticamente cuando existan artistas publicados en la plataforma.',
      services: 'Pronto verás nuevas rutas TDF en este espacio.',
      fanBenefits: 'Pronto agregaremos beneficios para fans.',
      artistBenefits: 'Pronto agregaremos beneficios para artistas.',
    },
    sections: { startEyebrow: 'Empieza por tu cuenta' },
  },
} as const;

export default es;
