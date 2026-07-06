import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';

void i18n.use(initReactI18next).init({
  lng: 'es',
  fallbackLng: 'en',
  interpolation: { escapeValue: false },
  resources: {
    es: {
      translation: {
        Parties: 'Personas',
        Bookings: 'Agenda',
        Kanban: 'Pipelines',
        notifications: {
          title: 'Notificaciones',
          markAll: 'Leer todo',
          markAllLoading: 'Marcando...',
          loading: 'Cargando notificaciones',
          empty: 'Sin notificaciones',
        },
        login: {
          resetDialog: {
            title: 'Recuperar acceso',
          },
          signupDialog: {
            title: 'Crear cuenta',
          },
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
          sections: {
            startEyebrow: 'Empieza por tu cuenta',
          },
        },
      },
    },
    en: {
      translation: {
        Parties: 'Parties',
        Bookings: 'Bookings',
        Kanban: 'Pipelines',
        notifications: {
          title: 'Notifications',
          markAll: 'Mark all read',
          markAllLoading: 'Marking...',
          loading: 'Loading notifications',
          empty: 'No notifications',
        },
        login: {
          resetDialog: {
            title: 'Recover access',
          },
          signupDialog: {
            title: 'Create account',
          },
        },
        tdfPlatform: {
          cta: {
            createAccount: 'Create account',
            fanProfile: 'Fan profile',
            artistProfile: 'Artist profile',
            createFanProfile: 'Create fan profile',
            createArtistProfile: 'Create artist profile',
            viewArtistProfile: 'View profile',
            explore: 'Explore',
            reserveExperience: 'Reserve experience',
            viewLocation: 'View location',
            viewReleases: 'View releases',
          },
          empty: {
            artists: 'The carousel will fill automatically when artists are published on the platform.',
            services: 'New TDF routes will appear here soon.',
            fanBenefits: 'Fan benefits will appear here soon.',
            artistBenefits: 'Artist benefits will appear here soon.',
          },
          sections: {
            startEyebrow: 'Start on your own',
          },
        },
      },
    },
  },
});

export default i18n;
