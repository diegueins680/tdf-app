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
      },
    },
  },
});

export default i18n;
