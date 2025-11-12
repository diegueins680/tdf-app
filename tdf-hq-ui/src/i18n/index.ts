import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';

void i18n.use(initReactI18next).init({
  lng: 'es',
  fallbackLng: 'en',
  interpolation: { escapeValue: false },
  resources: {
    es: { translation: { Parties: 'Personas', Bookings: 'Agenda', Kanban: 'Pipelines' } },
    en: { translation: { Parties: 'Parties', Bookings: 'Bookings', Kanban: 'Pipelines' } },
  },
});

export default i18n;
