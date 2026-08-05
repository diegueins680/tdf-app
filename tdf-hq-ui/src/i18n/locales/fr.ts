const fr = {
  common: { save: 'Enregistrer', saving: 'Enregistrement…', saved: 'Préférences enregistrées.', error: 'Une erreur est survenue.' },
  sessionMenu: { noRoles: 'Aucun rôle attribué', logout: 'Se déconnecter', open: 'Ouvrir le menu de session' },
  preferences: {
    title: 'Langue et région', subtitle: "Choisissez l'affichage de la langue, de la monnaie, des dates et des heures.",
    language: 'Langue', currency: 'Devise', timezone: 'Fuseau horaire', country: 'Code pays',
    countryHint: 'Code ISO facultatif à deux lettres, par exemple FR ou CA.',
  },
  system: { title: 'État du système', subtitle: 'Version, état et métadonnées du backend.', application: 'Application', version: 'Version', status: 'État', built: 'Compilé', codebase: 'Code source' },
} as const;

export default fr;
