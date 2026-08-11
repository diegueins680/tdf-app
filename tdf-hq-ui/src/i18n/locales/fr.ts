const fr = {
  common: { save: 'Enregistrer', saving: 'Enregistrement…', saved: 'Préférences enregistrées.', error: 'Une erreur est survenue.' },
  internships: { taskDetail: { instructions: 'Instructions' } },
  pagination: {
    items: 'éléments', rowsPerPage: 'Par page', loading: 'Chargement des résultats…', updating: 'Mise à jour des résultats…',
    displayed: '{{from}}–{{to}} sur {{count}} {{itemLabel}}', displayedMoreThan: '{{from}}–{{to}} sur plus de {{count}} {{itemLabel}}',
    firstPage: 'Aller à la première page', lastPage: 'Aller à la dernière page',
    nextPage: 'Aller à la page suivante', previousPage: 'Aller à la page précédente',
  },
  promoCode: {
    label: 'Code promotionnel (facultatif)', placeholder: 'SAISISSEZ-LE-CODE', clear: 'Effacer le code promotionnel',
    inactive: "Le code promotionnel n'est pas actif", invalid: 'Code promotionnel invalide', checking: 'Vérification du code promotionnel…',
    validUntil: "Valable jusqu'au", percentOff: '{{value}} % de réduction', fixedOff: '{{value}} de réduction',
    usesRemaining_one: '{{count}} utilisation restante', usesRemaining_other: '{{count}} utilisations restantes',
  },
  refunds: {
    loading: 'Chargement des demandes de remboursement', loadError: "Impossible de charger les demandes de remboursement.",
    pendingSummary_one: '{{count}} demande de remboursement en attente de validation',
    pendingSummary_other: '{{count}} demandes de remboursement en attente de validation', itemLabel: 'remboursements',
    orderId: 'ID de commande', amount: 'Montant', reason: 'Motif', status: 'Statut', requested: 'Demandé', actions: 'Actions',
    empty: 'Aucune demande de remboursement', approve: 'Approuver', reject: 'Refuser', rejected: 'Refusé : {{reason}}',
    confirmApproval: 'Approuver le remboursement de {{amount}} ?', rejectDialogTitle: 'Refuser la demande de remboursement',
    rejectionReason: 'Motif du refus', rejectionPlaceholder: 'Expliquez pourquoi cette demande de remboursement est refusée…',
    cancel: 'Annuler', rejectRefund: 'Refuser le remboursement',
    statuses: { pending: 'En attente', approved: 'Approuvé', rejected: 'Refusé', processed: 'Traité' },
  },
  artistFans: {
    loading: 'Chargement des fans…', loadError: 'Impossible de charger les fans.',
    empty: "Cet artiste n'a pas encore de fans.", followedSince: 'Fan depuis le {{date}}',
  },
  partyRelated: {
    unavailable: 'Contact indisponible', contact: 'Contact', subtitle: "Navigation rapide dans l'historique associé",
    organization: 'Organisation', user: 'Utilisateur', profile: 'Profil', customerBookings: 'Réservations (client)',
    engineerBookings: 'Réservations (ingénieur)', studentClasses: 'Cours (élève)', teacherClasses: 'Cours (professeur)',
    tracks: 'Titres', bookings: 'Réservations', customer: 'Client', engineer: 'Ingénieur', classes: 'Cours',
    student: 'Élève', teacher: 'Professeur', bookingFallback: 'Réservation n° {{id}}', subjectFallback: 'Matière n° {{id}}',
    studentFallback: 'Élève n° {{id}}', updated: 'Mis à jour : {{date}}', loading: "Chargement de l'historique associé",
    loadError: "Impossible de charger l'historique associé.",
    empty: "Il n'y a pas encore d'historique associé. Utilisez Profil pour consulter ou compléter ce contact.",
    bookingItems: 'réservations', classItems: 'cours', trackItems: 'titres',
    statuses: { confirmed: 'Confirmée', confirmada: 'Confirmée', scheduled: 'Planifiée', programada: 'Planifiée', mix: 'Mixage' },
  },
  sessionMenu: { noRoles: 'Aucun rôle attribué', logout: 'Se déconnecter', open: 'Ouvrir le menu de session' },
  preferences: {
    title: 'Langue et région', subtitle: "Choisissez l'affichage de la langue, de la monnaie, des dates et des heures.",
    language: 'Langue', currency: 'Devise', timezone: 'Fuseau horaire', country: 'Code pays',
    countryHint: 'Code ISO facultatif à deux lettres, par exemple FR ou CA.',
  },
  system: { title: 'État du système', subtitle: 'Version, état et métadonnées du backend.', application: 'Application', version: 'Version', status: 'État', built: 'Compilé', codebase: 'Code source' },
} as const;

export default fr;
