const de = {
  common: { save: 'Speichern', saving: 'Wird gespeichert…', saved: 'Einstellungen gespeichert.', error: 'Ein Fehler ist aufgetreten.' },
  pagination: {
    items: 'Einträge', rowsPerPage: 'Pro Seite', loading: 'Ergebnisse werden geladen…', updating: 'Ergebnisse werden aktualisiert…',
    displayed: '{{from}}–{{to}} von {{count}} {{itemLabel}}', displayedMoreThan: '{{from}}–{{to}} von mehr als {{count}} {{itemLabel}}',
    firstPage: 'Zur ersten Seite', lastPage: 'Zur letzten Seite', nextPage: 'Zur nächsten Seite', previousPage: 'Zur vorherigen Seite',
  },
  promoCode: {
    label: 'Aktionscode (optional)', placeholder: 'CODE-EINGEBEN', clear: 'Aktionscode löschen', inactive: 'Der Aktionscode ist nicht aktiv',
    invalid: 'Ungültiger Aktionscode', checking: 'Aktionscode wird geprüft…', validUntil: 'Gültig bis', percentOff: '{{value}} % Rabatt',
    fixedOff: '{{value}} Rabatt', usesRemaining_one: '{{count}} Verwendung verbleibend', usesRemaining_other: '{{count}} Verwendungen verbleibend',
  },
  refunds: {
    loading: 'Erstattungsanträge werden geladen', loadError: 'Erstattungsanträge konnten nicht geladen werden.',
    pendingSummary_one: '{{count}} Erstattungsantrag wartet auf Genehmigung',
    pendingSummary_other: '{{count}} Erstattungsanträge warten auf Genehmigung', itemLabel: 'Erstattungen',
    orderId: 'Bestell-ID', amount: 'Betrag', reason: 'Grund', status: 'Status', requested: 'Beantragt', actions: 'Aktionen',
    empty: 'Keine Erstattungsanträge', approve: 'Genehmigen', reject: 'Ablehnen', rejected: 'Abgelehnt: {{reason}}',
    confirmApproval: 'Erstattung über {{amount}} genehmigen?', rejectDialogTitle: 'Erstattungsantrag ablehnen',
    rejectionReason: 'Ablehnungsgrund', rejectionPlaceholder: 'Erklären Sie, warum dieser Erstattungsantrag abgelehnt wird…',
    cancel: 'Abbrechen', rejectRefund: 'Erstattung ablehnen',
    statuses: { pending: 'Ausstehend', approved: 'Genehmigt', rejected: 'Abgelehnt', processed: 'Verarbeitet' },
  },
  artistFans: {
    loading: 'Fans werden geladen…', loadError: 'Fans konnten nicht geladen werden.',
    empty: 'Dieser Künstler hat noch keine Fans.', followedSince: 'Fan seit {{date}}',
  },
  partyRelated: {
    unavailable: 'Kontakt nicht verfügbar', contact: 'Kontakt', subtitle: 'Schnellnavigation durch den zugehörigen Verlauf',
    organization: 'Organisation', user: 'Benutzer', profile: 'Profil', customerBookings: 'Buchungen (Kunde)',
    engineerBookings: 'Buchungen (Toningenieur)', studentClasses: 'Kurse (Schüler)', teacherClasses: 'Kurse (Lehrer)',
    tracks: 'Tracks', bookings: 'Buchungen', customer: 'Kunde', engineer: 'Toningenieur', classes: 'Kurse',
    student: 'Schüler', teacher: 'Lehrer', bookingFallback: 'Buchung Nr. {{id}}', subjectFallback: 'Fach Nr. {{id}}',
    studentFallback: 'Schüler Nr. {{id}}', updated: 'Aktualisiert: {{date}}', loading: 'Zugehöriger Verlauf wird geladen',
    loadError: 'Der zugehörige Verlauf konnte nicht geladen werden.',
    empty: 'Es gibt noch keinen zugehörigen Verlauf. Verwenden Sie Profil, um diesen Kontakt zu prüfen oder zu vervollständigen.',
    bookingItems: 'Buchungen', classItems: 'Kurse', trackItems: 'Tracks',
    statuses: { confirmed: 'Bestätigt', confirmada: 'Bestätigt', scheduled: 'Geplant', programada: 'Geplant', mix: 'Mix' },
  },
  sessionMenu: { noRoles: 'Keine Rollen zugewiesen', logout: 'Abmelden', open: 'Sitzungsmenü öffnen' },
  preferences: {
    title: 'Sprache und Region', subtitle: 'Wähle die Darstellung von Sprache, Währung, Datum und Uhrzeit.',
    language: 'Sprache', currency: 'Währung', timezone: 'Zeitzone', country: 'Ländercode',
    countryHint: 'Optionaler zweistelliger ISO-Code, zum Beispiel DE oder AT.',
  },
  system: { title: 'Systemstatus', subtitle: 'Backend-Version, Status und Metadaten.', application: 'Anwendung', version: 'Version', status: 'Status', built: 'Erstellt', codebase: 'Codebasis' },
} as const;

export default de;
