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
  sessionMenu: { noRoles: 'Keine Rollen zugewiesen', logout: 'Abmelden', open: 'Sitzungsmenü öffnen' },
  preferences: {
    title: 'Sprache und Region', subtitle: 'Wähle die Darstellung von Sprache, Währung, Datum und Uhrzeit.',
    language: 'Sprache', currency: 'Währung', timezone: 'Zeitzone', country: 'Ländercode',
    countryHint: 'Optionaler zweistelliger ISO-Code, zum Beispiel DE oder AT.',
  },
  system: { title: 'Systemstatus', subtitle: 'Backend-Version, Status und Metadaten.', application: 'Anwendung', version: 'Version', status: 'Status', built: 'Erstellt', codebase: 'Codebasis' },
} as const;

export default de;
