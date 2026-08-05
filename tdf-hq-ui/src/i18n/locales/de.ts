const de = {
  common: { save: 'Speichern', saving: 'Wird gespeichert…', saved: 'Einstellungen gespeichert.', error: 'Ein Fehler ist aufgetreten.' },
  sessionMenu: { noRoles: 'Keine Rollen zugewiesen', logout: 'Abmelden', open: 'Sitzungsmenü öffnen' },
  preferences: {
    title: 'Sprache und Region', subtitle: 'Wähle die Darstellung von Sprache, Währung, Datum und Uhrzeit.',
    language: 'Sprache', currency: 'Währung', timezone: 'Zeitzone', country: 'Ländercode',
    countryHint: 'Optionaler zweistelliger ISO-Code, zum Beispiel DE oder AT.',
  },
  system: { title: 'Systemstatus', subtitle: 'Backend-Version, Status und Metadaten.', application: 'Anwendung', version: 'Version', status: 'Status', built: 'Erstellt', codebase: 'Codebasis' },
} as const;

export default de;
