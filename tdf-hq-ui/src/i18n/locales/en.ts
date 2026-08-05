const en = {
  Parties: 'Parties',
  Bookings: 'Bookings',
  Kanban: 'Pipelines',
  common: {
    save: 'Save',
    saving: 'Saving…',
    saved: 'Preferences saved.',
    error: 'Something went wrong.',
  },
  sessionMenu: {
    noRoles: 'No roles assigned',
    logout: 'Sign out',
    open: 'Open session menu',
  },
  preferences: {
    title: 'Language and region',
    subtitle: 'Choose how language, money, dates, and times are displayed.',
    language: 'Language',
    currency: 'Currency',
    timezone: 'Time zone',
    country: 'Country code',
    countryHint: 'Optional two-letter ISO code, for example US or DE.',
  },
  system: {
    title: 'System status',
    subtitle: 'Backend version, health, and metadata.',
    application: 'Application',
    version: 'Version',
    status: 'Status',
    built: 'Built',
    codebase: 'Codebase',
  },
  notifications: {
    title: 'Notifications',
    markAll: 'Mark all read',
    markAllLoading: 'Marking…',
    loading: 'Loading notifications',
    empty: 'No notifications',
  },
  login: {
    resetDialog: { title: 'Recover access' },
    signupDialog: { title: 'Create account' },
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
    sections: { startEyebrow: 'Start on your own' },
  },
} as const;

export default en;
