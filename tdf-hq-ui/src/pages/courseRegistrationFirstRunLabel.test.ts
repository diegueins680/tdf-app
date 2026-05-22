import { jest } from '@jest/globals';

jest.unstable_mockModule('../utils/logger', () => ({
  logger: {
    log: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({
  default: () => null,
}));

const { cohortFirstRunLabel } = await import('./CourseRegistrationsAdminPage');

describe('cohortFirstRunLabel', () => {
  it('strips ad-platform lead form wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Google Ads lead form - Beatmaking 101',
      'Beatmaking 101 - YouTube ads lead page',
      'Formulario de leads de Google Ads - Beatmaking 101',
      'LinkedIn lead ads form - Beatmaking 101',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('keeps legitimate ad-platform course titles when they are not lead form artifacts', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'google-ads-for-producers', ccTitle: 'Google Ads for Producers' }),
    ).toBe('Google Ads for Producers');
    expect(
      cohortFirstRunLabel({ ccSlug: 'youtube-ads-masterclass', ccTitle: 'YouTube Ads Masterclass' }),
    ).toBe('YouTube Ads Masterclass');
  });

  it('strips feedback and evaluation descriptors before first-run copy uses the cohort label', () => {
    const titles = [
      'Course feedback form - Beatmaking 101',
      'Beatmaking 101 - evaluation page',
      'Formulario de evaluación - Beatmaking 101',
      'Beatmaking 101 - retroalimentación del curso',
      'Post-course feedback form - Beatmaking 101',
      'NPS survey for Beatmaking 101',
      'Beatmaking 101 - encuesta de satisfacción',
      'Beatmaking 101 - evaluación final del curso',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('keeps legitimate survey and evaluation course titles when they are not form artifacts', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'music-history-survey', ccTitle: 'Survey of Music History' }),
    ).toBe('Survey of Music History');
    expect(
      cohortFirstRunLabel({ ccSlug: 'producer-evaluation', ccTitle: 'Evaluation for Producers' }),
    ).toBe('Evaluation for Producers');
  });

  it('strips WhatsApp deep-link wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'wa.me link - Beatmaking 101',
      'WhatsApp chat link - Beatmaking 101',
      'WhatsApp deep link for Beatmaking 101',
      'Enlace de WhatsApp para Beatmaking 101',
      'Beatmaking 101 - link de wa.me',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('strips standalone scheduling providers attached to first-run cohort labels', () => {
    const titles = [
      'Beatmaking 101 - Calendly',
      'Beatmaking 101 | Cal.com',
      'Beatmaking 101 / Google Calendar',
      'Beatmaking 101: Microsoft Bookings',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('keeps legitimate scheduling-provider course titles when they are not wrappers', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'booking-tools-for-musicians', ccTitle: 'Calendly for Musicians' }),
    ).toBe('Calendly for Musicians');
  });

  it('strips video asset wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Welcome video - Beatmaking 101',
      'Orientation recording for Beatmaking 101',
      'Beatmaking 101 - onboarding video link',
      'Vimeo replay - Beatmaking 101',
      'Video de bienvenida - Beatmaking 101',
      'Beatmaking 101 - grabacion de orientacion',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('keeps legitimate video course titles when they are not asset wrappers', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'video-production-for-tdf-artists', ccTitle: 'Video Production for TDF Artists' }),
    ).toBe('Video Production for TDF Artists');
    expect(
      cohortFirstRunLabel({ ccSlug: 'youtube-for-musicians', ccTitle: 'YouTube for Musicians' }),
    ).toBe('YouTube for Musicians');
  });

  it('strips course-change request wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Course cancellation request - Beatmaking 101',
      'Beatmaking 101 - refund request form',
      'Withdrawal page for Beatmaking 101',
      'Beatmaking 101 - reschedule request',
      'Solicitud de reembolso - Beatmaking 101',
      'Beatmaking 101 - formulario de retiro del curso',
      'Solicitud de reprogramación para Beatmaking 101',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('strips reminder and notification signup wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Course reminder signup - Beatmaking 101',
      'Alert subscription for Beatmaking 101',
      'Beatmaking 101 - notification form',
      'Formulario de recordatorios - Beatmaking 101',
      'Beatmaking 101 - registro de notificaciones',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('strips waitlist portal and link wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Waitlist portal - Beatmaking 101',
      'Waiting list link for Beatmaking 101',
      'Beatmaking 101 - waitlist URL',
      'Portal de lista de espera - Beatmaking 101',
      'Beatmaking 101 - enlace de lista de espera',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('strips email marketing automation wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'MailerLite email sequence - Beatmaking 101',
      'Brevo automation - Beatmaking 101',
      'ActiveCampaign drip campaign for Beatmaking 101',
      'Beatmaking 101 - Klaviyo follow-up workflow',
      'RD Station secuencia de correos - Beatmaking 101',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('strips notification opt-in form wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'WhatsApp opt-in form - Beatmaking 101',
      'Beatmaking 101 - SMS opt-in page',
      'Formulario de opt-in de WhatsApp - Beatmaking 101',
      'Pagina de autorizacion de WhatsApp - Beatmaking 101',
      'Formulario de autorizacion via SMS para Beatmaking 101',
      'WhatsApp consent page for Beatmaking 101',
    ];

    for (const title of titles) {
      const label = cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title });
      expect(`${title} -> ${label}`).toBe(`${title} -> Beatmaking 101`);
    }
  });

  it('strips parent consent wrappers without treating them as notification opt-ins', () => {
    expect(
      cohortFirstRunLabel({
        ccSlug: 'beatmaking-101',
        ccTitle: 'Formulario de consentimiento de padres - Beatmaking 101',
      }),
    ).toBe('Beatmaking 101');
  });

  it('strips admin workflow wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Application review queue - Beatmaking 101',
      'Enrollment approval inbox for Beatmaking 101',
      'Beatmaking 101 - registration triage dashboard',
      'Course moderation tracker - Beatmaking 101',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('strips kanban and pipeline admin workflow wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Payment kanban - Beatmaking 101',
      'Follow-up pipeline for Beatmaking 101',
      'Beatmaking 101 - receipts kanban',
      'Kanban de pagos para Beatmaking 101',
      'Pipeline de seguimiento - Beatmaking 101',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('keeps legitimate kanban and pipeline course titles when they are not admin workflow wrappers', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'kanban-producers', ccTitle: 'Kanban for Producers' }),
    ).toBe('Kanban for Producers');
    expect(
      cohortFirstRunLabel({ ccSlug: 'music-business-pipeline', ccTitle: 'Music Business Pipeline' }),
    ).toBe('Music Business Pipeline');
  });

  it('strips payment and follow-up workflow wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Follow-up queue - Beatmaking 101',
      'Payment review board for Beatmaking 101',
      'Beatmaking 101 - receipts approval tracker',
      'Bandeja de seguimiento para Beatmaking 101',
      'Beatmaking 101 - tablero de pagos',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('strips contest and giveaway entry wrappers before first-run copy uses the cohort label', () => {
    const titles = [
      'Giveaway entry form - Beatmaking 101',
      'Raffle signup page for Beatmaking 101',
      'Beatmaking 101 - sweepstakes registration page',
      'Formulario de sorteo - Beatmaking 101',
      'Registro de concurso para Beatmaking 101',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('keeps legitimate contest course titles when they are not entry-form artifacts', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'contest-prep', ccTitle: 'Contest Prep for Producers' }),
    ).toBe('Contest Prep for Producers');
    expect(
      cohortFirstRunLabel({ ccSlug: 'giveaway-strategy', ccTitle: 'Giveaway Strategy for Artists' }),
    ).toBe('Giveaway Strategy for Artists');
  });

  it('strips wrapped response-sheet prefixes before first-run copy uses the cohort label', () => {
    const titles = [
      '(Form responses 1) - Beatmaking 101',
      '[Google Forms responses] - Beatmaking 101',
      '(Respuestas del formulario) - Beatmaking 101',
    ];

    for (const title of titles) {
      expect(cohortFirstRunLabel({ ccSlug: 'beatmaking-101', ccTitle: title })).toBe('Beatmaking 101');
    }
  });

  it('keeps legitimate alert and notification course titles when they are not signup artifacts', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'alerts-engineering-lab', ccTitle: 'Live Alerts for Engineers' }),
    ).toBe('Live Alerts for Engineers');
    expect(
      cohortFirstRunLabel({ ccSlug: 'notification-design', ccTitle: 'Notification Design for Creators' }),
    ).toBe('Notification Design for Creators');
  });

  it('keeps legitimate opt-in course titles when they are not form artifacts', () => {
    expect(
      cohortFirstRunLabel({
        ccSlug: 'whatsapp-opt-in-strategy',
        ccTitle: 'WhatsApp Opt-In Strategy for Artists',
      }),
    ).toBe('WhatsApp Opt-In Strategy for Artists');
  });

  it('keeps legitimate course-change topics when they are not request artifacts', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'refund-policy', ccTitle: 'Refund Policy for Creators' }),
    ).toBe('Refund Policy for Creators');
    expect(
      cohortFirstRunLabel({ ccSlug: 'schedule-change-workshop', ccTitle: 'Schedule Change Workshop' }),
    ).toBe('Schedule Change Workshop');
  });
});
