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

  it('keeps legitimate course-change topics when they are not request artifacts', () => {
    expect(
      cohortFirstRunLabel({ ccSlug: 'refund-policy', ccTitle: 'Refund Policy for Creators' }),
    ).toBe('Refund Policy for Creators');
    expect(
      cohortFirstRunLabel({ ccSlug: 'schedule-change-workshop', ccTitle: 'Schedule Change Workshop' }),
    ).toBe('Schedule Change Workshop');
  });
});
