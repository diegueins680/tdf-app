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
});
