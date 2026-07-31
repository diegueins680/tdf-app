import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const postMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: postMock,
}));

const { CampaignAutomations } = await import('./campaignAutomations');

describe('CampaignAutomations API', () => {
  beforeEach(() => {
    getMock.mockReset();
    postMock.mockReset();
  });

  it('installs campaigns as explicit template-keyed drafts', async () => {
    postMock.mockResolvedValueOnce({});

    await CampaignAutomations.install('music-services', '2026-07-30T14:00:00Z', 12);

    expect(postMock).toHaveBeenCalledWith('/ads/automations', {
      templateKey: 'music-services',
      startAt: '2026-07-30T14:00:00Z',
      dailyLimit: 12,
    });
  });

  it('uses the automation id for enrollment, preview, and recipient routes', async () => {
    postMock.mockResolvedValueOnce({ acceptedPartyIds: [7, 9], rejected: [] });
    getMock.mockResolvedValue([]);

    await CampaignAutomations.enroll(42, [7, 9]);
    await CampaignAutomations.preview(42);
    await CampaignAutomations.enrollments(42);

    expect(postMock).toHaveBeenCalledWith('/ads/automations/42/enroll', {
      partyIds: [7, 9],
    });
    expect(getMock).toHaveBeenNthCalledWith(1, '/ads/automations/42/preview');
    expect(getMock).toHaveBeenNthCalledWith(2, '/ads/automations/42/enrollments');
  });

  it('serializes campaign and enrollment status changes', async () => {
    postMock.mockResolvedValue({});

    await CampaignAutomations.setStatus(42, 'active', true);
    await CampaignAutomations.setEnrollmentStatus(
      42,
      99,
      'converted',
      '  operator_marked_converted  ',
    );

    expect(postMock).toHaveBeenNthCalledWith(
      1,
      '/ads/automations/42/status',
      { status: 'active', templatesApproved: true },
    );
    expect(postMock).toHaveBeenNthCalledWith(
      2,
      '/ads/automations/42/enrollments/99/status',
      {
        status: 'converted',
        reason: 'operator_marked_converted',
      },
    );
  });

  it.each([
    ['enroll', () => CampaignAutomations.enroll(0, [7])],
    ['preview', () => CampaignAutomations.preview(Number.NaN)],
    ['enrollments', () => CampaignAutomations.enrollments(-1)],
    ['campaign status', () => CampaignAutomations.setStatus(1.5, 'paused')],
    [
      'enrollment status',
      () => CampaignAutomations.setEnrollmentStatus(42, Number.MAX_SAFE_INTEGER + 1, 'stopped'),
    ],
  ])('rejects invalid ids before calling the API for %s', async (_label, action) => {
    expect(() => action()).toThrow('debe ser un entero positivo');
    expect(getMock).not.toHaveBeenCalled();
    expect(postMock).not.toHaveBeenCalled();
  });
});
