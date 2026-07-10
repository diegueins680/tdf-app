import {
  EMPTY_BAND_SCORE,
  buildBudgetRows,
  calculateBandScore,
  calculateCampaignSummary,
  getCampaignDayFromDates,
  getContentCalendarDate,
  getCurrentPhaseId,
  type BandScoreBreakdown,
} from './tdfDomoCampaign';

describe('tdfDomoCampaign', () => {
  it('calculates the weighted band score from the published selection criteria', () => {
    const perfectScore: BandScoreBreakdown = {
      musicQuality: 10,
      livePerformance: 10,
      artisticIdentity: 10,
      stageVisuals: 10,
      professionalism: 10,
      diversity: 10,
    };

    expect(calculateBandScore(perfectScore)).toBe(100);
    expect(calculateBandScore(EMPTY_BAND_SCORE)).toBe(0);

    expect(calculateBandScore({
      musicQuality: 8,
      livePerformance: 7,
      artisticIdentity: 6,
      stageVisuals: 5,
      professionalism: 10,
      diversity: 4,
    })).toBe(70.5);
  });

  it('clamps impossible score values before weighting', () => {
    expect(calculateBandScore({
      musicQuality: 12,
      livePerformance: -4,
      artisticIdentity: 5,
      stageVisuals: Number.NaN,
      professionalism: 11,
      diversity: 3,
    })).toBe(51.5);
  });

  it('summarizes funnel and cost metrics without dividing by zero', () => {
    expect(calculateCampaignSummary({
      reach: 10_000,
      videoViews: 6_000,
      video50Views: 2_500,
      saves: 120,
      shares: 80,
      profileVisits: 900,
      formClicks: 240,
      whatsappMessages: 30,
      applications: 60,
      qualifiedApplications: 24,
      selectedBands: 6,
      adSpendUsd: 360,
    })).toEqual({
      costPerApplication: 6,
      costPerQualified: 15,
      applicationRate: 0.25,
      qualifiedRate: 0.4,
      selectionProgress: 0.6,
      remainingSlots: 4,
    });

    expect(calculateCampaignSummary({
      reach: 0,
      videoViews: 0,
      video50Views: 0,
      saves: 0,
      shares: 0,
      profileVisits: 0,
      formClicks: 0,
      whatsappMessages: 0,
      applications: 0,
      qualifiedApplications: 0,
      selectedBands: 0,
      adSpendUsd: 0,
    }).costPerApplication).toBeNull();
  });

  it('builds the recommended budget split in dollars', () => {
    expect(buildBudgetRows(1000).map((row) => [row.id, row.amountUsd])).toEqual([
      ['awareness', 450],
      ['consideration', 250],
      ['conversion', 250],
      ['testing', 50],
    ]);
  });

  it('maps launch dates to campaign day, phase, and calendar dates', () => {
    expect(getCampaignDayFromDates('2026-07-09', '2026-07-09')).toBe(1);
    expect(getCampaignDayFromDates('2026-07-09', '2026-07-05')).toBe(-3);
    expect(getCurrentPhaseId(-3)).toBe('teaser');
    expect(getCurrentPhaseId(9)).toBe('desire');
    expect(getCurrentPhaseId(21)).toBe('closing');
    expect(getCurrentPhaseId(22)).toBeNull();
    expect(getContentCalendarDate(3, '2026-07-09')).toBe('2026-07-11');
  });
});

