import { buildEventFinanceCsvRows } from './eventFinanceCsv';
import type { SocialEventDTO, SocialEventBudgetLineDTO, SocialEventFinanceEntryDTO, SocialEventFinanceSummaryDTO } from '../api/socialEvents';
const event = { eventId: '7', eventTitle: 'Test', eventCurrency: 'USD' } as SocialEventDTO;

describe('event finance CSV precision', () => {
  it('refuses unsafe cached summary before producing rows', () => {
    const summary = JSON.parse('{"efsCurrency":"USD","efsNetCents":9007199254740993}') as SocialEventFinanceSummaryDTO;
    expect(() => buildEventFinanceCsvRows(event, [], [], summary)).toThrow('precisión');
  });
  it('refuses unsafe cached budget even without a summary', () => {
    const budget = { eblPlannedCents: 9007199254740992 } as SocialEventBudgetLineDTO;
    expect(() => buildEventFinanceCsvRows(event, [budget], [], null)).toThrow('precisión');
  });
  it('refuses unsafe cached entry even without a summary', () => {
    const entry = { efeAmountCents: -9007199254740992 } as SocialEventFinanceEntryDTO;
    expect(() => buildEventFinanceCsvRows(event, [], [entry], null)).toThrow('precisión');
  });
  it('exports exact safe cents and keeps percentages and absent values', () => {
    const summary = { efsCurrency: 'USD', efsNetCents: -9007199254740991, efsBudgetCents: null, efsBudgetUtilizationPct: 1.25 } as SocialEventFinanceSummaryDTO;
    const rows = buildEventFinanceCsvRows(event, [], [], summary);
    expect(rows.find(row => row[6] === 'net_cents')?.[7]).toBe(-9007199254740991);
    expect(rows.find(row => row[6] === 'budget_cents')?.[7]).toBeNull();
    expect(rows.find(row => row[6] === 'budget_utilization_pct')?.[7]).toBe(1.25);
  });
});
