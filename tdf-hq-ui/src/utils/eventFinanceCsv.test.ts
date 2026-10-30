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


describe('event finance CSV amount currency', () => {
  const currencyEvent: SocialEventDTO = {
    eventId: '7', eventTitle: 'Test', eventCurrency: 'USD', eventStart: '2030-01-01T12:00:00Z',
    eventEnd: '2030-01-01T13:00:00Z', eventArtists: [],
  };
  it('keeps a foreign void entry in its own currency alongside a valid USD summary', () => {
    const summary: SocialEventFinanceSummaryDTO = {
      efsEventId: '7', efsCurrency: 'USD', efsPlannedIncomeCents: 0, efsPlannedExpenseCents: 0,
      efsActualIncomeCents: 30, efsActualExpenseCents: 0, efsNetCents: 30,
      efsTicketPaidRevenueCents: 0, efsTicketRefundedRevenueCents: 0, efsTicketPendingRevenueCents: 0,
      efsGeneratedAt: '2030-01-01T12:00:00Z',
    };
    const foreignVoid: SocialEventFinanceEntryDTO = {
      efeId: '11', efeEventId: '7', efeDirection: 'income', efeSource: 'manual', efeCategory: 'other',
      efeConcept: 'Synthetic void record', efeAmountCents: 100, efeCurrency: 'EUR', efeStatus: 'void',
      efeOccurredAt: '2030-01-01T12:00:00Z',
    };
    const posted: SocialEventFinanceEntryDTO = {
      ...foreignVoid, efeId: '12', efeConcept: 'Synthetic posted record', efeAmountCents: 30,
      efeCurrency: 'USD', efeStatus: 'posted',
    };
    const rows = buildEventFinanceCsvRows(currencyEvent, [], [foreignVoid, posted], summary);
    expect(rows.filter(row => row[0] === 'finance_entry').map(row => [row[15], row[5], row[21], row[19]]))
      .toEqual([['11', 'EUR', 100, 'void'], ['12', 'USD', 30, 'posted']]);
    expect(rows.filter(row => row[0] === 'summary').every(row => row[5] === 'USD')).toBe(true);
  });

  it('keeps USD summary, budget and entry units, raw cents and the existing 24-column layout', () => {
    const occurredAt = '2030-01-01T12:00:00Z';
    const finishedAt = '2030-01-01T13:00:00Z';
    const summary: SocialEventFinanceSummaryDTO = {
      efsEventId: '7', efsCurrency: 'USD', efsPlannedIncomeCents: 0, efsPlannedExpenseCents: 500,
      efsActualIncomeCents: 0, efsActualExpenseCents: 30, efsNetCents: -30,
      efsTicketPaidRevenueCents: 0, efsTicketRefundedRevenueCents: 0, efsTicketPendingRevenueCents: 0,
      efsGeneratedAt: occurredAt,
    };
    const budget: SocialEventBudgetLineDTO = {
      eblId: '3', eblEventId: '7', eblCode: 'SOUND', eblName: 'Synthetic sound',
      eblType: 'expense', eblCategory: 'production', eblPlannedCents: 500, eblActualCents: 30,
    };
    const posted: SocialEventFinanceEntryDTO = {
      efeId: '12', efeEventId: '7', efeBudgetLineId: '3', efeDirection: 'expense', efeSource: 'manual',
      efeCategory: 'production', efeConcept: 'Synthetic sound expense', efeAmountCents: 30,
      efeCurrency: 'USD', efeStatus: 'posted', efeOccurredAt: occurredAt,
    };
    const rows = buildEventFinanceCsvRows(
      { ...currencyEvent, eventStart: occurredAt, eventEnd: finishedAt }, [budget], [posted], summary,
    );
    expect(rows.every(row => row.length === 24)).toBe(true);
    expect([rows[0]?.[5], rows[0]?.[21], rows[0]?.[23]])
      .toEqual(['currency', 'entry_amount_cents', 'entry_occurred_at']);
    const planned = rows.find(row => row[0] === 'summary' && row[6] === 'planned_expense_cents');
    expect([planned?.[5], planned?.[7]]).toEqual(['USD', 500]);
    const budgetRow = rows.find(row => row[0] === 'budget_line');
    expect([budgetRow?.[5], budgetRow?.[8], budgetRow?.[13], budgetRow?.[14]])
      .toEqual(['USD', '3', 500, 30]);
    const entry = rows.find(row => row[0] === 'finance_entry');
    expect(entry?.slice(1, 5)).toEqual(['7', 'Test', occurredAt, finishedAt]);
    expect([entry?.[5], entry?.[15], entry?.[19], entry?.[21], entry?.[23]])
      .toEqual(['USD', '12', 'posted', 30, occurredAt]);
  });
});
