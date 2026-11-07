import { assertSafeEventFinanceMoney } from './eventFinanceMoney';
import type { SocialEventDTO, SocialEventBudgetLineDTO, SocialEventFinanceEntryDTO, SocialEventFinanceSummaryDTO } from '../api/socialEvents';
import { resolveRuntimeCurrency } from './formatters';

export const buildEventFinanceCsvRows = (
  event: SocialEventDTO,
  budgetLines: SocialEventBudgetLineDTO[],
  financeEntries: SocialEventFinanceEntryDTO[],
  financeSummary: SocialEventFinanceSummaryDTO | null,
) => {
  if (financeSummary) assertSafeEventFinanceMoney(financeSummary);
  budgetLines.forEach(assertSafeEventFinanceMoney);
  financeEntries.forEach(assertSafeEventFinanceMoney);
  const rows: (string | number | boolean | null | undefined)[][] = [];
  rows.push([
    'record_type',
    'event_id',
    'event_title',
    'event_start',
    'event_end',
    'currency',
    'metric_name',
    'metric_value',
    'line_id',
    'line_code',
    'line_name',
    'line_type',
    'line_category',
    'line_planned_cents',
    'line_actual_cents',
    'entry_id',
    'entry_direction',
    'entry_source',
    'entry_category',
    'entry_status',
    'entry_concept',
    'entry_amount_cents',
    'entry_external_ref',
    'entry_occurred_at',
  ]);

  const eventId = event.eventId ?? '';
  const eventTitle = event.eventTitle;
  const eventStart = event.eventStart;
  const eventEnd = event.eventEnd;
  const eventCurrency = (financeSummary?.efsCurrency ?? event.eventCurrency ?? resolveRuntimeCurrency()).toUpperCase();

  const summaryMetrics: [string, number | string | null | undefined][] = [
    ['budget_cents', financeSummary?.efsBudgetCents],
    ['planned_income_cents', financeSummary?.efsPlannedIncomeCents],
    ['planned_expense_cents', financeSummary?.efsPlannedExpenseCents],
    ['actual_income_cents', financeSummary?.efsActualIncomeCents],
    ['actual_expense_cents', financeSummary?.efsActualExpenseCents],
    ['net_cents', financeSummary?.efsNetCents],
    ['ticket_paid_revenue_cents', financeSummary?.efsTicketPaidRevenueCents],
    ['ticket_refunded_revenue_cents', financeSummary?.efsTicketRefundedRevenueCents],
    ['ticket_pending_revenue_cents', financeSummary?.efsTicketPendingRevenueCents],
    ['accounts_payable_cents', financeSummary?.efsAccountsPayableCents],
    ['accounts_receivable_cents', financeSummary?.efsAccountsReceivableCents],
    ['contract_committed_cents', financeSummary?.efsContractCommittedCents],
    ['contract_paid_cents', financeSummary?.efsContractPaidCents],
    ['procurement_committed_cents', financeSummary?.efsProcurementCommittedCents],
    ['procurement_paid_cents', financeSummary?.efsProcurementPaidCents],
    ['asset_investment_cents', financeSummary?.efsAssetInvestmentCents],
    ['liability_balance_cents', financeSummary?.efsLiabilityBalanceCents],
    ['budget_variance_cents', financeSummary?.efsBudgetVarianceCents],
    ['budget_utilization_pct', financeSummary?.efsBudgetUtilizationPct],
  ];

  summaryMetrics.forEach(([metricName, metricValue]) => {
    rows.push([
      'summary',
      eventId,
      eventTitle,
      eventStart,
      eventEnd,
      eventCurrency,
      metricName,
      metricValue,
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
    ]);
  });

  budgetLines.forEach((line) => {
    rows.push([
      'budget_line',
      eventId,
      eventTitle,
      eventStart,
      eventEnd,
      eventCurrency,
      '',
      '',
      line.eblId ?? '',
      line.eblCode,
      line.eblName,
      line.eblType,
      line.eblCategory,
      line.eblPlannedCents,
      line.eblActualCents ?? '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
    ]);
  });

  financeEntries.forEach((entry) => {
    rows.push([
      'finance_entry',
      eventId,
      eventTitle,
      eventStart,
      eventEnd,
      entry.efeCurrency,
      '',
      '',
      entry.efeBudgetLineId ?? '',
      '',
      '',
      '',
      '',
      '',
      '',
      entry.efeId ?? '',
      entry.efeDirection,
      entry.efeSource,
      entry.efeCategory,
      entry.efeStatus,
      entry.efeConcept,
      entry.efeAmountCents,
      entry.efeExternalRef ?? '',
      entry.efeOccurredAt,
    ]);
  });

  return rows;
};

