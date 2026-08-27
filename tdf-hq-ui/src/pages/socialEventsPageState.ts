interface SocialEventsOverviewUiStateInput {
  canCreateEvent: boolean;
  eventCount: number;
  filtersActive: boolean;
}

interface SocialEventsOverviewUiState {
  emptyEventsMessage: string | null;
  showFilters: boolean;
  showCalendar: boolean;
  showRefreshAction: boolean;
}

interface SocialEventsCreateUiStateInput {
  canCreateEvent: boolean;
  eventCount: number;
  filtersActive: boolean;
  listLoadSucceeded: boolean;
}

interface SocialEventsCreateUiState {
  createWelcomeDescription: string;
  showCreateWelcome: boolean;
  showCreateToolbarAction: boolean;
}

interface SocialEventCardActionUiStateInput {
  hasSession: boolean;
  isOrganizer: boolean;
  ticketTierCount: number;
}

interface SocialEventCardActionUiState {
  showInviteForm: boolean;
  showOrganizerTools: boolean;
  showRsvpActions: boolean;
  showTicketOrders: boolean;
  showTicketPurchaseForm: boolean;
  showTicketSection: boolean;
}

interface SocialEventsFinanceSummaryValues {
  efsActualIncomeCents: number;
  efsActualExpenseCents: number;
  efsNetCents: number;
  efsBudgetUtilizationPct?: number | null;
  efsAccountsPayableCents?: number | null;
  efsAccountsReceivableCents?: number | null;
  efsContractCommittedCents?: number | null;
  efsContractPaidCents?: number | null;
  efsProcurementCommittedCents?: number | null;
  efsProcurementPaidCents?: number | null;
  efsAssetInvestmentCents?: number | null;
  efsLiabilityBalanceCents?: number | null;
  efsTicketPaidRevenueCents: number;
  efsTicketRefundedRevenueCents: number;
}

export interface SocialEventsFinanceMetricUiState {
  key: string;
  label: string;
  value: number | null | undefined;
  valueType: 'money' | 'percent';
  color?: 'success' | 'warning';
  variant?: 'outlined';
}

interface SocialEventsFinanceSummaryUiState {
  metrics: SocialEventsFinanceMetricUiState[];
  omittedEmptyDetailCount: number;
  omittedEmptyDetailSummary: string;
}

const FIRST_EVENT_CREATE_DESCRIPTION =
  'Empieza con el nombre y la fecha. Podrás sumar colaboradores, venue y detalles sin salir del flujo.';
const FOLLOW_UP_EVENT_CREATE_DESCRIPTION =
  'Crea otro evento con un borrador guiado y acceso inmediato para tu equipo.';
const ZERO_FINANCE_DETAILS_SUMMARY =
  'Los detalles financieros en cero se omiten hasta que tengan movimiento.';

export function getSocialEventsOverviewUiState({
  canCreateEvent,
  eventCount,
  filtersActive,
}: SocialEventsOverviewUiStateInput): SocialEventsOverviewUiState {
  if (eventCount > 0) {
    return {
      emptyEventsMessage: null,
      showFilters: true,
      showCalendar: true,
      showRefreshAction: true,
    };
  }

  if (filtersActive) {
    return {
      emptyEventsMessage:
        'No hay eventos por venir para este filtro. Ajusta ciudad, tipo o estado para ampliar la busqueda.',
      showFilters: true,
      showCalendar: false,
      showRefreshAction: false,
    };
  }

  return {
    emptyEventsMessage: canCreateEvent
      ? 'No hay eventos sociales por venir. Usa Crear evento para registrar el primero; el calendario aparecera cuando exista al menos un evento.'
      : 'No hay eventos sociales por venir. Inicia sesion para crear el primero; el calendario aparecera cuando exista al menos un evento.',
    showFilters: false,
    showCalendar: false,
    showRefreshAction: false,
  };
}

export function getSocialEventsCreateUiState({
  canCreateEvent,
  eventCount,
  filtersActive,
  listLoadSucceeded,
}: SocialEventsCreateUiStateInput): SocialEventsCreateUiState {
  const isFirstRun = eventCount === 0 && !filtersActive;
  const showCreateWelcome = canCreateEvent && isFirstRun && listLoadSucceeded;

  return {
    createWelcomeDescription: isFirstRun
      ? FIRST_EVENT_CREATE_DESCRIPTION
      : FOLLOW_UP_EVENT_CREATE_DESCRIPTION,
    showCreateWelcome,
    showCreateToolbarAction: canCreateEvent && !showCreateWelcome,
  };
}

export function getSocialEventCardActionUiState({
  hasSession,
  isOrganizer,
  ticketTierCount,
}: SocialEventCardActionUiStateInput): SocialEventCardActionUiState {
  const showTicketPurchaseForm = hasSession;

  return {
    showInviteForm: hasSession,
    showOrganizerTools: hasSession && isOrganizer,
    showRsvpActions: hasSession && !isOrganizer,
    showTicketOrders: hasSession,
    showTicketPurchaseForm,
    showTicketSection: ticketTierCount > 0 || showTicketPurchaseForm,
  };
}

const hasMeaningfulFinanceDetail = (value: number | null | undefined) =>
  typeof value === 'number' && value !== 0;

export function getSocialEventsFinanceSummaryUiState(
  summary: SocialEventsFinanceSummaryValues,
): SocialEventsFinanceSummaryUiState {
  const coreMetrics: SocialEventsFinanceMetricUiState[] = [
    {
      key: 'actual-income',
      label: 'Ingresos',
      value: summary.efsActualIncomeCents,
      valueType: 'money',
      color: 'success',
    },
    {
      key: 'actual-expense',
      label: 'Gastos',
      value: summary.efsActualExpenseCents,
      valueType: 'money',
      color: 'warning',
    },
    {
      key: 'net',
      label: 'Neto',
      value: summary.efsNetCents,
      valueType: 'money',
    },
    {
      key: 'budget-utilization',
      label: 'Utilizacion',
      value: summary.efsBudgetUtilizationPct,
      valueType: 'percent',
      variant: 'outlined',
    },
  ];
  const detailMetrics: SocialEventsFinanceMetricUiState[] = [
    { key: 'accounts-payable', label: 'CxP', value: summary.efsAccountsPayableCents, valueType: 'money', variant: 'outlined' },
    { key: 'accounts-receivable', label: 'CxC', value: summary.efsAccountsReceivableCents, valueType: 'money', variant: 'outlined' },
    { key: 'contract-committed', label: 'Contratos comprometidos', value: summary.efsContractCommittedCents, valueType: 'money', variant: 'outlined' },
    { key: 'contract-paid', label: 'Contratos pagados', value: summary.efsContractPaidCents, valueType: 'money', variant: 'outlined' },
    { key: 'procurement-committed', label: 'Compras comprometidas', value: summary.efsProcurementCommittedCents, valueType: 'money', variant: 'outlined' },
    { key: 'procurement-paid', label: 'Compras pagadas', value: summary.efsProcurementPaidCents, valueType: 'money', variant: 'outlined' },
    { key: 'asset-investment', label: 'Activos', value: summary.efsAssetInvestmentCents, valueType: 'money', variant: 'outlined' },
    { key: 'liability-balance', label: 'Pasivo neto', value: summary.efsLiabilityBalanceCents, valueType: 'money', variant: 'outlined' },
    { key: 'ticket-paid-revenue', label: 'Tickets pagados', value: summary.efsTicketPaidRevenueCents, valueType: 'money', variant: 'outlined' },
    { key: 'ticket-refunded-revenue', label: 'Tickets reembolsados', value: summary.efsTicketRefundedRevenueCents, valueType: 'money', variant: 'outlined' },
  ];
  const visibleDetailMetrics = detailMetrics.filter((metric) => hasMeaningfulFinanceDetail(metric.value));
  const omittedEmptyDetailCount = detailMetrics.length - visibleDetailMetrics.length;

  return {
    metrics: [...coreMetrics, ...visibleDetailMetrics],
    omittedEmptyDetailCount,
    omittedEmptyDetailSummary: omittedEmptyDetailCount > 0 ? ZERO_FINANCE_DETAILS_SUMMARY : '',
  };
}
