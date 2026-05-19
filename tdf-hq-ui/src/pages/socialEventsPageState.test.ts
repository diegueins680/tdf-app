import {
  getSocialEventsCreateUiState,
  getSocialEventsFinanceSummaryUiState,
  getSocialEventsOverviewUiState,
} from './socialEventsPageState';

describe('getSocialEventsOverviewUiState', () => {
  it('hides empty calendar chrome on first run and points admins to the first event', () => {
    expect(getSocialEventsOverviewUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: false,
    })).toEqual({
      emptyEventsMessage:
        'No hay eventos sociales por venir. Usa Crear evento para registrar el primero; el calendario aparecera cuando exista al menos un evento.',
      showFilters: false,
      showCalendar: false,
      showRefreshAction: false,
    });
  });

  it('uses filter-specific empty guidance without restoring empty calendar or refresh chrome', () => {
    expect(getSocialEventsOverviewUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: true,
    })).toEqual({
      emptyEventsMessage:
        'No hay eventos por venir para este filtro. Ajusta ciudad, tipo o estado para ampliar la busqueda.',
      showFilters: true,
      showCalendar: false,
      showRefreshAction: false,
    });
  });

  it('keeps the calendar visible once there is at least one event to place on it', () => {
    expect(getSocialEventsOverviewUiState({
      canCreateEvent: true,
      eventCount: 1,
      filtersActive: false,
    })).toEqual({
      emptyEventsMessage: null,
      showFilters: true,
      showCalendar: true,
      showRefreshAction: true,
    });
  });
});

describe('getSocialEventsCreateUiState', () => {
  it('keeps the create form visible for the first event', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: false,
      createFormOpen: false,
    })).toEqual({
      createFormDescription:
        'Crea el primer evento aqui; cuando exista al menos uno, apareceran los filtros y el calendario.',
      showCreateForm: true,
      showCreateToolbarAction: false,
    });
  });

  it('collapses the create form into one toolbar action after events exist', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: true,
      eventCount: 2,
      filtersActive: false,
      createFormOpen: false,
    })).toEqual({
      createFormDescription:
        'Completa solo los campos necesarios para registrar otro evento. La lista actual se mantiene debajo.',
      showCreateForm: false,
      showCreateToolbarAction: true,
    });
  });

  it('uses the compact create action for filtered empty results', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: true,
      createFormOpen: false,
    })).toEqual({
      createFormDescription:
        'Completa solo los campos necesarios para registrar otro evento. La lista actual se mantiene debajo.',
      showCreateForm: false,
      showCreateToolbarAction: true,
    });
  });

  it('does not offer create controls without a session', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: false,
      eventCount: 2,
      filtersActive: false,
      createFormOpen: true,
    })).toEqual({
      createFormDescription:
        'Completa solo los campos necesarios para registrar otro evento. La lista actual se mantiene debajo.',
      showCreateForm: false,
      showCreateToolbarAction: false,
    });
  });
});

describe('getSocialEventsFinanceSummaryUiState', () => {
  const buildFinanceSummary = (overrides = {}) => ({
    efsActualIncomeCents: 0,
    efsActualExpenseCents: 0,
    efsNetCents: 0,
    efsBudgetUtilizationPct: null,
    efsAccountsPayableCents: 0,
    efsAccountsReceivableCents: 0,
    efsContractCommittedCents: 0,
    efsContractPaidCents: 0,
    efsProcurementCommittedCents: 0,
    efsProcurementPaidCents: 0,
    efsAssetInvestmentCents: 0,
    efsLiabilityBalanceCents: 0,
    efsTicketPaidRevenueCents: 0,
    efsTicketRefundedRevenueCents: 0,
    ...overrides,
  });

  it('keeps first-run finance summaries to core chips plus one omitted-zero explanation', () => {
    const state = getSocialEventsFinanceSummaryUiState(buildFinanceSummary());

    expect(state.metrics.map((metric) => metric.label)).toEqual([
      'Ingresos',
      'Gastos',
      'Neto',
      'Utilizacion',
    ]);
    expect(state.omittedEmptyDetailCount).toBe(10);
    expect(state.omittedEmptyDetailSummary).toBe(
      'Los detalles financieros en cero se omiten hasta que tengan movimiento.',
    );
  });

  it('restores only finance detail chips that have movement', () => {
    const state = getSocialEventsFinanceSummaryUiState(buildFinanceSummary({
      efsAccountsPayableCents: 15000,
      efsContractPaidCents: -2500,
      efsTicketPaidRevenueCents: 40000,
    }));

    expect(state.metrics.map((metric) => metric.label)).toEqual([
      'Ingresos',
      'Gastos',
      'Neto',
      'Utilizacion',
      'CxP',
      'Contratos pagados',
      'Tickets pagados',
    ]);
    expect(state.omittedEmptyDetailCount).toBe(7);
    expect(state.metrics.some((metric) => metric.label === 'CxC')).toBe(false);
    expect(state.metrics.some((metric) => metric.label === 'Tickets reembolsados')).toBe(false);
  });
});
