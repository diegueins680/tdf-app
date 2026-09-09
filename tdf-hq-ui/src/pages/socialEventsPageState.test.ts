import { QueryClient } from '@tanstack/react-query';
import {
  getSocialEventCardActionUiState,
  getSocialEventsCreateUiState,
  getSocialEventsFinanceSummaryUiState,
  getSocialEventsOverviewUiState,
  removeDeletedSocialEventQueries,
} from './socialEventsPageState';

describe('removeDeletedSocialEventQueries', () => {
  it('removes every cached view of the deleted event without touching another event', () => {
    const queryClient = new QueryClient();
    queryClient.setQueryData(['social-event', '121'], { eventId: '121' });
    queryClient.setQueryData(['social-event-moments', '121'], [{ momentId: 'one' }]);
    queryClient.setQueryData(['social-event-ticket-tiers', '121'], [{ ticketTierId: 'general' }]);
    queryClient.setQueryData(['public-event-ticket-storefront', '121'], { checkoutAvailable: true });
    queryClient.setQueryData(['event-logistics', '121'], { eventId: '121' });
    queryClient.setQueryData(['social-invitations', '121'], [{ invitationId: 'invite-one' }]);
    queryClient.setQueryData(['social-ticket-tiers', '121'], [{ ticketTierId: 'general' }]);
    queryClient.setQueryData(['social-ticket-orders', '121', 'organizer'], [{ orderId: 'order-one' }]);
    queryClient.setQueryData(['social-budget-lines', '121'], [{ budgetLineId: 'line-one' }]);
    queryClient.setQueryData(['social-finance-entries', '121'], [{ financeEntryId: 'entry-one' }]);
    queryClient.setQueryData(['social-finance-summary', '121'], { eventId: '121' });
    queryClient.setQueryData(['social-event', '122'], { eventId: '122' });

    removeDeletedSocialEventQueries(queryClient, '121');

    expect(queryClient.getQueryData(['social-event', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-event-moments', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-event-ticket-tiers', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['public-event-ticket-storefront', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['event-logistics', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-invitations', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-ticket-tiers', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-ticket-orders', '121', 'organizer'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-budget-lines', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-finance-entries', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-finance-summary', '121'])).toBeUndefined();
    expect(queryClient.getQueryData(['social-event', '122'])).toEqual({ eventId: '122' });

    queryClient.clear();
  });
});

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
      listLoadSucceeded: true,
    })).toEqual({
      createWelcomeDescription:
        'Empieza con el nombre y la fecha. Podrás sumar colaboradores, venue y detalles sin salir del flujo.',
      showCreateWelcome: true,
      showCreateToolbarAction: false,
    });
  });

  it('uses one toolbar action after events exist', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: true,
      eventCount: 2,
      filtersActive: false,
      listLoadSucceeded: true,
    })).toEqual({
      createWelcomeDescription:
        'Crea otro evento con un borrador guiado y acceso inmediato para tu equipo.',
      showCreateWelcome: false,
      showCreateToolbarAction: true,
    });
  });

  it('uses the compact create action for filtered empty results', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: true,
      listLoadSucceeded: true,
    })).toEqual({
      createWelcomeDescription:
        'Crea otro evento con un borrador guiado y acceso inmediato para tu equipo.',
      showCreateWelcome: false,
      showCreateToolbarAction: true,
    });
  });

  it('does not offer create controls without a session', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: false,
      eventCount: 2,
      filtersActive: false,
      listLoadSucceeded: true,
    })).toEqual({
      createWelcomeDescription:
        'Crea otro evento con un borrador guiado y acceso inmediato para tu equipo.',
      showCreateWelcome: false,
      showCreateToolbarAction: false,
    });
  });

  it('keeps a compact create action while the first event list is unavailable', () => {
    expect(getSocialEventsCreateUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: false,
      listLoadSucceeded: false,
    })).toEqual({
      createWelcomeDescription:
        'Empieza con el nombre y la fecha. Podrás sumar colaboradores, venue y detalles sin salir del flujo.',
      showCreateWelcome: false,
      showCreateToolbarAction: true,
    });
  });
});

describe('getSocialEventCardActionUiState', () => {
  it('keeps signed-out event cards to readable event and ticket summary only', () => {
    expect(getSocialEventCardActionUiState({
      hasSession: false,
      hasAdminAccess: false,
      isOrganizer: false,
      ticketTierCount: 2,
    })).toEqual({
      showDeleteAction: false,
      showInviteForm: false,
      showOrganizerTools: false,
      showRsvpActions: false,
      showTicketOrders: false,
      showTicketPurchaseForm: false,
      showTicketSection: true,
    });
  });

  it('hides empty ticket chrome when signed-out cards have no ticket tiers', () => {
    expect(getSocialEventCardActionUiState({
      hasSession: false,
      hasAdminAccess: false,
      isOrganizer: false,
      ticketTierCount: 0,
    }).showTicketSection).toBe(false);
  });

  it('removes attendee RSVP actions from organizer cards while preserving organizer tools', () => {
    expect(getSocialEventCardActionUiState({
      hasSession: true,
      hasAdminAccess: false,
      isOrganizer: true,
      ticketTierCount: 0,
    })).toEqual({
      showDeleteAction: true,
      showInviteForm: true,
      showOrganizerTools: true,
      showRsvpActions: false,
      showTicketOrders: true,
      showTicketPurchaseForm: true,
      showTicketSection: true,
    });
  });

  it('shows event deletion to admins even when another party organizes the event', () => {
    expect(getSocialEventCardActionUiState({
      hasSession: true,
      hasAdminAccess: true,
      isOrganizer: false,
      ticketTierCount: 0,
    }).showDeleteAction).toBe(true);
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
