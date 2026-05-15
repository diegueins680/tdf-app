import { getSocialEventsCreateUiState, getSocialEventsOverviewUiState } from './socialEventsPageState';

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
    });
  });

  it('uses filter-specific empty guidance without restoring the empty calendar', () => {
    expect(getSocialEventsOverviewUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: true,
    })).toEqual({
      emptyEventsMessage:
        'No hay eventos por venir para este filtro. Ajusta ciudad, tipo o estado para ampliar la busqueda.',
      showFilters: true,
      showCalendar: false,
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
