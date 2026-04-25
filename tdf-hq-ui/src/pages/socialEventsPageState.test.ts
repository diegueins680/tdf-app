import { getSocialEventsOverviewUiState } from './socialEventsPageState';

describe('getSocialEventsOverviewUiState', () => {
  it('hides empty calendar chrome on first run and points admins to the first event', () => {
    expect(getSocialEventsOverviewUiState({
      canCreateEvent: true,
      eventCount: 0,
      filtersActive: false,
    })).toEqual({
      emptyEventsMessage:
        'No hay eventos sociales por venir. Usa Crear evento para registrar el primero; el calendario aparecera cuando exista al menos un evento.',
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
      showCalendar: true,
    });
  });
});
