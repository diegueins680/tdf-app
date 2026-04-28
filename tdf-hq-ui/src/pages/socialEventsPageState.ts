interface SocialEventsOverviewUiStateInput {
  canCreateEvent: boolean;
  eventCount: number;
  filtersActive: boolean;
}

interface SocialEventsOverviewUiState {
  emptyEventsMessage: string | null;
  showFilters: boolean;
  showCalendar: boolean;
}

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
    };
  }

  if (filtersActive) {
    return {
      emptyEventsMessage:
        'No hay eventos por venir para este filtro. Ajusta ciudad, tipo o estado para ampliar la busqueda.',
      showFilters: true,
      showCalendar: false,
    };
  }

  return {
    emptyEventsMessage: canCreateEvent
      ? 'No hay eventos sociales por venir. Usa Crear evento para registrar el primero; el calendario aparecera cuando exista al menos un evento.'
      : 'No hay eventos sociales por venir. Inicia sesion para crear el primero; el calendario aparecera cuando exista al menos un evento.',
    showFilters: false,
    showCalendar: false,
  };
}
