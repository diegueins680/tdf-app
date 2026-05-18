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
  createFormOpen: boolean;
}

interface SocialEventsCreateUiState {
  createFormDescription: string;
  showCreateForm: boolean;
  showCreateToolbarAction: boolean;
}

const FIRST_EVENT_CREATE_DESCRIPTION =
  'Crea el primer evento aqui; cuando exista al menos uno, apareceran los filtros y el calendario.';
const FOLLOW_UP_EVENT_CREATE_DESCRIPTION =
  'Completa solo los campos necesarios para registrar otro evento. La lista actual se mantiene debajo.';

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
      showRefreshAction: true,
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
  createFormOpen,
}: SocialEventsCreateUiStateInput): SocialEventsCreateUiState {
  const isFirstRun = eventCount === 0 && !filtersActive;

  return {
    createFormDescription: isFirstRun
      ? FIRST_EVENT_CREATE_DESCRIPTION
      : FOLLOW_UP_EVENT_CREATE_DESCRIPTION,
    showCreateForm: canCreateEvent && (isFirstRun || createFormOpen),
    showCreateToolbarAction: canCreateEvent && !isFirstRun && !createFormOpen,
  };
}
