import {
  defaultMinutesForService,
  describeServiceDefaults,
  getBookingCalendarStatusState,
  getBookingConflictAlertText,
  getBookingCustomerFieldState,
  requiresEngineerForService,
  shouldShowQuickBookingTemplate,
} from './bookingsPageLogic';

describe('bookingsPageLogic', () => {
  it('keeps the service prompt generic until a service is chosen', () => {
    expect(describeServiceDefaults('')).toBe('Elige el tipo de servicio asociado a la sesión.');
  });

  it('describes vocal recording defaults with room, duration, and engineer guidance', () => {
    expect(defaultMinutesForService('Vocal recording')).toBe(120);
    expect(requiresEngineerForService('Vocal recording')).toBe(true);
    expect(describeServiceDefaults('Vocal recording')).toBe(
      'Salas sugeridas: Vocal Booth + Control Room · Duración sugerida: 120 min · Se sugerirá un ingeniero',
    );
  });

  it('describes rehearsals without adding engineer guidance', () => {
    expect(defaultMinutesForService('Band rehearsal')).toBe(90);
    expect(requiresEngineerForService('Band rehearsal')).toBe(false);
    expect(describeServiceDefaults('Band rehearsal')).toBe(
      'Salas sugeridas: Live Room · Duración sugerida: 90 min',
    );
  });

  it('uses first-contact copy when the customer catalog is still empty', () => {
    expect(getBookingCustomerFieldState({ customerCount: 0, selectedCustomerId: null })).toEqual({
      helperText: 'Todavía no hay clientes guardados. Agrega el primero sin salir de esta sesión.',
      dialogTitle: 'Agregar primer contacto',
      quickCreateLabel: 'Agregar primer contacto',
      showQuickCreateAction: true,
    });
  });

  it('keeps one create-contact action available until a customer is selected', () => {
    expect(getBookingCustomerFieldState({ customerCount: 4, selectedCustomerId: null })).toEqual({
      helperText: 'Selecciona un cliente guardado. Si todavía no existe, créalo aquí.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateAction: true,
    });
  });

  it('hides the extra create-contact action after a customer is already assigned', () => {
    expect(getBookingCustomerFieldState({ customerCount: 4, selectedCustomerId: 12 })).toEqual({
      helperText: 'Cliente asignado. Cambia la selección solo si necesitas reemplazarlo.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateAction: false,
    });
  });

  it('keeps quick booking templates limited to unlocked create flows', () => {
    expect(shouldShowQuickBookingTemplate({ mode: 'create', serviceLocked: false })).toBe(true);
    expect(shouldShowQuickBookingTemplate({ mode: 'edit', serviceLocked: false })).toBe(false);
    expect(shouldShowQuickBookingTemplate({ mode: 'create', serviceLocked: true })).toBe(false);
  });

  it('keeps room conflict guidance in one specific warning with a capped conflict list', () => {
    expect(getBookingConflictAlertText([])).toBeNull();
    expect(getBookingConflictAlertText(['Mix principal', 'Ensayo tarde', null, 'Podcast'])).toBe(
      'Conflicto con 4 reserva(s): Mix principal, Ensayo tarde, reserva. Ajusta horario o salas.',
    );
  });

  it('replaces generic loading chrome with first-calendar guidance', () => {
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: true,
    })).toEqual({
      message: 'Cargando agenda… El calendario quedará listo para crear sesiones cuando termine esta primera carga.',
      severity: 'info',
    });
  });

  it('guides first-time calendar setup without adding fallback chrome once bookings exist', () => {
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
    })).toEqual({
      message: 'Todavía no hay sesiones. Selecciona un horario en el calendario para crear la primera; luego esta vista servirá para mover, editar y revisar conflictos.',
      severity: 'info',
    });

    expect(getBookingCalendarStatusState({
      bookingCount: 1,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
    })).toBeNull();
  });

  it('uses filtered-empty calendar guidance with one agenda reset action only when an active booking filter has no sessions', () => {
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: true,
      hasError: false,
      isLoading: false,
    })).toEqual({
      clearFilterActionLabel: 'Ver toda la agenda',
      message: 'No hay sesiones para este filtro. Selecciona un horario en el calendario para crear una sesión nueva.',
      severity: 'info',
    });
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
    })?.clearFilterActionLabel).toBeUndefined();
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: true,
      hasError: true,
      isLoading: false,
    })).toBeNull();
  });
});
