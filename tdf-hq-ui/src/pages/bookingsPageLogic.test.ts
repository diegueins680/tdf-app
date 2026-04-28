import {
  defaultMinutesForService,
  describeServiceDefaults,
  getBookingCalendarStatusState,
  getBookingConflictAlertText,
  getBookingCustomerFieldState,
  getBookingEngineerFieldState,
  getBookingOptionalDetailsState,
  getBookingRoomsFieldState,
  getBookingServiceFallbackEntryState,
  getBookingServiceFieldState,
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
    expect(getBookingCustomerFieldState({
      customerCount: 0,
      customerCatalogLoading: false,
      selectedCustomerId: null,
    })).toEqual({
      helperText: 'Todavía no hay clientes guardados. Agrega el primero sin salir de esta sesión.',
      dialogTitle: 'Agregar primer cliente',
      quickCreateLabel: 'Agregar primer cliente',
      showQuickCreateInsideAlert: true,
      showCustomerSelector: false,
      showQuickCreateAction: true,
    });
  });

  it('keeps quick contact creation hidden while the customer catalog is still loading', () => {
    expect(getBookingCustomerFieldState({
      customerCount: 0,
      customerCatalogLoading: true,
      selectedCustomerId: null,
    })).toEqual({
      helperText: 'Cargando clientes guardados… Espera un momento antes de crear un contacto nuevo para evitar duplicados.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateInsideAlert: false,
      showCustomerSelector: false,
      showQuickCreateAction: false,
    });
  });

  it('keeps one create-contact action available until a customer is selected', () => {
    expect(getBookingCustomerFieldState({
      customerCount: 4,
      customerCatalogLoading: false,
      selectedCustomerId: null,
    })).toEqual({
      helperText: 'Selecciona un cliente guardado. Si todavía no existe, créalo aquí.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateInsideAlert: false,
      showCustomerSelector: true,
      showQuickCreateAction: true,
    });
  });

  it('hides the extra create-contact action after a customer is already assigned', () => {
    expect(getBookingCustomerFieldState({
      customerCount: 4,
      customerCatalogLoading: false,
      selectedCustomerId: 12,
    })).toEqual({
      helperText: 'Cliente asignado. Cambia la selección solo si necesitas reemplazarlo.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateInsideAlert: false,
      showCustomerSelector: true,
      showQuickCreateAction: false,
    });
  });

  it('keeps quick booking templates as a fallback instead of duplicating the service catalog', () => {
    expect(shouldShowQuickBookingTemplate({
      hasServiceCatalog: false,
      mode: 'create',
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toBe(true);
    expect(shouldShowQuickBookingTemplate({
      hasServiceCatalog: true,
      mode: 'create',
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toBe(false);
    expect(shouldShowQuickBookingTemplate({
      hasServiceCatalog: false,
      mode: 'create',
      serviceCatalogReady: false,
      serviceLocked: false,
    })).toBe(false);
    expect(shouldShowQuickBookingTemplate({
      hasServiceCatalog: false,
      mode: 'edit',
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toBe(false);
    expect(shouldShowQuickBookingTemplate({
      hasServiceCatalog: false,
      mode: 'create',
      serviceCatalogReady: true,
      serviceLocked: true,
    })).toBe(false);
  });

  it('switches the service field to manual entry when the catalog fallback is active', () => {
    expect(getBookingServiceFieldState({
      hasServiceCatalog: false,
      mode: 'create',
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toEqual({
      helperText: 'Todavía no hay catálogo de servicios. Usa una plantilla de respaldo o escribe el servicio manualmente.',
      mode: 'manual',
    });

    expect(getBookingServiceFieldState({
      hasServiceCatalog: false,
      mode: 'edit',
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toEqual({
      helperText: 'Todavía no hay catálogo de servicios. Escribe el servicio manualmente para actualizar la sesión.',
      mode: 'manual',
    });
  });

  it('keeps the no-catalog fallback focused on one service-entry path at a time', () => {
    expect(getBookingServiceFallbackEntryState({
      fallbackTemplatesActive: false,
      manualEntryRequested: false,
    })).toEqual({
      showManualEntryField: true,
      showManualEntryToggle: false,
      showTemplateField: false,
      templateHelperText: '',
    });

    expect(getBookingServiceFallbackEntryState({
      fallbackTemplatesActive: true,
      manualEntryRequested: false,
    })).toEqual({
      manualEntryToggleLabel: 'Escribir servicio manualmente',
      showManualEntryField: false,
      showManualEntryToggle: true,
      showTemplateField: true,
      templateHelperText: 'Usa una plantilla para precargar servicio, salas y notas. Si no aplica, abre la entrada manual.',
    });

    expect(getBookingServiceFallbackEntryState({
      fallbackTemplatesActive: true,
      manualEntryRequested: true,
    })).toEqual({
      showManualEntryField: true,
      showManualEntryToggle: false,
      showTemplateField: false,
      templateReturnActionLabel: 'Volver a plantillas',
      templateHelperText: '',
    });
  });

  it('keeps the catalog selector once services are available or still loading', () => {
    expect(getBookingServiceFieldState({
      hasServiceCatalog: true,
      mode: 'create',
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toEqual({
      helperText: '',
      mode: 'catalog',
    });

    expect(getBookingServiceFieldState({
      hasServiceCatalog: false,
      mode: 'create',
      serviceCatalogReady: false,
      serviceLocked: false,
    })).toEqual({
      helperText: '',
      mode: 'catalog',
    });
  });

  it('hides engineer selection until the operator chooses a service or keeps an assigned engineer', () => {
    expect(getBookingEngineerFieldState({
      engineerCount: 2,
      hasAssignedEngineer: false,
      serviceType: '',
    })).toEqual({
      helperText: 'Selecciona el servicio primero para decidir si hace falta un ingeniero.',
      label: 'Ingeniero',
      showField: false,
    });

    expect(getBookingEngineerFieldState({
      engineerCount: 2,
      hasAssignedEngineer: true,
      serviceType: '',
    })).toEqual({
      helperText: 'Opcional.',
      label: 'Ingeniero',
      showField: true,
    });
  });

  it('keeps engineer guidance concise and contextual once the service is known', () => {
    expect(getBookingEngineerFieldState({
      engineerCount: 2,
      hasAssignedEngineer: false,
      serviceType: 'Mixing',
    })).toEqual({
      helperText: 'Recomendado para recording/mixing/mastering.',
      label: 'Ingeniero',
      showField: true,
    });

    expect(getBookingEngineerFieldState({
      engineerCount: 2,
      hasAssignedEngineer: false,
      serviceType: 'Band rehearsal',
    })).toEqual({
      helperText: 'Opcional.',
      label: 'Ingeniero',
      showField: true,
    });
  });

  it('hides the empty engineer picker until the catalog actually has someone to choose', () => {
    expect(getBookingEngineerFieldState({
      engineerCount: 0,
      hasAssignedEngineer: false,
      serviceType: 'Mixing',
    })).toEqual({
      helperText: 'Todavía no hay ingenieros en el catálogo de contactos. Continúa sin asignar uno o agrégalo después.',
      label: 'Ingeniero',
      showField: false,
    });

    expect(getBookingEngineerFieldState({
      engineerCount: 0,
      hasAssignedEngineer: true,
      serviceType: 'Mixing',
    })).toEqual({
      helperText: 'No hay ingenieros en el catálogo de contactos. Conserva el nombre actual o actualiza contactos para volver a seleccionarlo.',
      label: 'Ingeniero',
      showField: true,
    });
  });

  it('replaces the empty room picker with first-run setup guidance until rooms exist', () => {
    expect(getBookingRoomsFieldState({
      hasAssignedRooms: false,
      roomCatalogLoading: true,
      roomCount: 0,
      serviceType: '',
    })).toEqual({
      helperText: 'Cargando salas disponibles… En cuanto termine esta primera carga podrás asignarlas aquí.',
      showField: false,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: false,
      roomCatalogLoading: false,
      roomCount: 0,
      serviceType: '',
    })).toEqual({
      helperText: 'Todavía no hay salas registradas. Crea la primera en Salas y recursos para poder guardar sesiones.',
      setupActionLabel: 'Abrir salas y recursos',
      showField: false,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: false,
      roomCatalogLoading: false,
      roomCount: 2,
      serviceType: '',
    })).toEqual({
      helperText: 'Selecciona el servicio primero para sugerir salas y ajustar la combinación si hace falta.',
      showField: false,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: true,
      roomCatalogLoading: false,
      roomCount: 2,
      serviceType: '',
    })).toEqual({
      helperText: 'Ajusta las salas solo si esta sesión necesita otra combinación.',
      showField: true,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: false,
      roomCatalogLoading: false,
      roomCount: 2,
      serviceType: 'Recording',
    })).toEqual({
      helperText: 'Se precargan según el tipo de servicio.',
      showField: true,
    });
  });

  it('keeps notes and status collapsed until a session needs extra context', () => {
    expect(getBookingOptionalDetailsState({
      mode: 'create',
      notes: '',
      status: 'Confirmed',
    })).toEqual({
      collapsedHelperText: 'Opcional. Déjalo cerrado para una sesión estándar confirmada.',
      defaultExpanded: false,
      toggleLabel: 'Agregar notas o cambiar estado',
    });

    expect(getBookingOptionalDetailsState({
      mode: 'create',
      notes: 'Cliente pidió backline extra',
      status: 'Confirmed',
    }).defaultExpanded).toBe(true);

    expect(getBookingOptionalDetailsState({
      mode: 'create',
      notes: '',
      status: 'Tentative',
    }).defaultExpanded).toBe(true);

    expect(getBookingOptionalDetailsState({
      mode: 'edit',
      notes: '',
      status: 'Confirmed',
    }).defaultExpanded).toBe(true);
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
      message: 'No hay sesiones para este filtro. Vuelve a toda la agenda para revisar el calendario completo.',
      severity: 'info',
    });
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: true,
      hasError: false,
      isLoading: false,
    })?.message).not.toContain('crear una sesión nueva');
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
