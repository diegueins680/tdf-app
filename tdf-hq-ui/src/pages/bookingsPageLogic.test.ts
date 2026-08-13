import {
  getBookingCalendarStatusState,
  getBookingConflictAlertText,
  getBookingCustomerFieldState,
  getBookingEngineerFieldState,
  getBookingOptionalDetailsState,
  getBookingRoomsFieldState,
  getBookingServiceEntryGateState,
  getBookingServiceFieldState,
} from './bookingsPageLogic';

describe('bookingsPageLogic', () => {
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

  it('keeps service entry canonical when the published catalog is empty', () => {
    expect(getBookingServiceFieldState({
      hasServiceCatalog: false,
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toEqual({
      helperText: 'No hay servicios publicados. Publica una oferta desde Catálogos antes de guardar una sesión.',
      mode: 'catalog',
    });

    expect(getBookingServiceFieldState({
      hasServiceCatalog: false,
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toEqual({
      helperText: 'No hay servicios publicados. Publica una oferta desde Catálogos antes de guardar una sesión.',
      mode: 'catalog',
    });
  });

  it('keeps the catalog selector once services are available or still loading', () => {
    expect(getBookingServiceFieldState({
      hasServiceCatalog: true,
      serviceCatalogReady: true,
      serviceLocked: false,
    })).toEqual({
      helperText: '',
      mode: 'catalog',
    });

    expect(getBookingServiceFieldState({
      hasServiceCatalog: false,
      serviceCatalogReady: false,
      serviceLocked: false,
    })).toEqual({
      helperText: 'Cargando catálogo de servicios…',
      mode: 'catalog',
    });
  });

  it('keeps initial service-catalog loading to one notice before showing dependent fields', () => {
    expect(getBookingServiceEntryGateState({
      serviceCatalogReady: false,
      serviceLocked: false,
      serviceOfferingId: '',
    })).toEqual({
      helperText: 'Cargando catálogo de servicios… En cuanto termine esta primera carga podrás seleccionar el servicio.',
      showDependentFields: false,
      showServiceField: false,
    });

    expect(getBookingServiceEntryGateState({
      serviceCatalogReady: true,
      serviceLocked: false,
      serviceOfferingId: '',
    })).toEqual({
      helperText: '',
      showDependentFields: true,
      showServiceField: true,
    });

    expect(getBookingServiceEntryGateState({
      serviceCatalogReady: false,
      serviceLocked: false,
      serviceOfferingId: '11111111-1111-4111-8111-111111111111',
    })).toEqual({
      helperText: '',
      showDependentFields: true,
      showServiceField: true,
    });

    expect(getBookingServiceEntryGateState({
      serviceCatalogReady: false,
      serviceLocked: true,
      serviceOfferingId: '',
    })).toEqual({
      helperText: '',
      showDependentFields: true,
      showServiceField: true,
    });
  });

  it('hides engineer selection until the operator chooses a service or keeps an assigned engineer', () => {
    expect(getBookingEngineerFieldState({
      engineerCount: 2,
      hasAssignedEngineer: false,
      hasSelectedService: false,
      requiresEngineer: false,
    })).toEqual({
      helperText: 'Selecciona el servicio primero para decidir si hace falta un ingeniero.',
      label: 'Ingeniero',
      showField: false,
    });

    expect(getBookingEngineerFieldState({
      engineerCount: 2,
      hasAssignedEngineer: true,
      hasSelectedService: false,
      requiresEngineer: false,
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
      hasSelectedService: true,
      requiresEngineer: true,
    })).toEqual({
      helperText: 'Recomendado para recording/mixing/mastering.',
      label: 'Ingeniero',
      showField: true,
    });

    expect(getBookingEngineerFieldState({
      engineerCount: 2,
      hasAssignedEngineer: false,
      hasSelectedService: true,
      requiresEngineer: false,
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
      hasSelectedService: true,
      requiresEngineer: false,
    })).toEqual({
      helperText: '',
      label: 'Ingeniero',
      showField: false,
    });

    expect(getBookingEngineerFieldState({
      engineerCount: 0,
      hasAssignedEngineer: false,
      hasSelectedService: true,
      requiresEngineer: true,
    })).toEqual({
      helperText: 'Todavía no hay ingenieros en el catálogo de contactos. Continúa sin asignar uno o agrégalo después.',
      label: 'Ingeniero',
      showField: false,
    });

    expect(getBookingEngineerFieldState({
      engineerCount: 0,
      hasAssignedEngineer: true,
      hasSelectedService: true,
      requiresEngineer: true,
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
      hasSelectedService: false,
    })).toEqual({
      helperText: 'Cargando salas disponibles… En cuanto termine esta primera carga podrás asignarlas aquí.',
      showField: false,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: false,
      roomCatalogLoading: false,
      roomCount: 0,
      hasSelectedService: false,
    })).toEqual({
      helperText: 'Todavía no hay salas registradas. Crea la primera en Salas y recursos para poder guardar sesiones.',
      setupActionLabel: 'Abrir salas y recursos',
      showField: false,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: false,
      roomCatalogLoading: false,
      roomCount: 2,
      hasSelectedService: false,
    })).toEqual({
      helperText: 'Selecciona el servicio primero para sugerir salas y ajustar la combinación si hace falta.',
      showField: false,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: true,
      roomCatalogLoading: false,
      roomCount: 2,
      hasSelectedService: false,
    })).toEqual({
      helperText: 'Ajusta las salas solo si esta sesión necesita otra combinación.',
      showField: true,
    });

    expect(getBookingRoomsFieldState({
      hasAssignedRooms: false,
      roomCatalogLoading: false,
      roomCount: 2,
      hasSelectedService: true,
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
    expect(getBookingConflictAlertText(['Mix principal'])).toBe(
      'Conflicto con 1 reserva: Mix principal. Ajusta horario o salas.',
    );
    expect(getBookingConflictAlertText(['Mix principal', 'Mix principal', 'Ensayo tarde', null, 'Podcast'])).toBe(
      'Conflicto con 5 reservas: Mix principal (2), Ensayo tarde, reserva y 1 más. Ajusta horario o salas.',
    );
  });

  it('replaces generic loading chrome with first-calendar guidance', () => {
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: true,
      roomCatalogLoading: true,
      roomCount: 0,
    })).toEqual({
      message: 'Cargando agenda… El calendario quedará listo para crear sesiones cuando termine esta primera carga.',
      severity: 'info',
      showCalendar: false,
      title: 'Preparando agenda.',
    });
  });

  it('guides first-time setup with one explicit create action instead of duplicate action copy', () => {
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 1,
    })).toEqual({
      message: 'Cuando exista al menos una sesión, la agenda semanal servirá para mover, editar y revisar conflictos.',
      primaryActionLabel: 'Crear primera sesión',
      severity: 'info',
      showCalendar: false,
      title: 'Todavía no hay sesiones.',
    });
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 1,
    })?.message).not.toContain('Crea la primera sesión');

    expect(getBookingCalendarStatusState({
      bookingCount: 1,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 0,
    })).toBeNull();
  });

  it('guides room setup before opening the first booking form', () => {
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: true,
      roomCount: 0,
    })).toEqual({
      message: 'Cargando salas disponibles… En cuanto termine esta primera carga podrás crear la primera sesión.',
      severity: 'info',
      showCalendar: false,
      title: 'Preparando salas.',
    });

    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 0,
    })).toEqual({
      message: 'Todavía no hay salas registradas. Crea la primera en Salas y recursos antes de agendar sesiones.',
      primaryActionHref: '/estudio/salas',
      primaryActionLabel: 'Abrir salas y recursos',
      severity: 'info',
      showCalendar: false,
      title: 'Configura salas antes de agendar.',
    });
  });

  it('uses one reset-focused empty state when an active booking filter has no sessions', () => {
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: true,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 0,
    })).toEqual({
      clearFilterActionLabel: 'Ver toda la agenda',
      message: 'No hay sesiones para este filtro. Vuelve a toda la agenda para revisar el calendario completo.',
      severity: 'info',
      showCalendar: false,
      title: 'No hay sesiones en esta vista.',
    });
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: true,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 0,
    })?.message).not.toContain('crear una sesión nueva');
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: true,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 0,
    })?.primaryActionLabel).toBeUndefined();
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: false,
      hasError: false,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 1,
    })?.clearFilterActionLabel).toBeUndefined();
    expect(getBookingCalendarStatusState({
      bookingCount: 0,
      hasActiveFilter: true,
      hasError: true,
      isLoading: false,
      roomCatalogLoading: false,
      roomCount: 0,
    })).toBeNull();
  });
});
