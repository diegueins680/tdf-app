interface BookingCustomerFieldState {
  helperText: string;
  dialogTitle: string;
  quickCreateLabel: string;
  showQuickCreateInsideAlert: boolean;
  showCustomerSelector: boolean;
  showQuickCreateAction: boolean;
}

interface BookingCalendarStatusState {
  clearFilterActionLabel?: string;
  message: string;
  primaryActionHref?: string;
  primaryActionLabel?: string;
  severity: 'info';
  showCalendar: boolean;
  title?: string;
}

interface BookingServiceFieldState {
  helperText: string;
  mode: 'catalog';
}

interface BookingServiceEntryGateState {
  helperText: string;
  showDependentFields: boolean;
  showServiceField: boolean;
}

interface BookingEngineerFieldState {
  helperText: string;
  label: string;
  showField: boolean;
}

interface BookingRoomsFieldState {
  helperText: string;
  setupActionLabel?: string;
  showField: boolean;
}

interface BookingOptionalDetailsState {
  collapsedHelperText: string;
  defaultExpanded: boolean;
  toggleLabel: string;
}

export const getBookingCustomerFieldState = ({
  customerCount,
  customerCatalogLoading,
  selectedCustomerId,
}: {
  customerCount: number;
  customerCatalogLoading: boolean;
  selectedCustomerId: number | null;
}): BookingCustomerFieldState => {
  if (selectedCustomerId != null) {
    return {
      helperText: 'Cliente asignado. Cambia la selección solo si necesitas reemplazarlo.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateInsideAlert: false,
      showCustomerSelector: true,
      showQuickCreateAction: false,
    };
  }

  if (customerCatalogLoading) {
    return {
      helperText: 'Cargando clientes guardados… Espera un momento antes de crear un contacto nuevo para evitar duplicados.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateInsideAlert: false,
      showCustomerSelector: false,
      showQuickCreateAction: false,
    };
  }

  if (customerCount <= 0) {
    return {
      helperText: 'Todavía no hay clientes guardados. Agrega el primero sin salir de esta sesión.',
      dialogTitle: 'Agregar primer cliente',
      quickCreateLabel: 'Agregar primer cliente',
      showQuickCreateInsideAlert: true,
      showCustomerSelector: false,
      showQuickCreateAction: true,
    };
  }

  return {
    helperText: 'Selecciona un cliente guardado. Si todavía no existe, créalo aquí.',
    dialogTitle: 'Nuevo contacto',
    quickCreateLabel: 'Crear contacto nuevo',
    showQuickCreateInsideAlert: false,
    showCustomerSelector: true,
    showQuickCreateAction: true,
  };
};

export const getBookingCalendarStatusState = ({
  bookingCount,
  hasActiveFilter,
  hasError,
  isLoading,
  roomCatalogLoading,
  roomCount,
}: {
  bookingCount: number;
  hasActiveFilter: boolean;
  hasError: boolean;
  isLoading: boolean;
  roomCatalogLoading: boolean;
  roomCount: number;
}): BookingCalendarStatusState | null => {
  if (hasError) return null;

  if (isLoading) {
    return {
      message: 'Cargando agenda… El calendario quedará listo para crear sesiones cuando termine esta primera carga.',
      severity: 'info',
      showCalendar: false,
      title: 'Preparando agenda.',
    };
  }

  if (bookingCount > 0) return null;

  if (hasActiveFilter) {
    return {
      clearFilterActionLabel: 'Ver toda la agenda',
      message: 'No hay sesiones para este filtro. Vuelve a toda la agenda para revisar el calendario completo.',
      severity: 'info',
      showCalendar: false,
      title: 'No hay sesiones en esta vista.',
    };
  }

  if (roomCatalogLoading) {
    return {
      message: 'Cargando salas disponibles… En cuanto termine esta primera carga podrás crear la primera sesión.',
      severity: 'info',
      showCalendar: false,
      title: 'Preparando salas.',
    };
  }

  if (roomCount <= 0) {
    return {
      message: 'Todavía no hay salas registradas. Crea la primera en Salas y recursos antes de agendar sesiones.',
      primaryActionHref: '/estudio/salas',
      primaryActionLabel: 'Abrir salas y recursos',
      severity: 'info',
      showCalendar: false,
      title: 'Configura salas antes de agendar.',
    };
  }

  return {
    message: 'Cuando exista al menos una sesión, la agenda semanal servirá para mover, editar y revisar conflictos.',
    primaryActionLabel: 'Crear primera sesión',
    severity: 'info',
    showCalendar: false,
    title: 'Todavía no hay sesiones.',
  };
};

export const getBookingServiceFieldState = ({
  hasServiceCatalog,
  serviceCatalogReady,
  serviceLocked,
}: {
  hasServiceCatalog: boolean;
  serviceCatalogReady: boolean;
  serviceLocked: boolean;
}): BookingServiceFieldState => {
  return {
    helperText: !serviceCatalogReady
      ? 'Cargando catálogo de servicios…'
      : hasServiceCatalog || serviceLocked
        ? ''
        : 'No hay servicios publicados. Publica una oferta desde Catálogos antes de guardar una sesión.',
    mode: 'catalog',
  };
};

export const getBookingServiceEntryGateState = ({
  serviceCatalogReady,
  serviceLocked,
  serviceOfferingId,
}: {
  serviceCatalogReady: boolean;
  serviceLocked: boolean;
  serviceOfferingId: string;
}): BookingServiceEntryGateState => {
  if (serviceCatalogReady || serviceLocked || serviceOfferingId.trim() !== '') {
    return {
      helperText: '',
      showDependentFields: true,
      showServiceField: true,
    };
  }

  return {
    helperText: 'Cargando catálogo de servicios… En cuanto termine esta primera carga podrás seleccionar el servicio.',
    showDependentFields: false,
    showServiceField: false,
  };
};

export const getBookingEngineerFieldState = ({
  engineerCount,
  hasAssignedEngineer,
  hasSelectedService,
  requiresEngineer,
}: {
  engineerCount: number;
  hasAssignedEngineer: boolean;
  hasSelectedService: boolean;
  requiresEngineer: boolean;
}): BookingEngineerFieldState => {
  if (!hasAssignedEngineer && !hasSelectedService) {
    return {
      helperText: 'Selecciona el servicio primero para decidir si hace falta un ingeniero.',
      label: 'Ingeniero',
      showField: false,
    };
  }

  if (engineerCount === 0) {
    if (!hasAssignedEngineer && !requiresEngineer) {
      return {
        helperText: '',
        label: 'Ingeniero',
        showField: false,
      };
    }

    return {
      helperText: hasAssignedEngineer
        ? 'No hay ingenieros en el catálogo de contactos. Conserva el nombre actual o actualiza contactos para volver a seleccionarlo.'
        : 'Todavía no hay ingenieros en el catálogo de contactos. Continúa sin asignar uno o agrégalo después.',
      label: 'Ingeniero',
      showField: hasAssignedEngineer,
    };
  }

  if (requiresEngineer) {
    return {
      helperText: 'Recomendado para recording/mixing/mastering.',
      label: 'Ingeniero',
      showField: true,
    };
  }

  return {
    helperText: 'Opcional.',
    label: 'Ingeniero',
    showField: true,
  };
};

export const getBookingRoomsFieldState = ({
  hasAssignedRooms,
  hasSelectedService,
  roomCatalogLoading,
  roomCount,
}: {
  hasAssignedRooms: boolean;
  hasSelectedService: boolean;
  roomCatalogLoading: boolean;
  roomCount: number;
}): BookingRoomsFieldState => {
  if (roomCatalogLoading && roomCount <= 0) {
    return {
      helperText: 'Cargando salas disponibles… En cuanto termine esta primera carga podrás asignarlas aquí.',
      showField: false,
    };
  }

  if (roomCount <= 0) {
    return {
      helperText: 'Todavía no hay salas registradas. Crea la primera en Salas y recursos para poder guardar sesiones.',
      setupActionLabel: 'Abrir salas y recursos',
      showField: false,
    };
  }

  if (!hasAssignedRooms && !hasSelectedService) {
    return {
      helperText: 'Selecciona el servicio primero para sugerir salas y ajustar la combinación si hace falta.',
      showField: false,
    };
  }

  if (hasAssignedRooms && !hasSelectedService) {
    return {
      helperText: 'Ajusta las salas solo si esta sesión necesita otra combinación.',
      showField: true,
    };
  }

  return {
    helperText: 'Se precargan según el tipo de servicio.',
    showField: true,
  };
};

export const getBookingOptionalDetailsState = ({
  mode,
  notes,
  status,
}: {
  mode: 'create' | 'edit';
  notes: string;
  status: string;
}): BookingOptionalDetailsState => {
  const hasNotes = notes.trim() !== '';
  const normalizedStatus = status.trim().toLowerCase();
  const usesDefaultStatus = normalizedStatus === '' || normalizedStatus === 'confirmed';

  if (mode === 'edit' || hasNotes || !usesDefaultStatus) {
    return {
      collapsedHelperText: '',
      defaultExpanded: true,
      toggleLabel: 'Agregar notas o cambiar estado',
    };
  }

  return {
    collapsedHelperText: 'Opcional. Déjalo cerrado para una sesión estándar confirmada.',
    defaultExpanded: false,
    toggleLabel: 'Agregar notas o cambiar estado',
  };
};

export const getBookingConflictAlertText = (conflictTitles: (string | null | undefined)[]) => {
  if (conflictTitles.length === 0) return null;

  const labelCounts = new Map<string, number>();
  const labels = conflictTitles.map((title) => {
    const trimmed = title?.trim();
    return trimmed && trimmed.length > 0 ? trimmed : 'reserva';
  });
  labels.forEach((label) => {
    labelCounts.set(label, (labelCounts.get(label) ?? 0) + 1);
  });

  const visibleLabels = Array.from(labelCounts.entries())
    .slice(0, 3)
    .map(([label, count]) => (count > 1 ? `${label} (${count})` : label));
  const hiddenLabelCount = Math.max(0, labelCounts.size - visibleLabels.length);
  const visibleSummaryParts = hiddenLabelCount > 0
    ? [...visibleLabels, `${hiddenLabelCount} más`]
    : visibleLabels;
  const visibleTitles = visibleSummaryParts.length <= 1
    ? visibleSummaryParts[0] ?? 'reserva'
    : `${visibleSummaryParts.slice(0, -1).join(', ')} y ${visibleSummaryParts[visibleSummaryParts.length - 1]}`;
  const reservationCount = `${conflictTitles.length} reserva${conflictTitles.length === 1 ? '' : 's'}`;

  return `Conflicto con ${reservationCount}: ${visibleTitles}. Ajusta horario o salas.`;
};
