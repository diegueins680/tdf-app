const normalizeServiceType = (serviceType: string) => serviceType.trim().toLowerCase();

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
  severity: 'info';
}

interface BookingServiceFieldState {
  helperText: string;
  mode: 'catalog' | 'manual';
}

interface BookingServiceFallbackEntryState {
  manualEntryToggleLabel?: string;
  showManualEntryField: boolean;
  showManualEntryToggle: boolean;
  showTemplateField: boolean;
  templateReturnActionLabel?: string;
  templateHelperText: string;
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

export const requiresEngineerForService = (serviceType: string) => {
  const lowered = normalizeServiceType(serviceType);
  return ['recording', 'grabacion', 'grabación', 'mezcla', 'mixing', 'master', 'mastering'].some((keyword) =>
    lowered.includes(keyword),
  );
};

export const defaultMinutesForService = (serviceType: string) => {
  const lowered = normalizeServiceType(serviceType);
  if (lowered.includes('trial') || lowered.includes('lesson') || lowered.includes('clase')) return 60;
  if (lowered.includes('rehearsal') || lowered.includes('ensayo')) return 90;
  if (lowered.includes('mix') || lowered.includes('master')) return 120;
  if (lowered.includes('record') || lowered.includes('grab')) return 120;
  return 60;
};

const suggestedRoomPreset = (serviceType: string) => {
  const lowered = normalizeServiceType(serviceType);
  if (!lowered) return null;
  if (lowered.includes('audiovisual') && lowered.includes('live')) return 'Live Room + Control Room';
  if (lowered.includes('dj')) return 'DJ Booth';
  if (lowered.includes('band') && lowered.includes('record')) return 'Live Room + Control Room';
  if (lowered.includes('vocal') && lowered.includes('record')) return 'Vocal Booth + Control Room';
  if (lowered.includes('rehearsal') || lowered.includes('ensayo') || (lowered.includes('band') && lowered.includes('rehe'))) {
    return 'Live Room';
  }
  if (lowered.includes('mix') || lowered.includes('master')) return 'Control Room';
  if (lowered.includes('record')) return 'Control Room + Live Room';
  return 'primera sala disponible';
};

export const describeServiceDefaults = (serviceType: string) => {
  if (normalizeServiceType(serviceType) === '') {
    return 'Elige el tipo de servicio asociado a la sesión.';
  }

  const details = [
    `Salas sugeridas: ${suggestedRoomPreset(serviceType)}`,
    `Duración sugerida: ${defaultMinutesForService(serviceType)} min`,
  ];

  if (requiresEngineerForService(serviceType)) {
    details.push('Se sugerirá un ingeniero');
  }

  return details.join(' · ');
};

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
}: {
  bookingCount: number;
  hasActiveFilter: boolean;
  hasError: boolean;
  isLoading: boolean;
}): BookingCalendarStatusState | null => {
  if (hasError) return null;

  if (isLoading) {
    return {
      message: 'Cargando agenda… El calendario quedará listo para crear sesiones cuando termine esta primera carga.',
      severity: 'info',
    };
  }

  if (bookingCount > 0) return null;

  if (hasActiveFilter) {
    return {
      clearFilterActionLabel: 'Ver toda la agenda',
      message: 'No hay sesiones para este filtro. Selecciona un horario en el calendario para crear una sesión nueva.',
      severity: 'info',
    };
  }

  return {
    message: 'Todavía no hay sesiones. Selecciona un horario en el calendario para crear la primera; luego esta vista servirá para mover, editar y revisar conflictos.',
    severity: 'info',
  };
};

export const shouldShowQuickBookingTemplate = ({
  hasServiceCatalog,
  mode,
  serviceCatalogReady,
  serviceLocked,
}: {
  hasServiceCatalog: boolean;
  mode: 'create' | 'edit';
  serviceCatalogReady: boolean;
  serviceLocked: boolean;
}) => mode === 'create' && !serviceLocked && serviceCatalogReady && !hasServiceCatalog;

export const getBookingServiceFieldState = ({
  hasServiceCatalog,
  mode,
  serviceCatalogReady,
  serviceLocked,
}: {
  hasServiceCatalog: boolean;
  mode: 'create' | 'edit';
  serviceCatalogReady: boolean;
  serviceLocked: boolean;
}): BookingServiceFieldState => {
  if (serviceLocked || hasServiceCatalog || !serviceCatalogReady) {
    return {
      helperText: '',
      mode: 'catalog',
    };
  }

  if (mode === 'create') {
    return {
      helperText: 'Todavía no hay catálogo de servicios. Usa una plantilla de respaldo o escribe el servicio manualmente.',
      mode: 'manual',
    };
  }

  return {
    helperText: 'Todavía no hay catálogo de servicios. Escribe el servicio manualmente para actualizar la sesión.',
    mode: 'manual',
  };
};

export const getBookingServiceFallbackEntryState = ({
  fallbackTemplatesActive,
  manualEntryRequested,
}: {
  fallbackTemplatesActive: boolean;
  manualEntryRequested: boolean;
}): BookingServiceFallbackEntryState => {
  if (!fallbackTemplatesActive) {
    return {
      showManualEntryField: true,
      showManualEntryToggle: false,
      showTemplateField: false,
      templateHelperText: '',
    };
  }

  if (manualEntryRequested) {
    return {
      showManualEntryField: true,
      showManualEntryToggle: false,
      showTemplateField: false,
      templateReturnActionLabel: 'Volver a plantillas',
      templateHelperText: '',
    };
  }

  return {
    manualEntryToggleLabel: 'Escribir servicio manualmente',
    showManualEntryField: false,
    showManualEntryToggle: true,
    showTemplateField: true,
    templateHelperText: 'Usa una plantilla para precargar servicio, salas y notas. Si no aplica, abre la entrada manual.',
  };
};

export const getBookingEngineerFieldState = ({
  engineerCount,
  hasAssignedEngineer,
  serviceType,
}: {
  engineerCount: number;
  hasAssignedEngineer: boolean;
  serviceType: string;
}): BookingEngineerFieldState => {
  const normalizedServiceType = serviceType.trim();

  if (!hasAssignedEngineer && normalizedServiceType === '') {
    return {
      helperText: 'Selecciona el servicio primero para decidir si hace falta un ingeniero.',
      label: 'Ingeniero',
      showField: false,
    };
  }

  if (engineerCount === 0) {
    return {
      helperText: hasAssignedEngineer
        ? 'No hay ingenieros en el catálogo de contactos. Conserva el nombre actual o actualiza contactos para volver a seleccionarlo.'
        : 'Todavía no hay ingenieros en el catálogo de contactos. Continúa sin asignar uno o agrégalo después.',
      label: 'Ingeniero',
      showField: hasAssignedEngineer,
    };
  }

  if (requiresEngineerForService(normalizedServiceType)) {
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
  roomCatalogLoading,
  roomCount,
}: {
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

  const visibleTitles = conflictTitles
    .slice(0, 3)
    .map((title) => {
      const trimmed = title?.trim();
      return trimmed && trimmed.length > 0 ? trimmed : 'reserva';
    })
    .join(', ');

  return `Conflicto con ${conflictTitles.length} reserva(s): ${visibleTitles}. Ajusta horario o salas.`;
};
