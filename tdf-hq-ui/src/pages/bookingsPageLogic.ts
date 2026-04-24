const normalizeServiceType = (serviceType: string) => serviceType.trim().toLowerCase();

interface BookingCustomerFieldState {
  helperText: string;
  dialogTitle: string;
  quickCreateLabel: string;
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

interface BookingEngineerFieldState {
  helperText: string;
  label: string;
  showField: boolean;
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
      showCustomerSelector: true,
      showQuickCreateAction: false,
    };
  }

  if (customerCatalogLoading) {
    return {
      helperText: 'Cargando clientes guardados… Espera un momento antes de crear un contacto nuevo para evitar duplicados.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showCustomerSelector: false,
      showQuickCreateAction: false,
    };
  }

  if (customerCount <= 0) {
    return {
      helperText: 'Todavía no hay clientes guardados. Agrega el primero sin salir de esta sesión.',
      dialogTitle: 'Agregar primer contacto',
      quickCreateLabel: 'Agregar primer contacto',
      showCustomerSelector: false,
      showQuickCreateAction: true,
    };
  }

  return {
    helperText: 'Selecciona un cliente guardado. Si todavía no existe, créalo aquí.',
    dialogTitle: 'Nuevo contacto',
    quickCreateLabel: 'Crear contacto nuevo',
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
