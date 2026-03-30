const normalizeServiceType = (serviceType: string) => serviceType.trim().toLowerCase();

type BookingCustomerFieldState = {
  helperText: string;
  dialogTitle: string;
  quickCreateLabel: string;
  showQuickCreateAction: boolean;
};

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
  selectedCustomerId,
}: {
  customerCount: number;
  selectedCustomerId: number | null;
}): BookingCustomerFieldState => {
  if (selectedCustomerId != null) {
    return {
      helperText: 'Cliente asignado. Cambia la selección solo si necesitas reemplazarlo.',
      dialogTitle: 'Nuevo contacto',
      quickCreateLabel: 'Crear contacto nuevo',
      showQuickCreateAction: false,
    };
  }

  if (customerCount <= 0) {
    return {
      helperText: 'Todavía no hay clientes guardados. Agrega el primero sin salir de esta sesión.',
      dialogTitle: 'Agregar primer contacto',
      quickCreateLabel: 'Agregar primer contacto',
      showQuickCreateAction: true,
    };
  }

  return {
    helperText: 'Selecciona un cliente guardado. Si todavía no existe, créalo aquí.',
    dialogTitle: 'Nuevo contacto',
    quickCreateLabel: 'Crear contacto nuevo',
    showQuickCreateAction: true,
  };
};
