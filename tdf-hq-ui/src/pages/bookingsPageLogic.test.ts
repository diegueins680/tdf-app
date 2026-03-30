import {
  defaultMinutesForService,
  describeServiceDefaults,
  getBookingCustomerFieldState,
  requiresEngineerForService,
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
});
