import {
  defaultMinutesForService,
  describeServiceDefaults,
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
});
