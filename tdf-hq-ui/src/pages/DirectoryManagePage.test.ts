import { classifiedFormError, moneyToMinor, profileFormError, taxonomyRequirements } from './DirectoryManagePage';

const completeInput = {
  required: new Set<string>(),
  cityIds: ['quito'],
  remote: false,
  availableToTravel: false,
  professionIds: ['producer'],
  instrumentIds: ['bass'],
  genreIds: ['rock'],
  startsAt: '2026-09-20T20:00',
  endsAt: '2026-09-20T22:00',
  compensationTypeId: 'range',
  budgetMode: 'range',
  budgetMin: '100.50',
  budgetMax: '250',
  serviceOfferingId: 'recording',
};

describe('directory classified form policy', () => {
  it('reads required fields from the server-managed category metadata', () => {
    const requirements = taxonomyRequirements({
      id: 'category',
      code: 'seeking-musician',
      name: 'Busco músico',
      requirements: { required: ['instrumentIds', 'locations'] },
    });
    expect(requirements).toEqual(new Set(['instrumentIds', 'locations']));
  });

  it('blocks a category until all of its contextual requirements are present', () => {
    expect(classifiedFormError({
      ...completeInput,
      required: new Set(['instrumentIds', 'locations']),
      instrumentIds: [],
    })).toBe('Selecciona al menos un instrumento.');

    expect(classifiedFormError({
      ...completeInput,
      required: new Set(['dateRange', 'compensationTypeId', 'budget', 'serviceOfferingId']),
    })).toBeNull();
  });

  it('rejects inverted periods and monetary ranges', () => {
    expect(classifiedFormError({
      ...completeInput,
      endsAt: '2026-09-20T19:00',
    })).toBe('La fecha de fin no puede preceder al inicio.');
    expect(classifiedFormError({
      ...completeInput,
      budgetMax: '50',
    })).toBe('El presupuesto máximo no puede ser menor que el mínimo.');
  });

  it('converts display amounts with the selected currency precision', () => {
    expect(moneyToMinor('100.50', 2)).toBe(10050);
    expect(moneyToMinor('123', 0)).toBe(123);
    expect(moneyToMinor('-1', 2)).toBeUndefined();
  });
});

describe('directory professional profile form policy', () => {
  const validProfile = {
    name: 'Bajista Quito',
    cityIds: ['quito', 'cuenca'],
    primaryCityId: 'quito',
    onsite: true,
    remote: true,
    availableToTravel: false,
    rateMin: '100',
    rateMax: '250',
    portfolio: [{ itemType: 'audio' as const, title: 'Demo', url: 'https://example.test/demo' }],
    links: [{ label: 'Sitio', url: 'https://example.test' }],
  };

  it('requires an explicit primary city inside the service areas', () => {
    expect(profileFormError({ ...validProfile, primaryCityId: 'guayaquil' }))
      .toBe('Selecciona al menos una ciudad y marca la principal.');
  });

  it('rejects invalid ranges and URLs with embedded credentials', () => {
    expect(profileFormError({ ...validProfile, rateMin: '-1' }))
      .toBe('Las tarifas deben ser números no negativos.');
    expect(profileFormError({ ...validProfile, rateMax: '50' }))
      .toBe('La tarifa máxima no puede ser menor que la mínima.');
    expect(profileFormError({ ...validProfile, links: [{ label: 'Privado', url: 'https://user:secret@example.test' }] }))
      .toBe('Cada enlace necesita etiqueta y una URL HTTP(S) o ruta interna válida sin credenciales.');
  });

  it('accepts multiple service cities and closed typed media', () => {
    expect(profileFormError(validProfile)).toBeNull();
  });

  it('allows an existing non-city primary area to survive an editor round trip', () => {
    expect(profileFormError({ ...validProfile, cityIds: [], primaryCityId: '', hasPreservedPrimaryArea: true })).toBeNull();
  });

  it('accepts same-origin legacy media paths but rejects protocol-relative URLs', () => {
    expect(profileFormError({ ...validProfile, links: [{ label: 'Media TDF', url: '/media/profile/demo.mp3' }] })).toBeNull();
    expect(profileFormError({ ...validProfile, links: [{ label: 'Externo', url: '//evil.example/demo' }] }))
      .toBe('Cada enlace necesita etiqueta y una URL HTTP(S) o ruta interna válida sin credenciales.');
  });
});
