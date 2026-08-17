import { classifiedFormError, moneyToMinor, taxonomyRequirements } from './DirectoryManagePage';

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
