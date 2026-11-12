import { buildServiceOfferingDraft, type OfferingForm } from './ServiceTypesPage';

const form: OfferingForm = {
  entityId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  baseVersion: 4,
  code: ' studio-recording ',
  nameEs: ' Grabación de estudio ',
  nameEn: ' Studio recording ',
  descriptionEs: '',
  descriptionEn: ' Recorded in the main studio ',
  sortOrder: 20,
  categoryId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
  pricingModelId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
  price: '125.00',
  currencyId: 'dddddddd-dddd-4ddd-8ddd-dddddddddddd',
  billingUnitEs: ' hora ',
  billingUnitEn: ' hour ',
  taxRateId: '',
  defaultDurationMinutes: '120',
  requiresEngineer: true,
  resources: [{
    resourceId: '12',
    selectionModeId: 'eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee',
    sortOrder: 10,
  }],
  reason: ' Publicar configuración validada ',
};

describe('buildServiceOfferingDraft', () => {
  it('converts a displayed currency amount to exact cents while preserving canonical relationships and never copied service selectors', () => {
    const draft = buildServiceOfferingDraft(form);

    expect(draft).toMatchObject({
      entityId: form.entityId,
      baseVersion: 4,
      code: 'studio-recording',
      nameEs: 'Grabación de estudio',
      nameEn: 'Studio recording',
      descriptionEn: 'Recorded in the main studio',
      sortOrder: 20,
      serviceOffering: {
        categoryId: form.categoryId,
        pricingModelId: form.pricingModelId,
        rateCents: 12500,
        currencyId: form.currencyId,
        defaultDurationMinutes: 120,
        requiresEngineer: true,
        defaultResources: form.resources,
      },
      reason: 'Publicar configuración validada',
      sourcePlatform: 'web-admin',
    });
    expect(draft.descriptionEs).toBeUndefined();
    expect(draft.serviceOffering?.taxRateId).toBeUndefined();
    expect(draft).not.toHaveProperty('serviceType');
    expect(draft.serviceOffering).not.toHaveProperty('pricingModelCode');
  });
  it('keeps a quoted price absent, preserves zero and accepts decimal commas', () => {
    expect(buildServiceOfferingDraft({ ...form, price: '' }).serviceOffering?.rateCents).toBeUndefined();
    expect(buildServiceOfferingDraft({ ...form, price: '0' }).serviceOffering?.rateCents).toBe(0);
    expect(buildServiceOfferingDraft({ ...form, price: '25,50' }).serviceOffering?.rateCents).toBe(2550);
  });

  it('never silently removes an invalid entered price from a draft', () => {
    expect(() => buildServiceOfferingDraft({ ...form, price: '25.501' })).toThrow();
    expect(() => buildServiceOfferingDraft({ ...form, price: '-25' })).toThrow();
    expect(() => buildServiceOfferingDraft({ ...form, price: '1e3' })).toThrow();
    expect(() => buildServiceOfferingDraft({ ...form, price: '90071992547409.92' })).toThrow();
  });
});
