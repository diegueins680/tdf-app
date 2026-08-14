import { buildRadioAutoStopDraft, type RadioAutoStopForm } from './RadioAutoStopCatalogPage';

describe('Radio auto-stop catalog draft', () => {
  it('writes the typed duration/default payload and no copied minute selector', () => {
    const form: RadioAutoStopForm = {
      entityId: '11111111-1111-4111-8111-111111111111',
      baseVersion: 2,
      code: ' minutes-120 ',
      nameEs: ' 120 minutos ',
      nameEn: ' 120 minutes ',
      descriptionEs: '',
      descriptionEn: '',
      durationMinutes: '120',
      defaultForBroadcast: true,
      sortOrder: 40,
      reason: ' Política revisada ',
    };

    const draft = buildRadioAutoStopDraft(form);

    expect(draft).toMatchObject({
      entityId: form.entityId,
      baseVersion: 2,
      code: 'minutes-120',
      radioAutoStop: { durationMinutes: 120, defaultForBroadcast: true },
      reason: 'Política revisada',
      sourcePlatform: 'web-admin',
    });
    expect(draft).not.toHaveProperty('durationMinutes');
    expect(draft).not.toHaveProperty('minutes');
  });
});
