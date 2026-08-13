import { buildAppearanceModeDraft, type AppearanceModeForm } from './AppearanceModeCatalogPage';

describe('Appearance-mode catalog draft', () => {
  it('writes the typed persisted-default decision and canonical UUID without a copied renderer field', () => {
    const form: AppearanceModeForm = {
      entityId: '11111111-1111-4111-8111-111111111111',
      baseVersion: 3,
      code: 'system',
      nameEs: ' Usar configuración del sistema ',
      nameEn: ' Use system setting ',
      descriptionEs: '',
      descriptionEn: '',
      sortOrder: 0,
      defaultForApplication: true,
      reason: ' Política revisada ',
    };

    const draft = buildAppearanceModeDraft(form);

    expect(draft).toMatchObject({
      entityId: form.entityId,
      baseVersion: 3,
      code: 'system',
      appearanceMode: { defaultForApplication: true },
      reason: 'Política revisada',
      sourcePlatform: 'web-admin',
    });
    expect(draft).not.toHaveProperty('theme');
    expect(draft).not.toHaveProperty('themeMode');
  });
});
