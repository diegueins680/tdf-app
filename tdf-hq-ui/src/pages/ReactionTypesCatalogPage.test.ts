import { buildReactionTypeDraft, type ReactionTypeForm } from './ReactionTypesCatalogPage';

describe('reaction type catalog administration', () => {
  it('builds a strict bilingual draft with the persisted display symbol', () => {
    const form: ReactionTypeForm = {
      entityId: '50800000-0000-4000-8000-000000000003',
      baseVersion: 4,
      code: ' applause ',
      displaySymbol: ' 👏 ',
      nameEs: ' Aplauso ',
      nameEn: ' Applause ',
      descriptionEs: '',
      descriptionEn: ' Audience applause ',
      sortOrder: 2,
      reason: ' Actualizar traducción ',
    };

    expect(buildReactionTypeDraft(form)).toMatchObject({
      entityId: '50800000-0000-4000-8000-000000000003',
      baseVersion: 4,
      code: 'applause',
      displaySymbol: '👏',
      nameEs: 'Aplauso',
      nameEn: 'Applause',
      descriptionEn: 'Audience applause',
      sortOrder: 2,
      reason: 'Actualizar traducción',
      sourcePlatform: 'web-admin',
    });
  });

  it('omits mutable identity fields for a new item', () => {
    const draft = buildReactionTypeDraft({
      code: 'surprised', displaySymbol: '😮', nameEs: 'Sorpresa', nameEn: 'Surprised',
      descriptionEs: '', descriptionEn: '', sortOrder: 3, reason: 'Nueva reacción',
    });
    expect(draft.entityId).toBeUndefined();
    expect(draft.baseVersion).toBeUndefined();
  });

  it('omits reaction-only symbol metadata for a creator badge type', () => {
    const draft = buildReactionTypeDraft({
      code: 'og', displaySymbol: '', nameEs: 'Miembro fundador', nameEn: 'Founding member',
      descriptionEs: '', descriptionEn: '', sortOrder: 30, reason: 'Persistir insignia',
    });
    expect(draft).not.toHaveProperty('displaySymbol');
    expect(draft).toMatchObject({ code: 'og', nameEs: 'Miembro fundador', nameEn: 'Founding member' });
  });
});
