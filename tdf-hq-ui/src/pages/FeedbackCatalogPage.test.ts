import { buildFeedbackCatalogDraft, type FeedbackCatalogForm } from './FeedbackCatalogPage';

describe('feedback catalog administration', () => {
  it('builds a canonical bilingual draft with an explicit scoped-default decision', () => {
    const form: FeedbackCatalogForm = {
      entityId: '31000000-0000-4000-8000-000000000002',
      baseVersion: 3,
      code: ' idea ',
      nameEs: ' Idea ',
      nameEn: ' Idea ',
      descriptionEs: '',
      descriptionEn: ' Product idea ',
      sortOrder: 10,
      globalDefault: true,
      reason: ' Actualizar catálogo ',
    };

    expect(buildFeedbackCatalogDraft(form)).toMatchObject({
      entityId: '31000000-0000-4000-8000-000000000002',
      baseVersion: 3,
      code: 'idea',
      nameEs: 'Idea',
      nameEn: 'Idea',
      descriptionEn: 'Product idea',
      sortOrder: 10,
      globalDefault: true,
      reason: 'Actualizar catálogo',
      sourcePlatform: 'web-admin',
    });
  });
});
