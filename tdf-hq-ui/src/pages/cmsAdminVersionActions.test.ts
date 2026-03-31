import { getCmsVersionRowActions } from './cmsAdminVersionActions';

describe('getCmsVersionRowActions', () => {
  it('keeps publish actions for drafts and other non-published versions', () => {
    expect(getCmsVersionRowActions('draft')).toEqual({
      showPublish: true,
      showLoadInEditor: true,
      loadedStateLabel: null,
    });

    expect(getCmsVersionRowActions('archived')).toEqual({
      showPublish: true,
      showLoadInEditor: true,
      loadedStateLabel: null,
    });
  });

  it('removes publish actions for published versions because the live page is already linked globally', () => {
    expect(getCmsVersionRowActions('published')).toEqual({
      showPublish: false,
      showLoadInEditor: true,
      loadedStateLabel: null,
    });

    expect(getCmsVersionRowActions(' published ')).toEqual({
      showPublish: false,
      showLoadInEditor: true,
      loadedStateLabel: null,
    });
  });

  it('replaces the duplicate load action with passive state when the version is already in the editor', () => {
    expect(getCmsVersionRowActions('draft', { isLoadedInEditor: true })).toEqual({
      showPublish: true,
      showLoadInEditor: false,
      loadedStateLabel: 'En formulario',
    });
  });
});
