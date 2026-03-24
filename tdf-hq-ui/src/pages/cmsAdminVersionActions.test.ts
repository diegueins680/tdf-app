import { getCmsVersionRowActions } from './cmsAdminVersionActions';

describe('getCmsVersionRowActions', () => {
  it('keeps publish actions for drafts and other non-published versions', () => {
    expect(getCmsVersionRowActions('draft')).toEqual({
      showPublish: true,
    });

    expect(getCmsVersionRowActions('archived')).toEqual({
      showPublish: true,
    });
  });

  it('removes publish actions for published versions because the live page is already linked globally', () => {
    expect(getCmsVersionRowActions('published')).toEqual({
      showPublish: false,
    });

    expect(getCmsVersionRowActions(' published ')).toEqual({
      showPublish: false,
    });
  });
});
