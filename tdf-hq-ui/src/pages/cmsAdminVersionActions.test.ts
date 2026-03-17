import { getCmsVersionRowActions } from './cmsAdminVersionActions';

describe('getCmsVersionRowActions', () => {
  it('keeps publish actions for drafts and other non-published versions', () => {
    expect(getCmsVersionRowActions('draft')).toEqual({
      showPublish: true,
      showPublishAndView: true,
      showOpenLivePage: false,
    });

    expect(getCmsVersionRowActions('archived')).toEqual({
      showPublish: true,
      showPublishAndView: true,
      showOpenLivePage: false,
    });
  });

  it('shows only the live-page action for published versions', () => {
    expect(getCmsVersionRowActions('published')).toEqual({
      showPublish: false,
      showPublishAndView: false,
      showOpenLivePage: true,
    });

    expect(getCmsVersionRowActions(' published ')).toEqual({
      showPublish: false,
      showPublishAndView: false,
      showOpenLivePage: true,
    });
  });
});
