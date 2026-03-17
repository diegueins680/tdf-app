import {
  CUSTOM_CMS_SLUG_OPTION,
  DEFAULT_CMS_SLUGS,
  getCmsSlugFieldState,
} from './cmsAdminSlugSelection';

describe('getCmsSlugFieldState', () => {
  it('keeps built-in slugs on the primary select without showing a duplicate text field', () => {
    for (const slug of DEFAULT_CMS_SLUGS) {
      expect(getCmsSlugFieldState(slug)).toEqual({
        selectValue: slug,
        showCustomInput: false,
      });
    }
  });

  it('routes blank and custom slugs to the explicit custom input mode', () => {
    expect(getCmsSlugFieldState('')).toEqual({
      selectValue: CUSTOM_CMS_SLUG_OPTION,
      showCustomInput: true,
    });

    expect(getCmsSlugFieldState('promo-landing')).toEqual({
      selectValue: CUSTOM_CMS_SLUG_OPTION,
      showCustomInput: true,
    });
  });
});
