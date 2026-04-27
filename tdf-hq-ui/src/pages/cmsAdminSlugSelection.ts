export const DEFAULT_CMS_SLUGS = [
  'records-public',
  'records-releases',
  'records-sessions',
  'fan-hub',
  'course-production',
] as const;

export const CUSTOM_CMS_SLUG_OPTION = '__custom__';

const defaultCmsSlugSet = new Set<string>(DEFAULT_CMS_SLUGS);

export interface CmsSlugFieldState {
  selectValue: string;
  showCustomInput: boolean;
}

export const getCmsSlugFieldState = (slug: string): CmsSlugFieldState => {
  const isDefaultSlug = defaultCmsSlugSet.has(slug);

  return {
    selectValue: isDefaultSlug ? slug : CUSTOM_CMS_SLUG_OPTION,
    showCustomInput: !isDefaultSlug,
  };
};
