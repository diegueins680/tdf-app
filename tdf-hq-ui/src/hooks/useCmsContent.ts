import { useQuery } from '@tanstack/react-query';
import { Cms, type CmsContentDTO } from '../api/cms';

export function useCmsContent(slug: string, locale = 'es') {
  return useQuery<CmsContentDTO>({
    queryKey: ['cms-public', slug, locale],
    queryFn: () => Cms.getPublic(slug, locale),
    retry: 1,
  });
}
