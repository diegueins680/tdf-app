import { useQuery } from '@tanstack/react-query';
import { Cms, type CmsContentDTO } from '../api/cms';

export function useCmsContent(slug: string, locale = 'es') {
  return useQuery<CmsContentDTO>({
    queryKey: ['cms-public', slug, locale],
    queryFn: () => Cms.getPublic(slug, locale),
    retry: 1,
  });
}

export function useCmsContents(slugPrefix: string, locale = 'es') {
  return useQuery<CmsContentDTO[]>({
    queryKey: ['cms-public-list', slugPrefix, locale],
    queryFn: () => Cms.getPublicList({ slugPrefix, locale }),
    retry: 1,
  });
}
