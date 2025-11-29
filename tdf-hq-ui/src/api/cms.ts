import { get, post, del } from './client';

export interface CmsContentDTO {
  ccdId: number;
  ccdSlug: string;
  ccdLocale: string;
  ccdVersion: number;
  ccdStatus: string;
  ccdTitle?: string | null;
  ccdPayload?: unknown;
  ccdCreatedAt: string;
  ccdPublishedAt?: string | null;
}

export interface CmsContentIn {
  cciSlug: string;
  cciLocale: string;
  cciTitle?: string | null;
  cciStatus?: string | null;
  cciPayload?: unknown;
}

export const Cms = {
  getPublic: (slug: string, locale = 'es') =>
    get<CmsContentDTO>(`/cms/content?slug=${encodeURIComponent(slug)}&locale=${encodeURIComponent(locale)}`),
  list: (params?: { slug?: string; locale?: string }) => {
    const search = new URLSearchParams();
    if (params?.slug) search.set('slug', params.slug);
    if (params?.locale) search.set('locale', params.locale);
    const qs = search.toString();
    return get<CmsContentDTO[]>(`/cms/content${qs ? `?${qs}` : ''}`);
  },
  create: (payload: CmsContentIn) => post<CmsContentDTO>('/cms/content', payload),
  publish: (id: number) => post<CmsContentDTO>(`/cms/content/${id}/publish`, {}),
  remove: (id: number) => del<void>(`/cms/content/${id}`),
};
