import { get, post, del } from './client';
const CMS_ADMIN_BASE = '/cms/admin/content';

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
  getPublicList: (params?: { locale?: string; slugPrefix?: string }) => {
    const search = new URLSearchParams();
    if (params?.locale) search.set('locale', params.locale);
    if (params?.slugPrefix) search.set('slugPrefix', params.slugPrefix);
    const qs = search.toString();
    return get<CmsContentDTO[]>(`/cms/contents${qs ? `?${qs}` : ''}`);
  },
  list: (params?: { slug?: string; locale?: string }) => {
    const search = new URLSearchParams();
    if (params?.slug) search.set('slug', params.slug);
    if (params?.locale) search.set('locale', params.locale);
    const qs = search.toString();
    return get<CmsContentDTO[]>(`${CMS_ADMIN_BASE}${qs ? `?${qs}` : ''}`);
  },
  create: (payload: CmsContentIn) => post<CmsContentDTO>(CMS_ADMIN_BASE, payload),
  publish: (id: number) => post<CmsContentDTO>(`${CMS_ADMIN_BASE}/${id}/publish`, {}),
  remove: (id: number) => del<void>(`${CMS_ADMIN_BASE}/${id}`),
};
