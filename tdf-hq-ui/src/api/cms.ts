import { get, post, del } from './client';
const CMS_ADMIN_BASE = '/cms/admin/content';

type WireCmsContentDTO = Partial<CmsContentDTO> & {
  id?: number;
  slug?: string;
  locale?: string;
  version?: number;
  status?: string;
  title?: string | null;
  payload?: unknown;
  createdAt?: string;
  publishedAt?: string | null;
};

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

const normalizeCmsContent = (content: WireCmsContentDTO): CmsContentDTO => ({
  ccdId: content.ccdId ?? content.id ?? 0,
  ccdSlug: content.ccdSlug ?? content.slug ?? '',
  ccdLocale: content.ccdLocale ?? content.locale ?? 'es',
  ccdVersion: content.ccdVersion ?? content.version ?? 0,
  ccdStatus: content.ccdStatus ?? content.status ?? 'missing',
  ccdTitle: content.ccdTitle ?? content.title ?? null,
  ccdPayload: content.ccdPayload !== undefined ? content.ccdPayload : content.payload,
  ccdCreatedAt: content.ccdCreatedAt ?? content.createdAt ?? '',
  ccdPublishedAt: content.ccdPublishedAt ?? content.publishedAt ?? null,
});

const toWireCmsContentIn = (payload: CmsContentIn) => ({
  slug: payload.cciSlug,
  locale: payload.cciLocale,
  title: payload.cciTitle,
  status: payload.cciStatus,
  payload: payload.cciPayload,
});

export const Cms = {
  getPublic: (slug: string, locale = 'es') =>
    get<WireCmsContentDTO>(`/cms/content?slug=${encodeURIComponent(slug)}&locale=${encodeURIComponent(locale)}`)
      .then(normalizeCmsContent),
  getPublicList: (params?: { locale?: string; slugPrefix?: string }) => {
    const search = new URLSearchParams();
    if (params?.locale) search.set('locale', params.locale);
    if (params?.slugPrefix) search.set('slugPrefix', params.slugPrefix);
    const qs = search.toString();
    return get<WireCmsContentDTO[]>(`/cms/contents${qs ? `?${qs}` : ''}`)
      .then((items) => items.map(normalizeCmsContent));
  },
  list: (params?: { slug?: string; locale?: string }) => {
    const search = new URLSearchParams();
    if (params?.slug) search.set('slug', params.slug);
    if (params?.locale) search.set('locale', params.locale);
    const qs = search.toString();
    return get<WireCmsContentDTO[]>(`${CMS_ADMIN_BASE}${qs ? `?${qs}` : ''}`)
      .then((items) => items.map(normalizeCmsContent));
  },
  create: (payload: CmsContentIn) =>
    post<WireCmsContentDTO>(CMS_ADMIN_BASE, toWireCmsContentIn(payload)).then(normalizeCmsContent),
  publish: (id: number) => post<WireCmsContentDTO>(`${CMS_ADMIN_BASE}/${id}/publish`, {}).then(normalizeCmsContent),
  remove: (id: number) => del<void>(`${CMS_ADMIN_BASE}/${id}`),
};
