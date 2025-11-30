import { get, patch, post } from './client';
import type { components } from './generated/types';

export type CourseMetadata = components['schemas']['CourseMetadata'];
export type CourseRegistrationRequest = components['schemas']['CourseRegistrationRequest'];
export type CourseRegistrationResponse = components['schemas']['CourseRegistrationResponse'];
export type CourseRegistrationStatusUpdate = components['schemas']['CourseRegistrationStatusUpdate'];

export interface CourseRegistrationDTO {
  crId: number;
  crCourseSlug: string;
  crFullName?: string | null;
  crEmail?: string | null;
  crPhoneE164?: string | null;
  crSource: string;
  crStatus: string;
  crHowHeard?: string | null;
  crUtmSource?: string | null;
  crUtmMedium?: string | null;
  crUtmCampaign?: string | null;
  crUtmContent?: string | null;
  crCreatedAt: string;
  crUpdatedAt: string;
}

const courseBase = (slug: string) => `/public/courses/${slug}`;

export const Courses = {
  getMetadata: (slug: string) => get<CourseMetadata>(courseBase(slug)),
  register: (slug: string, payload: CourseRegistrationRequest) =>
    post<CourseRegistrationResponse>(`${courseBase(slug)}/registrations`, payload),
  updateStatus: (slug: string, registrationId: number, payload: CourseRegistrationStatusUpdate) =>
    patch<CourseRegistrationResponse>(`/admin/courses/${slug}/registrations/${registrationId}/status`, payload),
  listRegistrations: (params: { slug?: string; status?: string; limit?: number }) => {
    const search = new URLSearchParams();
    if (params.slug) search.set('slug', params.slug);
    if (params.status) search.set('status', params.status);
    if (params.limit) search.set('limit', String(params.limit));
    const qs = search.toString();
    return get<CourseRegistrationDTO[]>(`/admin/courses/registrations${qs ? `?${qs}` : ''}`);
  },
  getRegistration: (slug: string, registrationId: number) =>
    get<CourseRegistrationDTO>(`/admin/courses/${slug}/registrations/${registrationId}`),
};
