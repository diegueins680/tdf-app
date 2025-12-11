import { get, patch, post } from './client';
import type { components } from './generated/types';

export type CourseMetadata = components['schemas']['CourseMetadata'] & {
  remaining?: number | null;
  sessionStartHour?: number | null;
  sessionDurationHours?: number | null;
  instructorName?: string | null;
  instructorBio?: string | null;
  instructorAvatarUrl?: string | null;
};
export type CourseRegistrationRequest = components['schemas']['CourseRegistrationRequest'];
export type CourseRegistrationResponse = components['schemas']['CourseRegistrationResponse'];
export type CourseRegistrationStatusUpdate = components['schemas']['CourseRegistrationStatusUpdate'];

export interface CourseSessionIn {
  label: string;
  date: string;
  order?: number | null;
}

export interface CourseSyllabusIn {
  title: string;
  topics: string[];
  order?: number | null;
}

export interface CourseUpsert {
  slug: string;
  title: string;
  subtitle?: string | null;
  format?: string | null;
  duration?: string | null;
  priceCents: number;
  currency: string;
  capacity: number;
  sessionStartHour?: number | null;
  sessionDurationHours?: number | null;
  locationLabel?: string | null;
  locationMapUrl?: string | null;
  whatsappCtaUrl?: string | null;
  landingUrl?: string | null;
  daws: string[];
  includes: string[];
  instructorName?: string | null;
  instructorBio?: string | null;
  instructorAvatarUrl?: string | null;
  sessions: CourseSessionIn[];
  syllabus: CourseSyllabusIn[];
}

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
  upsert: (payload: CourseUpsert) => post<CourseMetadata>('/admin/courses', payload),
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
