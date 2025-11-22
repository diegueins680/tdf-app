import { get, patch, post } from './client';
import type { components } from './generated/types';

export type CourseMetadata = components['schemas']['CourseMetadata'];
export type CourseRegistrationRequest = components['schemas']['CourseRegistrationRequest'];
export type CourseRegistrationResponse = components['schemas']['CourseRegistrationResponse'];
export type CourseRegistrationStatusUpdate = components['schemas']['CourseRegistrationStatusUpdate'];

const courseBase = (slug: string) => `/public/courses/${slug}`;

export const Courses = {
  getMetadata: (slug: string) => get<CourseMetadata>(courseBase(slug)),
  register: (slug: string, payload: CourseRegistrationRequest) =>
    post<CourseRegistrationResponse>(`${courseBase(slug)}/registrations`, payload),
  updateStatus: (slug: string, registrationId: number, payload: CourseRegistrationStatusUpdate) =>
    patch<CourseRegistrationResponse>(`/admin/courses/${slug}/registrations/${registrationId}/status`, payload),
};
