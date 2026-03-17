import { del, get, patch, post } from './client';
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
  crPartyId?: number | null;
  crFullName?: string | null;
  crEmail?: string | null;
  crPhoneE164?: string | null;
  crSource: string | null;
  crStatus: string;
  crAdminNotes?: string | null;
  crHowHeard?: string | null;
  crUtmSource?: string | null;
  crUtmMedium?: string | null;
  crUtmCampaign?: string | null;
  crUtmContent?: string | null;
  crCreatedAt: string;
  crUpdatedAt: string;
}

export interface CourseRegistrationReceiptDTO {
  crrId: number;
  crrRegistrationId: number;
  crrPartyId?: number | null;
  crrFileUrl: string;
  crrFileName?: string | null;
  crrMimeType?: string | null;
  crrNotes?: string | null;
  crrUploadedBy?: number | null;
  crrCreatedAt: string;
  crrUpdatedAt: string;
}

export interface CourseRegistrationFollowUpDTO {
  crfId: number;
  crfRegistrationId: number;
  crfPartyId?: number | null;
  crfEntryType: string;
  crfSubject?: string | null;
  crfNotes: string;
  crfAttachmentUrl?: string | null;
  crfAttachmentName?: string | null;
  crfNextFollowUpAt?: string | null;
  crfCreatedBy?: number | null;
  crfCreatedAt: string;
  crfUpdatedAt: string;
}

export interface CourseRegistrationDossierDTO {
  crdRegistration: CourseRegistrationDTO;
  crdReceipts: CourseRegistrationReceiptDTO[];
  crdFollowUps: CourseRegistrationFollowUpDTO[];
  crdCanMarkPaid: boolean;
}

export interface CourseRegistrationNotesUpdate {
  notes?: string | null;
}

export interface CourseRegistrationReceiptCreate {
  fileUrl: string;
  fileName?: string | null;
  mimeType?: string | null;
  notes?: string | null;
}

export interface CourseRegistrationReceiptUpdate {
  fileUrl?: string | null;
  fileName?: string | null;
  mimeType?: string | null;
  notes?: string | null;
}

export interface CourseRegistrationFollowUpCreate {
  entryType?: string | null;
  subject?: string | null;
  notes: string;
  attachmentUrl?: string | null;
  attachmentName?: string | null;
  nextFollowUpAt?: string | null;
}

export interface CourseRegistrationFollowUpUpdate {
  entryType?: string | null;
  subject?: string | null;
  notes?: string | null;
  attachmentUrl?: string | null;
  attachmentName?: string | null;
  nextFollowUpAt?: string | null;
}

export interface CourseEmailEventDTO {
  ceId: number;
  ceCourseSlug: string;
  ceRegistrationId?: number | null;
  ceRecipientEmail: string;
  ceRecipientName?: string | null;
  ceEventType: string;
  ceStatus: string;
  ceMessage?: string | null;
  ceCreatedAt: string;
}

export interface CourseCohortOptionDTO {
  ccSlug: string;
  ccTitle?: string | null;
}

const courseBase = (slug: string) => `/public/courses/${slug}`;

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

const normalizeCourseSlug = (slug: string): string => {
  const trimmed = slug.trim();
  if (trimmed === '') {
    throw new Error('slug no puede estar vacío.');
  }
  return encodeURIComponent(trimmed);
};

const setTrimmedParam = (search: URLSearchParams, key: string, value?: string): void => {
  const trimmed = value?.trim();
  if (trimmed) search.set(key, trimmed);
};

export const Courses = {
  upsert: (payload: CourseUpsert) => post<CourseMetadata>('/admin/courses', payload),
  listCohorts: () => get<CourseCohortOptionDTO[]>('/admin/courses/cohorts'),
  getMetadata: (slug: string) => get<CourseMetadata>(courseBase(normalizeCourseSlug(slug))),
  register: (slug: string, payload: CourseRegistrationRequest) =>
    post<CourseRegistrationResponse>(`${courseBase(normalizeCourseSlug(slug))}/registrations`, payload),
  updateStatus: (slug: string, registrationId: number, payload: CourseRegistrationStatusUpdate) =>
    patch<CourseRegistrationResponse>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/status`,
      payload,
    ),
  getRegistrationDossier: (slug: string, registrationId: number) =>
    get<CourseRegistrationDossierDTO>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/dossier`,
    ),
  updateRegistrationNotes: (slug: string, registrationId: number, payload: CourseRegistrationNotesUpdate) =>
    patch<CourseRegistrationDTO>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/notes`,
      payload,
    ),
  createReceipt: (slug: string, registrationId: number, payload: CourseRegistrationReceiptCreate) =>
    post<CourseRegistrationReceiptDTO>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/receipts`,
      payload,
    ),
  updateReceipt: (
    slug: string,
    registrationId: number,
    receiptId: number,
    payload: CourseRegistrationReceiptUpdate,
  ) =>
    patch<CourseRegistrationReceiptDTO>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/receipts/${requirePositiveInteger(receiptId, 'receiptId')}`,
      payload,
    ),
  deleteReceipt: (slug: string, registrationId: number, receiptId: number) =>
    del<void>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/receipts/${requirePositiveInteger(receiptId, 'receiptId')}`,
    ),
  createFollowUp: (slug: string, registrationId: number, payload: CourseRegistrationFollowUpCreate) =>
    post<CourseRegistrationFollowUpDTO>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/follow-ups`,
      payload,
    ),
  updateFollowUp: (
    slug: string,
    registrationId: number,
    followUpId: number,
    payload: CourseRegistrationFollowUpUpdate,
  ) =>
    patch<CourseRegistrationFollowUpDTO>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/follow-ups/${requirePositiveInteger(followUpId, 'followUpId')}`,
      payload,
    ),
  deleteFollowUp: (slug: string, registrationId: number, followUpId: number) =>
    del<void>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/follow-ups/${requirePositiveInteger(followUpId, 'followUpId')}`,
    ),
  listRegistrations: (params?: { slug?: string; status?: string; limit?: number }) => {
    const search = new URLSearchParams();
    setTrimmedParam(search, 'slug', params?.slug);
    setTrimmedParam(search, 'status', params?.status);
    if (params?.limit != null) search.set('limit', String(requirePositiveInteger(params.limit, 'limit')));
    const qs = search.toString();
    return get<CourseRegistrationDTO[]>(`/admin/courses/registrations${qs ? `?${qs}` : ''}`);
  },
  listRegistrationEmails: (registrationId: number, limit = 100) =>
    get<CourseEmailEventDTO[]>(
      `/admin/courses/registrations/${requirePositiveInteger(registrationId, 'registrationId')}/emails?limit=${requirePositiveInteger(limit, 'limit')}`,
    ),
  getRegistration: (slug: string, registrationId: number) =>
    get<CourseRegistrationDTO>(
      `/admin/courses/${normalizeCourseSlug(slug)}/registrations/${requirePositiveInteger(registrationId, 'registrationId')}`,
    ),
};
