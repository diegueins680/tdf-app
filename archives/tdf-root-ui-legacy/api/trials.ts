import { get, post } from './client';
import type {
  AttendancePayload,
  ClassSessionCreate,
  ClassSessionDTO,
  CommissionDTO,
  PackageDTO,
  PurchaseOutDTO,
  SubjectDTO,
  TrialAssignPayload,
  TrialPurchasePayload,
  TrialRequestCreate,
  TrialRequestDTO,
  TrialSchedulePayload,
  TrialTeacherAvailabilityDTO,
  TrialSlotDTO,
} from './types';

export const Trials = {
  createPublicSignup: (body: {
    firstName: string;
    lastName: string;
    email: string;
    phone?: string;
    password?: string;
    googleIdToken?: string;
    marketingOptIn?: boolean;
  }) => post('/v1/signup', body),

  createPublicInterest: (body: {
    interestType: string;
    subjectId?: number;
    details?: string;
    driveLink?: string;
  }) => post('/v1/interests', body),

  createTrialRequest: (body: TrialRequestCreate) => post<TrialRequestDTO>('/v1/trial-requests', body),

  listTrialRequests: (filters?: { subjectId?: number; status?: string }) => {
    const params = new URLSearchParams();
    if (filters?.subjectId !== undefined) params.set('subjectId', String(filters.subjectId));
    if (filters?.status) params.set('status', filters.status);
    const suffix = params.toString() ? `?${params.toString()}` : '';
    return get<TrialRequestDTO[]>(`/v1/trial-requests${suffix}`);
  },

  assignTrial: (requestId: number, body: TrialAssignPayload) =>
    post<TrialRequestDTO>(`/v1/trial-requests/${requestId}/assign`, body),

  scheduleTrial: (body: TrialSchedulePayload) => post<TrialRequestDTO>('/v1/trial-assignments', body),

  listSubjects: () => get<SubjectDTO[]>('/v1/subjects'),

  listTrialAvailability: (subjectId: number) =>
    get<TrialTeacherAvailabilityDTO[]>(`/v1/trial-availability?subjectId=${subjectId}`),
  listTrialSlots: (subjectId: number) => {
    const params = new URLSearchParams();
    params.set('subjectId', String(subjectId));
    const suffix = params.toString() ? `?${params.toString()}` : '';
    return get<TrialSlotDTO[]>(`/v1/trial-slots${suffix}`);
  },

  listPackages: (subjectId?: number) => {
    const params = new URLSearchParams();
    if (subjectId !== undefined) params.set('subjectId', String(subjectId));
    const suffix = params.toString() ? `?${params.toString()}` : '';
    return get<PackageDTO[]>(`/v1/packages${suffix}`);
  },

  createPurchase: (body: TrialPurchasePayload) => post<PurchaseOutDTO>('/v1/purchases', body),

  createClassSession: (body: ClassSessionCreate) => post<ClassSessionDTO>('/v1/class-sessions', body),

  markAttendance: (sessionId: number, body: AttendancePayload) =>
    post<ClassSessionDTO>(`/v1/class-sessions/${sessionId}/attend`, body),

  listCommissions: (params?: { from?: string; to?: string; teacherId?: number }) => {
    const search = new URLSearchParams();
    if (params?.from) search.set('from', params.from);
    if (params?.to) search.set('to', params.to);
    if (params?.teacherId !== undefined) search.set('teacherId', String(params.teacherId));
    const suffix = search.toString() ? `?${search.toString()}` : '';
    return get<CommissionDTO[]>(`/v1/commissions${suffix}`);
  },
};

