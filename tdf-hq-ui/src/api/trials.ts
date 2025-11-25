import { get, post } from './client';

export interface TrialSubject {
  subjectId: number;
  name: string;
  active: boolean;
  roomIds: string[];
}

export interface PreferredSlot {
  startAt: string;
  endAt: string;
}

export interface TrialSlot {
  subjectId: number;
  teacherId: number;
  teacherName: string;
  slots: PreferredSlot[];
}

export interface TrialRequestPayload {
  partyId?: number;
  subjectId: number;
  preferred: PreferredSlot[];
  notes?: string;
  fullName?: string;
  email?: string;
  phone?: string;
}

export interface TrialRequestResponse {
  requestId: number;
  status: string;
}

const base = '/trials/v1';

export const Trials = {
  listSubjects: () => get<TrialSubject[]>(`${base}/subjects`),
  listSlots: (subjectId?: number) => {
    const qs = subjectId ? `?subjectId=${subjectId}` : '';
    return get<TrialSlot[]>(`${base}/trial-slots${qs}`);
  },
  createRequest: (payload: TrialRequestPayload) =>
    post<TrialRequestResponse>(`${base}/trial-requests`, payload),
};
