import { get, post, patch, put, del } from './client';

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

export interface ClassSessionOut {
  classSessionId: number;
  consumedMinutes: number;
}

export interface TeacherSubject {
  subjectId: number;
  name: string;
}

export interface TeacherDTO {
  teacherId: number;
  teacherName: string;
  subjects: TeacherSubject[];
}

export interface TeacherSubjectsUpdate {
  subjectIds: number[];
}

export interface ClassSessionDTO {
  classSessionId: number;
  teacherId: number;
  teacherName?: string | null;
  subjectId: number;
  subjectName?: string | null;
  studentId: number;
  studentName?: string | null;
  startAt: string;
  endAt: string;
  status: string;
  roomId?: string | null;
  roomName?: string | null;
  bookingId?: number | null;
  notes?: string | null;
  updatedAt?: string | null;
}

export interface ClassSessionCreate {
  studentId: number;
  teacherId: number;
  subjectId: number;
  startAt: string;
  endAt: string;
  roomId: number;
  bookingId?: number | null;
  status?: string;
}

export interface ClassSessionUpdate {
  teacherId?: number;
  subjectId?: number;
  studentId?: number;
  startAt?: string;
  endAt?: string;
  roomId?: number;
  bookingId?: number | null;
  notes?: string | null;
  status?: string;
}

export interface ClassSessionAttend {
  attended: boolean;
  notes?: string | null;
}

export interface StudentCreate {
  fullName: string;
  email: string;
  phone?: string;
  notes?: string;
}

export interface StudentDTO {
  studentId: number;
  displayName: string;
  email?: string | null;
  phone?: string | null;
}

export interface StudentUpdate {
  displayName?: string;
  email?: string | null;
  phone?: string | null;
  notes?: string | null;
}

export interface TeacherStudentLinkIn {
  studentId: number;
}

export interface TrialAvailabilitySlotDTO {
  availabilityId: number;
  subjectId: number;
  subjectName?: string | null;
  teacherId: number;
  teacherName?: string | null;
  roomId: string;
  roomName?: string | null;
  startAt: string;
  endAt: string;
  notes?: string | null;
}

export interface TrialAvailabilityUpsert {
  availabilityId?: number | null;
  subjectId: number;
  roomId: string;
  startAt: string;
  endAt: string;
  notes?: string | null;
  teacherId?: number | null;
}

const base = '/trials/v1';

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

const setPositiveIntParam = (search: URLSearchParams, key: string, value?: number): void => {
  if (value == null) return;
  search.set(key, String(requirePositiveInteger(value, key)));
};

const setTrimmedParam = (search: URLSearchParams, key: string, value?: string): void => {
  const trimmed = value?.trim();
  if (trimmed) search.set(key, trimmed);
};

export const Trials = {
  listSubjects: () => get<TrialSubject[]>(`${base}/subjects`),
  listSlots: (subjectId?: number) => {
    const qs = subjectId == null ? '' : `?subjectId=${requirePositiveInteger(subjectId, 'subjectId')}`;
    return get<TrialSlot[]>(`${base}/trial-slots${qs}`);
  },
  createRequest: (payload: TrialRequestPayload) =>
    post<TrialRequestResponse>(`${base}/trial-requests`, payload),
  listTeachers: () => get<TeacherDTO[]>(`${base}/teachers`),
  listTeacherClasses: (teacherId: number, params?: { subjectId?: number; from?: string; to?: string }) => {
    const normalizedTeacherId = requirePositiveInteger(teacherId, 'teacherId');
    const search = new URLSearchParams();
    setPositiveIntParam(search, 'subjectId', params?.subjectId);
    setTrimmedParam(search, 'from', params?.from);
    setTrimmedParam(search, 'to', params?.to);
    const qs = search.toString();
    return get<ClassSessionDTO[]>(`${base}/teachers/${normalizedTeacherId}/classes${qs ? `?${qs}` : ''}`);
  },
  updateTeacherSubjects: (teacherId: number, payload: TeacherSubjectsUpdate) =>
    put<TeacherDTO>(`${base}/teachers/${requirePositiveInteger(teacherId, 'teacherId')}/subjects`, payload),
  listClassSessions: (params?: { subjectId?: number; teacherId?: number; studentId?: number; from?: string; to?: string; status?: string }) => {
    const search = new URLSearchParams();
    setPositiveIntParam(search, 'subjectId', params?.subjectId);
    setPositiveIntParam(search, 'teacherId', params?.teacherId);
    setPositiveIntParam(search, 'studentId', params?.studentId);
    setTrimmedParam(search, 'from', params?.from);
    setTrimmedParam(search, 'to', params?.to);
    setTrimmedParam(search, 'status', params?.status);
    const qs = search.toString();
    return get<ClassSessionDTO[]>(`${base}/class-sessions${qs ? `?${qs}` : ''}`);
  },
  createClassSession: (payload: ClassSessionCreate) =>
    post<ClassSessionOut>(`${base}/class-sessions`, {
      ...payload,
      studentId: requirePositiveInteger(payload.studentId, 'studentId'),
      teacherId: requirePositiveInteger(payload.teacherId, 'teacherId'),
      subjectId: requirePositiveInteger(payload.subjectId, 'subjectId'),
      roomId: requirePositiveInteger(payload.roomId, 'roomId'),
    }),
  updateClassSession: (classId: number, payload: ClassSessionUpdate) =>
    patch<ClassSessionDTO>(`${base}/class-sessions/${requirePositiveInteger(classId, 'classId')}`, payload),
  attendClassSession: (classId: number, payload: ClassSessionAttend) =>
    post<ClassSessionOut>(`${base}/class-sessions/${requirePositiveInteger(classId, 'classId')}/attend`, payload),
  listStudents: () => get<StudentDTO[]>(`${base}/students`),
  createStudent: (payload: StudentCreate) => post<StudentDTO>(`${base}/students`, payload),
  updateStudent: (studentId: number, payload: StudentUpdate) =>
    patch<StudentDTO>(`${base}/students/${requirePositiveInteger(studentId, 'studentId')}`, payload),
  listTeacherStudents: (teacherId: number) =>
    get<StudentDTO[]>(`${base}/teachers/${requirePositiveInteger(teacherId, 'teacherId')}/students`),
  addTeacherStudent: (teacherId: number, payload: TeacherStudentLinkIn) =>
    post<void>(`${base}/teachers/${requirePositiveInteger(teacherId, 'teacherId')}/students`, {
      studentId: requirePositiveInteger(payload.studentId, 'studentId'),
    }),
  removeTeacherStudent: (teacherId: number, studentId: number) =>
    del<void>(
      `${base}/teachers/${requirePositiveInteger(teacherId, 'teacherId')}/students/${requirePositiveInteger(studentId, 'studentId')}`,
    ),
  listAvailabilitySlots: (params?: { subjectId?: number; from?: string; to?: string }) => {
    const search = new URLSearchParams();
    setPositiveIntParam(search, 'subjectId', params?.subjectId);
    setTrimmedParam(search, 'from', params?.from);
    setTrimmedParam(search, 'to', params?.to);
    const qs = search.toString();
    return get<TrialAvailabilitySlotDTO[]>(`${base}/trial-availability/slots${qs ? `?${qs}` : ''}`);
  },
  upsertAvailabilitySlot: (payload: TrialAvailabilityUpsert) =>
    post<TrialAvailabilitySlotDTO>(`${base}/trial-availability`, {
      ...payload,
      subjectId: requirePositiveInteger(payload.subjectId, 'subjectId'),
      teacherId: payload.teacherId == null ? payload.teacherId : requirePositiveInteger(payload.teacherId, 'teacherId'),
    }),
  deleteAvailabilitySlot: (availabilityId: number) =>
    del<void>(`${base}/trial-availability/${requirePositiveInteger(availabilityId, 'availabilityId')}`),
};
