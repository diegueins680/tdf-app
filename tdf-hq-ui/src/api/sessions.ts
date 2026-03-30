import { get } from './client';

export interface Page<T> {
  items: T[];
  page: number;
  pageSize: number;
  total: number;
}

export interface SessionDTO {
  sessionId: string;
  sStartAt: string;
  sEndAt: string;
  sStatus: string;
  sBookingRef?: string | null;
  sBandId?: string | null;
  sClientPartyRef?: string | null;
  sService: string;
  sEngineerRef: string;
  sAssistantRef?: string | null;
  sRoomIds: string[];
  sSampleRate?: number | null;
  sBitDepth?: number | null;
  sDaw?: string | null;
  sSessionFolderDriveId?: string | null;
  sNotes?: string | null;
}

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

export const Sessions = {
  list: (params?: { page?: number; pageSize?: number }) => {
    const query = new URLSearchParams();
    if (params?.page != null) query.set('page', String(requirePositiveInteger(params.page, 'page')));
    if (params?.pageSize != null) query.set('pageSize', String(requirePositiveInteger(params.pageSize, 'pageSize')));
    const suffix = query.toString();
    return get<Page<SessionDTO>>(`/sessions${suffix ? `?${suffix}` : ''}`);
  },
};
